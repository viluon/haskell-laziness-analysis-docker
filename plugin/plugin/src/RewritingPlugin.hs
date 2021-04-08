{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables, LambdaCase, TemplateHaskell #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Wunused-binds -Wunused-matches #-}
{- HLINT ignore "Use record patterns" -}

module RewritingPlugin (plugin) where

import System.IO.Unsafe (unsafePerformIO)

-- ghc
import qualified GhcPlugins as GHC

import qualified CoreUtils
import qualified TcRnMonad  as GHC
import qualified TcHsSyn    as GHC
import qualified TcType     as GHC
import qualified TcEvidence as GHC
import qualified TcSMonad   as GHC hiding (tcLookupId, getTopEnv)
import qualified TcSimplify as GHC
import qualified TyCoRep    as GHC
import qualified GHC.ThToHs as GHC
import qualified RnExpr     as GHC
import qualified TcExpr     as GHC
import qualified Desugar    as GHC

import qualified GHC

-- ghc-heap-view
import qualified GHC.HeapView as GHC

-- time
import Data.Time.Clock.System (getSystemTime, SystemTime(..))

-- syb
import Data.Generics (everywhereM, listify, mkM, GenericM, everywhere', gmapM)
import Data.Generics.Basics (Typeable)
import Data.Generics.Aliases (extM)

-- template-haskell
import qualified Language.Haskell.TH as TH
-- debug
import Debug.Trace (trace)
-- mtl & transformers
import Control.Monad.State.Class ()
import Control.Monad.Trans.State.Strict (StateT(runStateT), get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))

plugin :: GHC.Plugin
plugin =  GHC.defaultPlugin { GHC.typeCheckResultAction = const install {- this is to get rid of CLI options -}
                            }

install :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
install _ env | trace "●●● rewriting-plugin entry ●●●" True = do
  new_binds <- rewrite $ GHC.tcg_binds env
  GHC.liftIO . putStrLn . GHC.showSDocUnsafe . GHC.ppr $ new_binds
  return env {GHC.tcg_binds = new_binds}
install _ _ = undefined


type Bind  = GHC.HsBindLR GHC.GhcTc GHC.GhcTc

type RHS        = GHC.GRHS       GHC.GhcTc LExpr
type RHSs       = GHC.GRHSs      GHC.GhcTc LExpr
type Match      = GHC.Match      GHC.GhcTc LExpr
type MatchGroup = GHC.MatchGroup GHC.GhcTc LExpr
type LExpr      = GHC.LHsExpr    GHC.GhcTc
type Expr       = GHC.HsExpr     GHC.GhcTc

entryExpr :: TH.ExpQ
entryExpr = [| trace "rewriting-plugin says hello!" True |]

-- | Check that an expression has the expected type.
-- By Matthew Pickering as shown in plugin-constraint.
typecheckExpr :: GHC.Type -> GHC.LHsExpr GHC.GhcRn -> GHC.TcM (GHC.LHsExpr GHC.GhcTc)
typecheckExpr t e = do
  -- Typecheck the expression and capture generated constraints
  (unwrapped_expr, wanteds) <- GHC.captureConstraints (GHC.tcMonoExpr e (GHC.Check t))
  -- Create the wrapper
  wrapper <- GHC.mkWpLet . GHC.EvBinds . GHC.evBindMapBinds . snd
              <$> GHC.runTcS ( GHC.solveWanteds wanteds )
  -- Apply the wrapper
  let final_expr = GHC.mkLHsWrap wrapper unwrapped_expr
  -- Zonk to instantiate type variables
  GHC.zonkTopLExpr final_expr

-- | Given a type-checked expression, pair the expression up with its Type.
toTyped :: LExpr -> GHC.TcM (Maybe (LExpr, GHC.Type))
toTyped e = do
  hs_env <- GHC.getTopEnv
  (_, mbe) <- GHC.liftIO $ GHC.deSugarExpr hs_env e

  return $ (\x -> (e, CoreUtils.exprType x)) <$> mbe

-- utility functions
indentWithRangle = unlines . fmap ("> " ++) . lines
notInUse = error "supposedly not in use after typechecking"
unsupportedExtensionOf = error . ("unsupported extension of " ++)


-- | Parse, rename, and typecheck a TH quoted expression.
tc :: GHC.Type -> TH.ExpQ -> GHC.TcM LExpr
tc ty expr = do
  Right expr_ps <- fmap (GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan)
    $ GHC.liftIO
    $ TH.runQ expr
  -- rename the TH source
  (expr_rn, _ ) <- GHC.rnLExpr expr_ps
  -- typecheck
  typecheckExpr ty expr_rn

data WrapperState = WS { ws_fun :: Maybe GHC.Name -- ^ the function we're inside of
                       }

-- type GenericM m = forall a. Data a => a -> m a

-- like everywhereM, but top-down
everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
  where
    go :: GenericM m
    go x = do
      x' <- f x
      gmapM go x'

occNameStr :: GHC.HasOccName a => a -> String
occNameStr = show . GHC.occNameFS . GHC.occName

closureType c = case c of
  GHC.ConstrClosure    {} -> "constr"
  GHC.FunClosure       {} -> "fun"
  GHC.ThunkClosure     {} -> "thunk"
  GHC.SelectorClosure  {} -> "selector"
  GHC.PAPClosure       {} -> "pap"
  GHC.APClosure        {} -> "ap"
  GHC.IndClosure       {} -> "ind"
  GHC.BCOClosure       {} -> "bco"
  GHC.BlackholeClosure {} -> "blackhole"
  GHC.ArrWordsClosure  {} -> "bytearray#"
  _                       -> "!unknown"

traceArg :: String -> String -> a -> a
traceArg funName arg x = unsafePerformIO $ do
  let x' = GHC.asBox x
  closure <- GHC.getBoxedClosureData x'
  MkSystemTime {systemNanoseconds = time} <- getSystemTime

  appendFile "/tmp/trace.csv" $ (++ "\n") $ concatMap (++ ",") [show time, funName, arg, closureType closure]
  return x

-- TODO: use optics
rewrite :: GHC.LHsBinds GHC.GhcTc -> GHC.TcM (GHC.LHsBinds GHC.GhcTc)
rewrite binds = do
  entry <- tc GHC.boolTy entryExpr
  let ep_guard = GHC.BodyStmt (error "element type of the rhs") entry (error "should be ?") (error "should be noSyntaxExpr")
  rewriteIdRefs

  where
  -- goal: rewrite argument references to go through trace
  -- plan:
  --   - [x] capture all bindings
  --   - [x] rewrite refs
  --   - [x] propagate scope (! misleading, we care about the surrounding function name) to ref sites
  --     - how do we do this?
  --   - [x] use ghc-heap-view to peek inside the arguments
  --   - [ ] add a global mutable map
  --   - [ ] count function calls via the map

  isMatch :: Match -> Bool
  isMatch GHC.Match {} = True
  isMatch _ = False

  boundVars = do
    match <- listify isMatch binds
    lpat <- GHC.m_pats match
    lextract lpat
    where
      unl x = GHC.unLoc x

      lextract :: GHC.LPat GHC.GhcTc -> [GHC.Id]
      lextract x = extractIds . unl $ x

      extractIds :: GHC.Pat GHC.GhcTc -> [GHC.Id]
      extractIds  GHC.WildPat   {}                       = []
      extractIds (GHC.VarPat   _ lid)                    = [unl lid]
      extractIds (GHC.LazyPat  _ lpat)                   = lextract lpat
      extractIds (GHC.AsPat    _ lid lpat)               = unl lid : lextract lpat
      extractIds (GHC.ParPat   _ lpat)                   = lextract lpat
      extractIds (GHC.BangPat  _ lpat)                   = lextract lpat
      extractIds (GHC.ListPat  _ lpats)                  = do lpat <- lpats; lextract lpat
      extractIds (GHC.TuplePat _ lpats _)                = do lpat <- lpats; lextract lpat
      extractIds (GHC.SumPat   _ lpat _ _)               = lextract lpat
      extractIds (GHC.ConPatIn _ details)                = error "todo"
      -- HsConDetails (LPat GhcTc) (HsRecFields GhcTc (LPat GhcTc))
      extractIds  GHC.ConPatOut {GHC.pat_args = details} = case details of
                                                            GHC.InfixCon  lpatl lpatr -> lextract lpatl ++ lextract lpatr
                                                            GHC.PrefixCon lpats       -> concatMap lextract lpats
                                                            GHC.RecCon    r           -> error "todo"

      extractIds (GHC.ViewPat   _ _ lpat)                = lextract lpat
      extractIds  GHC.SplicePat {}                       = error "todo"
      extractIds  GHC.LitPat    {}                       = []
      extractIds  GHC.NPat      {}                       = []
      extractIds  GHC.NPlusKPat {}                       = error "todo"
      extractIds (GHC.SigPat   _ lpat _)                 = lextract lpat
      extractIds (GHC.CoPat    _ _ pat _)                = extractIds pat
      extractIds  GHC.XPat      {}                       = unsupportedExtensionOf "Pat"

  rewriteIdRefs :: GHC.TcM (GHC.LHsBinds GHC.GhcTc)
  rewriteIdRefs = do
    (binds, _) <- (`runStateT` WS {ws_fun = Nothing}) $ everywhereM' trans binds
    return binds

    where
      -- the monadic transformation capturing function info and wrapping argument references
      trans :: Typeable a => a -> StateT WrapperState GHC.TcM a
      trans = mkM collectFunInfo `extM` wrapRef

  collectFunInfo :: Bind -> StateT WrapperState GHC.TcM Bind
  collectFunInfo bind = let name = case bind of
                                    GHC.FunBind  {GHC.fun_id = lid} -> Right $ GHC.varName . GHC.unLoc $ lid
                                    GHC.PatBind  {}                 -> Left  "pat bind"
                                    GHC.VarBind  {GHC.var_id = vid} -> Right $ GHC.varName vid
                                    GHC.AbsBinds {}                 -> Left  "abs bind"
                                    _                               -> Left  . ("unsupported Bind extension:\n" ++) . GHC.showSDocUnsafe . GHC.ppr $ bind
                        in case trace "------ collectFunInfo! " name of
                            Right !nm -> trace (indentWithRangle . show . GHC.occNameFS . GHC.occName $ nm)
                                          put WS {ws_fun = Just nm} >>= (const . return $ bind)
                            Left  msg -> trace ("  (" ++ msg ++ ")") $ return bind

  wrapRef :: LExpr -> StateT WrapperState GHC.TcM LExpr
  wrapRef (GHC.L span v@(GHC.HsVar x lid))
    -- make sure the Id is bound by some pattern and it's not a part of an already rewritten expression
    | GHC.unLoc lid `elem` boundVars && span /= GHC.noSrcSpan = do
      WS {ws_fun = Just funName} <- get

      let argName = occNameStr . GHC.varName . GHC.unLoc $ lid
      let varWithoutSrcSpan = GHC.L GHC.noSrcSpan v
      let funName' = occNameStr funName

      Right expr_ps <- fmap (GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan)
        $ GHC.liftIO
        $ TH.runQ [| traceArg funName' argName |]
      -- rename the TH source
      (expr_rn, _ ) <- lift $ GHC.rnLExpr expr_ps

      -- obtain the type of the expression we're wrapping
      Just (_, org_ty) <- lift $ toTyped varWithoutSrcSpan

      let ty = foldr1 (GHC.mkFunTy GHC.VisArg) [org_ty, org_ty]

      -- Typecheck the new expression and capture generated constraints
      (unwrapped_expr, wanteds) <- lift $
        GHC.captureConstraints (GHC.tcMonoExpr expr_rn (GHC.Check (trace (GHC.showSDocUnsafe (GHC.ppr ty)) ty)))
      -- Create the wrapper
      wrapper <- lift $ GHC.mkWpLet . GHC.EvBinds . GHC.evBindMapBinds . snd
                  <$> GHC.runTcS (GHC.solveWanteds wanteds)
      -- Apply the wrapper
      let final_expr = GHC.mkHsApp (GHC.mkLHsWrap wrapper unwrapped_expr) varWithoutSrcSpan
      -- Zonk to instantiate type variables
      lift $ GHC.zonkTopLExpr (trace "------ wrapRef!" final_expr)

  wrapRef e = return e

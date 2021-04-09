{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables, LambdaCase, TemplateHaskell #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Wunused-binds -Wunused-matches #-}
{- HLINT ignore "Use record patterns" -}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Bag        as GHC

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
-- containers
import qualified Data.HashSet as S

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

data WrapperState = WS { ws_fun         :: Maybe GHC.Name     -- ^ the function we're inside of
                       , ws_callCounter :: Maybe GHC.Id       -- ^ the reference to the Int denoting the number of the current function call
                       , ws_processed   :: !(S.HashSet LExpr) -- ^ the set of already processed expressions
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

data TraceSort = ArgTrace | EntryTrace deriving (Eq, Show)

logt :: TraceSort -> [String] -> IO ()
logt sort params = do
  MkSystemTime {systemNanoseconds = time} <- getSystemTime
  appendFile "/tmp/trace.csv"
    . (++ "\n")
    . concatMap (++ ",")
    . ([show time, show sort] ++)
    $ params

traceEntry :: a -> a
traceEntry = id

traceArg :: String -> String -> Int -> a -> a
traceArg funName arg callNumber x = unsafePerformIO $ do
  let x' = GHC.asBox x
  closure <- GHC.getBoxedClosureData x'

  logt ArgTrace
    [ funName
    , show callNumber
    , arg
    , closureType closure
    ]
  return x

isMatch :: Match -> Bool
isMatch GHC.Match {} = True
isMatch _ = False

collectFunInfo :: Bind -> StateT WrapperState GHC.TcM Bind
collectFunInfo bind =
  let name = case bind of
        GHC.FunBind  {} -> Right $ GHC.varName . GHC.unLoc . GHC.fun_id $ bind
        GHC.PatBind  {} -> Left  "pat bind"
        GHC.VarBind  {} -> Right $ GHC.varName . GHC.var_id $ bind
        GHC.AbsBinds {} -> Left  "abs bind"
        _               -> Left  . ("unsupported Bind extension:\n" ++) . GHC.showSDocUnsafe . GHC.ppr $ bind
  in case trace "------ collectFunInfo! " name of
    Right !nm -> trace (indentWithRangle . show . GHC.occNameFS . GHC.occName $ nm) $
                  do
                    state <- get
                    put state {ws_fun = Just nm}
                    return bind
    Left  msg -> trace ("  (" ++ msg ++ ")") $ return bind

incrementCallCounter :: LExpr -> StateT WrapperState GHC.TcM LExpr
incrementCallCounter lexpr@(GHC.L span expr)
  | {- TODO: come up with a more robust solution -} span /= GHC.noSrcSpan = do
  counterName <- lift $ GHC.newName $ GHC.mkOccName GHC.dataName "call-counter"
  let counterId = GHC.mkLocalId counterName GHC.intTy

  -- let matchGroup :: MatchGroup = GHC.MG
  --       { GHC.mg_ext    = GHC.MatchGroupTc
  --                         { GHC.mg_arg_tys = error "to-do" -- :: [Type]  Types of the arguments, t1..tn
  --                         , GHC.mg_res_ty  = error "to-do" -- :: Type    Type of the result, tr
  --                         }
  --       , GHC.mg_alts   = error "to-do" -- :: Located [LMatch GhcTc LExpr]
  --       , GHC.mg_origin = GHC.Generated
  --       }

  let nef = GHC.NoExtField

  let matchGroup = GHC.MG
        { GHC.mg_ext    = GHC.NoExtField
        , GHC.mg_alts   = error "to-do"
        , GHC.mg_origin = GHC.Generated
        }

  let actualBind :: GHC.HsBindLR GHC.GhcRn GHC.GhcRn = GHC.FunBind
        { GHC.fun_ext     = GHC.emptyNameSet -- locally-bound free variables of this definition
        , GHC.fun_id      = error "to-do"
        , GHC.fun_matches = matchGroup
        , GHC.fun_co_fn   = GHC.WpHole       -- identity coercion
        , GHC.fun_tick    = []               -- ticks to put on the rhs
        }

  let binds = GHC.HsValBinds nef (GHC.ValBinds nef (GHC.unitBag (GHC.L GHC.noSrcSpan actualBind)) [])
  let letExpr = GHC.HsLet nef (GHC.L GHC.noSrcSpan binds) lexpr
  -- TODO update state
  return $ GHC.L GHC.noSrcSpan letExpr

incrementCallCounter x = return x

-- goal: rewrite argument references to go through trace
-- plan:
--   - [x] capture all bindings
--   - [x] rewrite refs
--   - [x] propagate scope (! misleading, we care about the surrounding function name) to ref sites
--     - how do we do this?
--   - [x] use ghc-heap-view to peek inside the arguments
--   - [ ] add a global mutable map
--   - [ ] count function calls via the map

rewrite :: GHC.LHsBinds GHC.GhcTc -> GHC.TcM (GHC.LHsBinds GHC.GhcTc)
rewrite binds = fst <$> (`runStateT` initialState) (everywhereM' trans binds)
  where

  initialState = WS {ws_fun = Nothing, ws_callCounter = Nothing, ws_processed = S.empty}

  -- the monadic transformation capturing function info, introducing call number variables, and wrapping argument references
  trans :: Typeable a => a -> StateT WrapperState GHC.TcM a
  trans = mkM collectFunInfo `extM` wrapRef `extM` incrementCallCounter

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

  wrapRef :: LExpr -> StateT WrapperState GHC.TcM LExpr
  wrapRef (GHC.L span v@(GHC.HsVar x lid))
    -- make sure the Id is bound by some pattern and it's not a part of an already rewritten expression
    | GHC.unLoc lid `elem` boundVars && span /= GHC.noSrcSpan = do
      WS { ws_fun = Just funName
         , ws_callCounter = Just callCounter
         } <- get

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
        GHC.captureConstraints (GHC.tcMonoExpr expr_rn (GHC.Check ty))
      -- Create the wrapper
      wrapper <- lift $ GHC.mkWpLet . GHC.EvBinds . GHC.evBindMapBinds . snd
                  <$> GHC.runTcS (GHC.solveWanteds wanteds)
      -- Apply the wrapper
      -- GHC.mkHsLam (pats :: [LPat]) (body :: LHsExpr)
      -- GHC.mkHsWrapPat HsWrapper Pat (GhcPass id) Type

      let counter :: LExpr = GHC.L GHC.noSrcSpan $ GHC.HsVar GHC.NoExtField $ GHC.L GHC.noSrcSpan callCounter

      let final_expr = GHC.mkHsApp
            (GHC.mkHsApp (GHC.mkLHsWrap wrapper unwrapped_expr) varWithoutSrcSpan) counter
      -- Zonk to instantiate type variables
      lift $ GHC.zonkTopLExpr (trace "------ wrapRef!" final_expr)

  wrapRef e = return e

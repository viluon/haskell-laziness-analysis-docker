{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{- HLINT ignore "Use record patterns" -}

module RewritingPlugin (plugin) where

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

-- syb
import Data.Generics (everywhereM, listify, mkM)
-- template-haskell
import qualified Language.Haskell.TH as TH
-- debug
import Debug.Trace (trace)
-- transformers
import Control.Monad.Trans.State.Strict

plugin :: GHC.Plugin
plugin =  GHC.defaultPlugin { GHC.typeCheckResultAction = const install {- this is to get rid of CLI options -}
                            }

{-
  Plan:
  ====
  - create custom tracing functions trace_n where n is the number of arguments
  - apply trace_n to the arguments of the surrounding function at every exit point
  - profit?

  the question then is whether to keep trace_n in the plugin's source or conjure it up
  in every module separately
  ... and how to do either.
-}

install :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
install _ env | trace "●●● rewriting-plugin entry ●●●" True = do
  new_binds <- rewrite $ GHC.tcg_binds env
  GHC.liftIO . putStrLn . GHC.showSDocUnsafe . GHC.ppr $ new_binds
  return env {GHC.tcg_binds = new_binds}


type Bind  = GHC.HsBindLR GHC.GhcTc GHC.GhcTc

type RHS        = GHC.GRHS       GHC.GhcTc LExpr
type RHSs       = GHC.GRHSs      GHC.GhcTc LExpr
type Match      = GHC.Match      GHC.GhcTc LExpr
type MatchGroup = GHC.MatchGroup GHC.GhcTc LExpr
type LExpr      = GHC.LHsExpr    GHC.GhcTc
type Expr       = GHC.HsExpr     GHC.GhcTc

entryExpr :: TH.ExpQ
entryExpr = [| trace "rewriting-plugin says hello!" True |]

exitExpr :: TH.ExpQ
exitExpr = [| trace "exit point" |]

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

-- TODO: use optics
rewrite :: GHC.LHsBinds GHC.GhcTc -> GHC.TcM (GHC.LHsBinds GHC.GhcTc)
rewrite binds = do
  entry <- tc GHC.boolTy entryExpr
  let ep_guard = GHC.BodyStmt (error "element type of the rhs") entry (error "should be ?") (error "should be noSyntaxExpr")
  rewriteIdRefs

  where
  -- goal: rewrite argument references to go through trace
  -- plan:
  --   - capture all bindings
  --   - rewrite refs
  --   - add a global mutable map
  --   - count function calls via the map

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
  rewriteIdRefs = everywhereM (mkM wrapRef) binds

  wrapRef :: LExpr -> GHC.TcM LExpr
  wrapRef v@(GHC.L _ (GHC.HsVar x lid)) | GHC.unLoc lid `elem` boundVars = do
    Right expr_ps <- fmap (GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan)
      $ GHC.liftIO
      $ TH.runQ [| trace "hello world" |]
    -- rename the TH source
    (expr_rn, _ ) <- GHC.rnLExpr expr_ps

    -- obtain the type of the expression we're wrapping
    Just (_, org_ty) <- toTyped v

    let ty = foldr1 (GHC.mkFunTy GHC.VisArg) [org_ty, org_ty]

    -- Typecheck the new expression and capture generated constraints
    (unwrapped_expr, wanteds) <-
      GHC.captureConstraints (GHC.tcMonoExpr expr_rn (GHC.Check (trace (GHC.showSDocUnsafe (GHC.ppr ty)) ty)))
    -- Create the wrapper
    wrapper <- GHC.mkWpLet . GHC.EvBinds . GHC.evBindMapBinds . snd
                <$> GHC.runTcS ( GHC.solveWanteds wanteds )
    -- Apply the wrapper
    let final_expr = GHC.mkHsApp (GHC.mkLHsWrap wrapper unwrapped_expr) v
    -- Zonk to instantiate type variables
    GHC.zonkTopLExpr final_expr

  wrapRef e = return e

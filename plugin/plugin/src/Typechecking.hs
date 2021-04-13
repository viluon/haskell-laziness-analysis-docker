module Typechecking (
  -- types
  Bind, RHS, LExpr, GenLocSpan, Match,

  -- utilities
  (~>), tc, toTyped, occNameStr,
) where

-- ghc
import qualified CoreUtils (exprType)
import qualified Desugar    as GHC
import qualified GhcPlugins as GHC
import qualified GHC.ThToHs as GHC
import qualified TcEvidence as GHC
import qualified TcSimplify as GHC
import qualified TcRnMonad  as GHC
import qualified TcSMonad   as GHC (runTcS)
import qualified TcHsSyn    as GHC (zonkTopLExpr)
import qualified TcType     as GHC
import qualified TcExpr     as GHC
import qualified RnExpr     as GHC


import qualified GHC

-- template-haskell
import qualified Language.Haskell.TH as TH


type Bind  = GHC.HsBindLR GHC.GhcTc GHC.GhcTc

type RHS        = GHC.GRHS       GHC.GhcTc LExpr
type LExpr      = GHC.LHsExpr    GHC.GhcTc
type GenLocSpan = GHC.GenLocated GHC.SrcSpan
type RHSs       = GHC.GRHSs      GHC.GhcTc LExpr
type Match      = GHC.Match      GHC.GhcTc LExpr
type MatchGroup = GHC.MatchGroup GHC.GhcTc LExpr
type Expr       = GHC.HsExpr     GHC.GhcTc

-- | Check that an expression has the expected type.
-- By Matthew Pickering as shown in plugin-constraint.
typecheckExpr :: GHC.Type -> GHC.LHsExpr GHC.GhcRn -> GHC.TcM (GHC.LHsExpr GHC.GhcTc)
typecheckExpr t e = do
  -- Typecheck the expression and capture generated constraints
  (unwrapped_expr, wanteds) <- GHC.captureConstraints (GHC.tcMonoExpr e (GHC.Check t))
  -- Create the wrapper
  wrapper <- GHC.mkWpLet . GHC.EvBinds . GHC.evBindMapBinds . snd
              <$> GHC.runTcS (GHC.solveWanteds wanteds)
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

occNameStr :: GHC.HasOccName a => a -> String
occNameStr = read . show . GHC.occNameFS . GHC.occName

infixr 6 ~>
(~>) :: GHC.Type -> GHC.Type -> GHC.Type
(~>) = GHC.mkVisFunTy

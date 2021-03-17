{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{- HLINT ignore "Use record patterns" -}

module RewritingPlugin (plugin) where

-- ghc
import qualified GhcPlugins as GHC

import qualified TcRnMonad  as GHC
import qualified TcHsSyn    as GHC
import qualified TcType     as GHC
import qualified TcEvidence as GHC
import qualified TcSMonad   as GHC hiding (tcLookupId)
import qualified TcSimplify as GHC
import qualified GHC.ThToHs as GHC
import qualified RnExpr     as GHC
import qualified TcExpr     as GHC

import qualified GHC

-- syb
import Data.Generics (everywhereM, listify, mkM)
-- template-haskell
import qualified Language.Haskell.TH as TH
-- debug
import Debug.Trace (trace)

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


indentWithRangle = unlines . fmap ("> " ++) . lines

-- | Parse, rename, and typecheck a TH quoted expression.
tc :: TH.ExpQ -> GHC.TcM LExpr
tc expr = do
  Right expr_ps <- fmap (GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan)
    $ GHC.liftIO
    $ TH.runQ expr
  -- rename the TH source
  (expr_rn, _ ) <- GHC.rnLExpr expr_ps
  -- typecheck
  typecheckExpr GHC.boolTy expr_rn

-- TODO: use optics
rewrite :: GHC.LHsBinds GHC.GhcTc -> GHC.TcM (GHC.LHsBinds GHC.GhcTc)
rewrite binds = do
  entry <- tc entryExpr
  exit  <- tc  exitExpr
  let ep_guard = GHC.BodyStmt (error "element type of the rhs") entry (error "should be ?") (error "should be noSyntaxExpr")
  return $ go (GHC.L GHC.noSrcSpan ep_guard) binds

  where
  go entryPointGuard = fmap . fmap $ rw rewriteExitPoints

    where
    rw :: (Expr -> Expr) -> Bind -> Bind
    rw f bind@ GHC.FunBind {GHC.fun_matches = mg} | trace "FunBind" True =
      bind {GHC.fun_matches = rewriteMatchGroup f mg}
    rw f bind@ GHC.PatBind {GHC.pat_rhs = rhss} | trace "PatBind" True =
      bind {GHC.pat_rhs = rw'' f rhss}
    rw f bind@ GHC.AbsBinds {GHC.abs_binds = binds} =
      bind {GHC.abs_binds = (fmap . fmap $ rw f) binds}
    rw f x | trace ("Bind else:\n" ++ (indentWithRangle . GHC.showSDocUnsafe . GHC.ppr $ x)) True =
      trace ("tru bind:" ++ name) x
      where name = case x of
                    GHC.XHsBindsLR {} -> "XHsBindsLR"
                    GHC.PatSynBind {} -> "PatSynBind"
                    GHC.VarBind    {} -> "VarBind"

    rw' :: (Expr -> Expr) -> Match -> Match
    rw' f m@ GHC.Match {GHC.m_grhss = rhs} | trace "Match" True =
      m {GHC.m_grhss = rw'' f rhs}
    rw' f x = x

    rw'' :: (Expr -> Expr) -> RHSs -> RHSs
    rw'' f rhss@ GHC.GRHSs {GHC.grhssGRHSs = guarded} | trace "RHSs" True =
      rhss {GHC.grhssGRHSs = (fmap . fmap) (rw''' f) guarded}
    rw'' f x = x

    rw''' :: (Expr -> Expr) -> RHS -> RHS
    rw''' f (GHC.GRHS x guards body) | trace "RHS" True =
      GHC.GRHS x (entryPointGuard : guards) (f <$> body)
    rw''' f x = x

    rewriteMatchGroup :: (Expr -> Expr) -> MatchGroup -> MatchGroup
    rewriteMatchGroup f mg@GHC.MG {GHC.mg_alts = alts} = mg {GHC.mg_alts = (fmap . fmap . fmap) (rw' f) alts}
    rewriteMatchGroup _ _                              = unsupportedExtensionOf "MatchGroup"

    notInUse = error "supposedly not in use after typechecking"
    unsupportedExtensionOf = error . ("unsupported extension of " ++)

    rewriteExitPoints :: Expr -> Expr
    rewriteExitPoints e =
      case e of
        GHC.HsVar           {}              -> wrapExitPoint e
        GHC.HsUnboundVar    {}              -> e -- will not compile with holes
        (GHC.HsConLikeOut   _ _)            -> error "todo" -- not sure about this one
        GHC.HsRecFld        {}              -> wrapExitPoint e
        GHC.HsOverLabel     {}              -> notInUse
        GHC.HsIPVar         {}              -> notInUse
        GHC.HsOverLit       {}              -> wrapExitPoint e
        GHC.HsLit           {}              -> wrapExitPoint e
        (GHC.HsLam          x mg)           -> GHC.HsLam     x $ rewriteMatchGroup rewriteExitPoints mg
        (GHC.HsLamCase      x mg)           -> GHC.HsLamCase x $ rewriteMatchGroup rewriteExitPoints mg
        (GHC.HsApp          x f a)          -> GHC.HsApp     x f $ rewriteExitPoints <$> a
        (GHC.HsAppType      x e wc)         -> GHC.HsAppType x (rewriteExitPoints <$> e) wc -- not sure about this one either
        (GHC.OpApp          x l o r)        -> GHC.OpApp     x l o $ rewriteExitPoints <$> r
        (GHC.NegApp         x e n)          -> GHC.NegApp    x (rewriteExitPoints <$> e) n
        (GHC.HsPar          x e)            -> GHC.HsPar     x $ rewriteExitPoints <$> e
        (GHC.SectionL       x e o)          -> GHC.SectionL  x (rewriteExitPoints <$> e) o
        (GHC.SectionR       x o e)          -> GHC.SectionR  x o $ rewriteExitPoints <$> e
        (GHC.ExplicitTuple  _ _ _)          -> e
        (GHC.ExplicitSum    _ _ _ _)        -> e
        (GHC.HsCase         x e mg)         -> GHC.HsCase x e (rewriteMatchGroup rewriteExitPoints mg)
        (GHC.HsIf           x cond p th el) -> GHC.HsIf x cond p (rewriteExitPoints <$> th) (rewriteExitPoints <$> el)
        (GHC.HsMultiIf      x rhss)         -> GHC.HsMultiIf x ((fmap . fmap . fmap) rw'' $ rhss)
        (GHC.HsLet          x b e)          -> GHC.HsLet x b $ rewriteExitPoints <$> e
        (GHC.HsDo           x n stmts)      -> error "todo, involves statements"
        (GHC.ExplicitList   x _ _)          -> error "todo"
          -- how do we actually deal with laziness? push traces to the leaves? the trouble is that the list is lazy,
          -- so until (some of) its elements are evaluated, we have no clue as to whether the arguments in scope were
          -- required or not. There also is no order to these evaluations.
        (GHC.RecordCon      _ _ _)          -> e
        (GHC.RecordUpd      _ _ _)          -> e
        (GHC.ExprWithTySig  _ _ _)          -> e
        (GHC.ArithSeq       _ _ _)          -> e
        (GHC.HsSCC          _ _ _ _)        -> e
        (GHC.HsCoreAnn      _ _ _ _)        -> e
        (GHC.HsBracket      _ _)            -> e
        (GHC.HsRnBracketOut _ _ _)          -> e
        (GHC.HsTcBracketOut _ _ _)          -> e
        (GHC.HsSpliceE      _ _)            -> e
        (GHC.HsProc         _ _ _)          -> e
        (GHC.HsStatic       _ _)            -> e
        (GHC.HsTick         _ _ _)          -> e
        (GHC.HsBinTick      _ _ _ _)        -> e
        (GHC.HsTickPragma   _ _ _ _ _)      -> e
        (GHC.HsWrap         x w e)          -> GHC.HsWrap x w $ rewriteExitPoints e -- TC output only
        (GHC.XExpr          _)              -> e

    wrapExitPoint :: Expr -> Expr
    wrapExitPoint = id

-- rewrite :: GHC.LHsBinds GHC.GhcTc -> GHC.TcM (GHC.LHsBinds GHC.GhcTc)
-- rewrite binds = do mkM rw `everywhereM` binds
--   where
--         rw :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM (Expr.LHsExpr GHC.GhcTc)
--         rw (GHC.L srcSpan expr) = case expr of
--           Expr.HsApp xa e1 e2 -> undefined
--         rw x = return x

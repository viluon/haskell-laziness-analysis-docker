{-# LANGUAGE BangPatterns, TemplateHaskell, RankNTypes, LambdaCase, ScopedTypeVariables #-}

module Rewriting (
  rewrite
) where

import Typechecking
import Logging

-- ghc
import qualified TcRnMonad  as GHC
import qualified TcHsSyn    as GHC (zonkTopLExpr)
import qualified TysWiredIn as GHC (intTy)
import qualified Var        as GHC (varName)
import qualified Outputable as GHC (ppr, showSDocUnsafe)
import qualified GHC

-- syb
import Data.Generics.Aliases (extM, extT, mkM, mkQ, mkT, GenericM)
import Data.Generics.Schemes (everywhere, listify, something)
import Data.Generics.Basics  (Data(gmapM), Typeable)
import Data.Generics.Text    (gshow)

-- mtl & transformers
import Control.Monad.Trans.State.Strict (StateT(runStateT), get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.State.Class ()

-- debug
import Debug.Trace (trace)


data WrapperState = WS { ws_fun         :: Maybe GHC.Name     -- ^ the function we're inside of
                       , ws_callCounter :: Maybe GHC.Id       -- ^ the reference to the Int denoting the number of the current function call
                       }

-- like everywhereM, but top-down
everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
  where
    go :: GenericM m
    go x = do
      x' <- f x
      gmapM go x'

indentWithRangle :: String -> String
indentWithRangle = unlines . fmap ("> " ++) . lines

collectFunInfo :: Bind -> StateT WrapperState GHC.TcM Bind
collectFunInfo bind =
  let name = case bind of
        GHC.FunBind  {} -> Right $ GHC.varName . GHC.unLoc . GHC.fun_id $ bind
        GHC.PatBind  {} -> Left  "pat bind"
        GHC.VarBind  {} -> Right $ GHC.varName . GHC.var_id $ bind
        GHC.AbsBinds {} -> Left  "abs bind"
        _               -> Left  . ("unsupported Bind extension:\n" ++) . GHC.showSDocUnsafe . GHC.ppr $ bind
  in case trace "------ collectFunInfo! " name of
    -- FIXME ugliness
    Right !nm | "call_number" /= occNameStr nm ->
      trace (indentWithRangle . occNameStr $ nm) $
        do
          state <- get
          put state {ws_fun = Just nm}
          return bind
    Left msg -> trace ("  (" ++ msg ++ ")") $ return bind
    _        -> trace "hit a call_number"   $ return bind

dummyBinding :: a
dummyBinding = undefined

incrementCallCounter :: RHS -> StateT WrapperState GHC.TcM RHS
incrementCallCounter (GHC.GRHS x guards lexpr@(GHC.L span _))
  | {- TODO: come up with a more robust solution -} span /= GHC.noSrcSpan = do
  WS {ws_fun = Just funName} <- get
  let funName' = occNameStr funName
  -- build the let expression for the call_number variable
  Just (_, org_ty) <- lift $ toTyped lexpr
  letExpr <- lift $ tc org_ty [| let !call_number = traceEntry funName' in dummyBinding |]

  -- extract the call_number Id
  let Just !var = (something $ mkQ Nothing (\case
        x | "call_number" == occNameStr x -> Just x
        _ -> Nothing
        )) letExpr

  -- update state
  !state <- get
  put state {ws_callCounter = Just var}

  -- apply the λ-wrapped let binder to the original expression
  !completeExpr <- lift . GHC.zonkTopLExpr $ everywhere (mkT removeSrcSpans `extT` replaceDummyWith lexpr) letExpr

  GHC.liftIO . putStrLn . GHC.showSDocUnsafe $ GHC.ppr completeExpr
  return $ GHC.GRHS x guards completeExpr

  where
    removeSrcSpans :: GenLocSpan LExpr -> GenLocSpan LExpr
    removeSrcSpans (GHC.L _ x) = GHC.L GHC.noSrcSpan x

    replaceDummyWith :: LExpr -> LExpr -> LExpr
    replaceDummyWith replacement = \case
      (GHC.L _ (GHC.HsWrap _ _ (GHC.HsVar _ (GHC.L _ x))))
        | "dummyBinding" == occNameStr x -> replacement
      x -> x

incrementCallCounter x = return x

isMatch :: Match -> Bool
isMatch GHC.Match {} = True
isMatch _ = False

rewrite :: GHC.LHsBinds GHC.GhcTc -> GHC.TcM (GHC.LHsBinds GHC.GhcTc)
rewrite binds = fst <$> (`runStateT` initialState) (everywhereM' trans binds)
  where

  initialState = WS {ws_fun = Nothing, ws_callCounter = Nothing}

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
      extractIds  GHC.XPat      {}                       = error "unsupported extension of Pat"

  wrapRef :: LExpr -> StateT WrapperState GHC.TcM LExpr
  wrapRef (GHC.L span v@(GHC.HsVar x lid))
    -- make sure the Id is bound by some pattern and it's not a part of an already rewritten expression
    | GHC.unLoc lid `elem` boundVars && span /= GHC.noSrcSpan = do
      WS { ws_fun         = Just funName
         , ws_callCounter = Just callCounter
         } <- get

      let argName = occNameStr . GHC.varName . GHC.unLoc $ lid
      let varWithoutSrcSpan = GHC.L GHC.noSrcSpan v
      let funName' = occNameStr funName

      Just (_, !org_ty) <- lift $ toTyped varWithoutSrcSpan
      !pap <- lift $ tc (GHC.intTy ~> org_ty ~> org_ty) [| traceArg funName' argName |]

      let counter = GHC.L GHC.noSrcSpan $ GHC.HsVar GHC.NoExtField $ GHC.L GHC.noSrcSpan callCounter
      let !final_expr = GHC.mkHsApp (GHC.mkHsApp pap counter) varWithoutSrcSpan

      -- Zonk to instantiate type variables
      lift $ GHC.zonkTopLExpr (trace "------ wrapRef!" final_expr)

  wrapRef e = return e
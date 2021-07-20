{-# LANGUAGE BangPatterns, TemplateHaskell, Rank2Types, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rewriting
( rewrite
) where

import Typechecking
import Logging
import Zipping

-- ghc
import qualified TcRnMonad  as GHC
import qualified TcHsSyn    as GHC (zonkTopLExpr)
import qualified TysWiredIn as GHC (intTy)
import qualified Var        as GHC (varName)
import qualified Outputable as GHC (Outputable, ppr, showSDocUnsafe)
import qualified GHC

-- syb
import Data.Generics.Aliases (extM, extT, mkM, mkQ, mkT, GenericM)
import Data.Generics.Schemes (everywhere, listify, something)
import Data.Generics.Basics  (Data(gmapM), Typeable, cast)
import Data.Generics.Text    (gshow)

-- syz
import Data.Generics.Zipper

-- mtl & transformers
import Control.Monad.Trans.State.Strict (StateT(runStateT), get, put, gets, modify')
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.State.Class ()

-- monad utilities in base
import Control.Monad ( (<=<), (>=>), join, when, unless )

-- utilities in base
import Data.Function ( (&) )
import Data.Maybe (isJust, listToMaybe, fromJust)


data WrapperState = WS { ws_fun          :: Maybe GHC.Name
                         -- ^ the function we're inside of
                       , ws_callCounters :: [GHC.Id]
                         -- ^ the stack of references to the Ints, each denoting
                         --   the number of the current function call. The stack
                         --   follows the nesting of λ-abstractions.
                       , ws_incrCCSuccess :: Bool
                         -- ^ a horrible ad-hoc thing that I will go to hell for.
                         --   FIXME: Get rid of this ASAP.
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
collectFunInfo bind | trace' "collectFunInfo" True =
  -- the 'flip join' is like a switch statement: we match on bind defining a
  -- function *taking bind* in every case. The case-specific function is applied
  -- immediately. The arms that don't need the bind start with const.
  let name = flip join bind $ \case
        GHC.FunBind  {} -> Right . GHC.varName . GHC.unLoc . GHC.fun_id
        GHC.PatBind  {} -> Left  . const "pat bind"
        GHC.VarBind  {} -> Right . GHC.varName . GHC.var_id
        GHC.AbsBinds {} -> Left  . const "abs bind"
        _               -> Left  . ("unsupported Bind extension:\n" ++) . GHC.showSDocUnsafe . GHC.ppr
  in case name of
    Right !nm | "call_number" /= occNameStr nm ->
      trace (indentWithRangle . occNameStr $ nm) $
        do
          state <- get
          trace'' ("the top-level fun is " ++ pprWithoutNull nm) $ pure ()
          put $! state {ws_fun = Just nm}
          return bind
    Left msg -> trace ("  (" ++ msg ++ ")") $ return bind
    _        ->                               return bind
collectFunInfo _ = error "impossible"

dummyBinding :: a
dummyBinding = undefined

incrementCallCounter :: RHS -> StateT WrapperState GHC.TcM RHS
incrementCallCounter (GHC.GRHS x guards lexpr@(GHC.L span _))
  | {- TODO: come up with a more robust solution -} span /= GHC.noSrcSpan = do
  trace' "incrementCC" $ pure ()
  WS {ws_fun = Just funName, ws_callCounters = counters} <- get
  let funName' = occNameStr funName
  -- build the let expression for the call_number variable
  Just (_, org_ty) <- lift $ toTyped lexpr
  letExpr <- lift $ tc org_ty [|
    let call_number = traceEntry funName' in call_number `seq` dummyBinding
    |]

  -- extract the call_number Id
  let Just !var = (something $ mkQ Nothing (\case
        x | "call_number" == occNameStr x -> Just x
        _ -> Nothing
        )) letExpr

  -- update state
  !state <- get
  put state {ws_callCounters = var : counters}

  -- apply the λ-wrapped let binder to the original expression
  !completeExpr <- lift . GHC.zonkTopLExpr $ everywhere (mkT removeSrcSpans `extT` replaceDummyWith lexpr) letExpr

  -- FIXME: adhocbomination
  modify' $ \s -> s {ws_incrCCSuccess = True}
  trace''' (("incrCC result:\n" ++) . pprWithoutNull $ completeExpr) $ pure ()
  return $! GHC.GRHS x guards completeExpr

  where
    removeSrcSpans :: GenLocSpan LExpr -> GenLocSpan LExpr
    removeSrcSpans (GHC.L _ x) = GHC.L GHC.noSrcSpan x

    replaceDummyWith :: LExpr -> LExpr -> LExpr
    replaceDummyWith replacement = \case
      (GHC.L _ (GHC.HsWrap _ _ (GHC.HsVar _ (GHC.L _ x))))
        | "dummyBinding" == occNameStr x -> replacement
      x -> x

incrementCallCounter x = pure x

isMatch :: Match -> Bool
isMatch GHC.Match {} = True
isMatch _ = False

rewrite :: Binds -> GHC.TcM Binds
rewrite binds = fst <$> runStateT (zggoM tranz react binds) initialState
  where

  tranz :: Data a => Zipper a -> StateT WrapperState GHC.TcM (Zipper a)
  tranz z = do
    z <- transM trans z
    -- `mkM` is `extM return`
    modify' $ \s -> s {ws_incrCCSuccess = False}
    z <- transM (mkM incrementCallCounter) z
    WS {ws_incrCCSuccess = success} <- get
    if success then
      flip join (down z >>= down) $ \case
        Just z  -> flip trace'' (pure z) . pprWithoutNull @(Maybe LExpr) . query cast . fromJust
        Nothing -> error "this should never happen"
    else pure z

  react :: Zipper Binds -> StateT WrapperState GHC.TcM (Zipper Binds)
  react z = do
    let mrhs = query cast z
    -- TODO this match is really ugly, the pattern should at least be given
    -- a name and shared between incrementCallCounter and this place.
    -- Ideally we'd avoid this double-check completely though, which may be
    -- possible if we do multiple passes.
    let shouldPop = case mrhs of
          -- FIXME: so atm we seem to be failing the span /= GHC.noSrcSpan check,
          --        because we react before the span is deleted (right before the
          --        incrementCC invocation), and by the time we return we're no
          --        longer in a RHS but in the parent node (so the trace here isn't
          --        even called).
          --        oops, after, not before, right? 'cause the span is missing
          --        when we check here, and incrementCC removes it
          Just (GHC.GRHS _ _ (GHC.L span e) :: RHS) | trace "shouldPop?" span == GHC.noSrcSpan
            -> trace''' ("here comes the poppery\n" ++ gshow e) True
          _ -> False
    counters <- gets ws_callCounters
    unless (null counters) $
      -- we just matched an incrementCallCounter application
      -- it's time to pop the latest counter from the stack,
      -- as we're returning from the lambda body
      trace'' ("would pop " ++ pprWithoutNull (listToMaybe counters)) $
        modify' $ \s -> s {ws_callCounters = tail counters}
    pure z

  initialState = WS {ws_fun = Nothing, ws_callCounters = [], ws_incrCCSuccess = False}

  -- the monadic transformation capturing function info, ~~introducing call
  -- number variables~~ (incrementCallCounter is now on holiday in
  -- tranzylvania), and wrapping argument references
  trans :: Typeable a => a -> StateT WrapperState GHC.TcM a
  trans = mkM collectFunInfo `extM` wrapRef

  boundVars = do
    match <- listify isMatch binds
    lpat <- GHC.m_pats match
    lextract lpat
    where
      unl x = GHC.unLoc x

      lextract :: GHC.LPat GHC.GhcTc -> [GHC.Id]
      lextract = extractIds . unl

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
  wrapRef top@(GHC.L span v@(GHC.HsVar x lid)) =
    trace''' (indentWithRangle $ unlines
      [ "here's the deal: ", pprWithoutNull lid
      , "and do we have a noSrcSpan?", show span
      , "please enjoy this gshow output, it's on the house", gshow v ]) $
    -- make sure the Id is bound by some pattern and it's not a part of an already rewritten expression
    if GHC.unLoc lid `elem` boundVars && span /= GHC.noSrcSpan then do
      trace' "wrapRef" $ pure ()
      WS { ws_fun          = Just funName
         , ws_callCounters = counters@(callCounter : _)
         } <- get

      let argName = trace
            ("the other counters are " ++ pprWithoutNull counters)
            . occNameStr . GHC.varName . GHC.unLoc $ lid
      let varWithoutSrcSpan = GHC.L GHC.noSrcSpan v
      let funName' = occNameStr funName

      Just (_, !org_ty) <- lift $ toTyped varWithoutSrcSpan
      !pap <- lift $! tc (GHC.intTy ~> org_ty ~> org_ty) [| traceArg funName' argName |]

      let counter = GHC.L GHC.noSrcSpan $ GHC.HsVar GHC.NoExtField $ GHC.L GHC.noSrcSpan callCounter
      let !final_expr = GHC.mkHsApp (GHC.mkHsApp pap counter) varWithoutSrcSpan

      -- Zonk to instantiate type variables
      res <- lift $! GHC.zonkTopLExpr final_expr
      trace''' ("wrapRef result:\n" ++ pprWithoutNull res) $ pure res
    else return top

  wrapRef e = return e

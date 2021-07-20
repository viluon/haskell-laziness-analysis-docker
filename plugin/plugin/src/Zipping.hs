{-# LANGUAGE TypeApplications, Rank2Types, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable #-}

module Zipping
( zgoM
, zggoM
-- only for debugging
, goM
, ztrans
, exs
, trexs
, testGoM
, testGoM2
) where

import Logging

-- monad utilities in base
import Control.Monad ( (<=<) )

-- syb
import Data.Generics.Aliases (extM, extT, mkM, mkQ, mkT, GenericM)
import Data.Generics.Basics  (Data(gmapM), Typeable, cast)
import Data.Generics.Text    (gshow)

-- syz
import Data.Generics.Zipper

ggoM :: Monad m => (Zipper a -> m (Zipper a)) -> (Zipper a -> m (Zipper a)) -> Zipper a -> m (Zipper a)
ggoM trns react zipr' = do
  -- transform the current node
  zipr <- trns zipr'
  -- we wanna do this: solve it for children, left to right
  -- then "pop" and move to the right sibling
  -- top-down is trns as the first step, bottom-up is trns as the last step
  zipr <- downM (pure zipr) (ggoM trns react . leftmost) zipr
  zipr <- react zipr
  rightM (pure zipr) (ggoM trns react) zipr

goM :: Monad m => (forall d. Data d => d -> m d) -> (Zipper a -> m (Zipper a)) -> Zipper a -> m (Zipper a)
goM trns = ggoM (transM trns)

zgoM :: (Monad m, Data a) => (forall d. Data d => d -> m d) -> (Zipper a -> m (Zipper a)) -> a -> m a
zgoM f r = fmap fromZipper . goM f r . toZipper

zggoM :: (Monad m, Data a) => (Zipper a -> m (Zipper a)) -> (Zipper a -> m (Zipper a)) -> a -> m a
zggoM f r = fmap fromZipper . ggoM f r . toZipper


-- debugging

exs :: [[Int]]
exs = do
  len <- [2 .. 5]
  pure $ take len $ drop (2 * len - 1) [9, 8 .. 1]

data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Eq, Show, Data, Functor, Foldable)

buildTree :: [a] -> Tree a
buildTree []      = Leaf
buildTree (x:xs)  = let (l, r) = splitAt 4 xs
                    in x `Branch` buildTree l $ buildTree r

trexs :: Tree Int
trexs = buildTree $ concat exs

ztrans :: Monad m => Zipper a -> m (Zipper a)
ztrans = goM pure undefined where f x | trace (gshow x) True = pure x; f _ = undefined

dincr :: Data d => d -> d
dincr = mkT (+ (1 :: Int))

showHole :: forall a b. (Typeable a, Show a) => Zipper b -> Maybe String
showHole = fmap show . getHole @a

testGoM :: Maybe [[Int]]
testGoM =
  fromZipper
    <$> goM
      (\x -> trace (gshow x) $ pure $ dincr x)
      (\z -> trace ("react: " ++ show (
        showHole @Int z <> showHole @[Int] z <> showHole @[[Int]] z)
        ) $ pure z)
      (toZipper exs)

testGoM2 :: Maybe (Tree Int)
testGoM2 = fromZipper <$>
  goM
    (\x -> trace (("trns: " ++) $ gshow x) $ pure $ dincr x)
    (\z -> trace (("react: " ++) . show . getHole @Int $ z) $ pure z)
    (toZipper trexs)

-- | Apply a generic monadic transformer using the specified movement operations.
myM :: (Monad m)
      => (Zipper a -> Maybe (Zipper a)) -- ^ Move to
      -> (Zipper a -> Maybe (Zipper a)) -- ^ Move back
      -> m (Zipper a) -- ^ Default if can't move
      -> (Zipper a -> m (Zipper a)) -- ^ Monadic transformer if can move
      -> Zipper a -- ^ Zipper
      -> m (Zipper a)
myM move1 move2 b f =
  -- move, transform (with f), then move back
  moveQ move1 b (moveQ move2 b return <=< f)


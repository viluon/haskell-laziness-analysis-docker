import qualified Data.IntMap as M
import Data.List

primes :: [Int]
primes = step 2 M.empty
  where step :: Int -> M.IntMap [Int] -> [Int]
        step n m | Just vs <- M.lookup n m =
          step (n+1) $ foldl' (\m v -> M.insertWith (++) (n+v) [v] m) m vs
        step p m = p : step p (M.insert p [p] m)

main :: IO ()
main = print $ take 10 primes

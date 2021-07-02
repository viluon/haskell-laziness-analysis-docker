{-# LANGUAGE BangPatterns #-}

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)


{-# NOINLINE counter #-}
counter = unsafePerformIO $ newIORef 0

traceEntry :: String -> Int
traceEntry str = unsafePerformIO $ do
  putStrLn $ "entry: " ++ str
  atomicModifyIORef' counter (\x -> (x + 1, ()))
  readIORef counter

traceArg :: String -> String -> Int -> a -> a
traceArg funName arg callNumber x = unsafePerformIO $ do
  putStrLn $ concat ["arg: ", funName, " ", show callNumber, " ", arg]
  return x

qsort xs = let !callNumber = traceEntry "qsort"
           in case traceArg "qsort" "xs" callNumber xs of
             []     -> []
             (a:as) ->
               qsort left
                 ++ [traceArg "qsort" "a" callNumber a]
                 ++ qsort right
               where
               (left, right)
                = (filter
                    (<= traceArg "qsort" "a" callNumber a)
                    (traceArg "qsort" "as" callNumber as),
                   filter
                    (> traceArg "qsort" "a" callNumber a)
                    (traceArg "qsort" "as" callNumber as))

main = let x = qsort [3, 1, 2] in print (x, x)

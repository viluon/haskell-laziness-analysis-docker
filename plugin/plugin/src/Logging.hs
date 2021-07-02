{-# LANGUAGE TupleSections #-}

module Logging (
  traceEntry, traceArg
) where

-- unsafe features
import System.IO.Unsafe (unsafePerformIO)

-- IO
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO

-- time
import Data.Time.Clock.System (getSystemTime, SystemTime(..))

-- ghc-heap-view
import qualified GHC.HeapView as GHC

-- concurrency
import Control.Concurrent (myThreadId)

-- containers
import qualified Data.Map.Strict as M


data TraceSort = ArgTrace | EntryTrace deriving (Eq, Show)

{-# NOINLINE outputHandle #-}
outputHandle :: IORef Handle
outputHandle = unsafePerformIO $ do
  -- TODO set the path with a CLI argument if possible
  handle <- openFile "/tmp/trace.csv" AppendMode
  newIORef handle

logt :: TraceSort -> [String] -> IO ()
logt sort params = do
  MkSystemTime {systemNanoseconds = time} <- getSystemTime
  threadId <- myThreadId
  handle <- readIORef outputHandle

  hPutStr handle
    . (++ "\n")
    . concatMap (++ ",")
    . ([show time, show threadId, show sort] ++)
    $ params
  hFlush handle

{-# NOINLINE functionEntries #-}
functionEntries :: IORef (M.Map String Int)
functionEntries = unsafePerformIO $ newIORef M.empty

traceEntry :: String -> Int
traceEntry funName = unsafePerformIO $ do
  atomicModifyIORef' functionEntries ((, ()) . M.insertWith (+) funName 1)
  entries <- readIORef functionEntries
  let callNumber = entries M.! funName

  logt EntryTrace
    [ funName
    , show callNumber
    ]
  return callNumber

closureType :: GHC.GenClosure GHC.Box -> String
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

{-

foo x =
  RHS <- replace x with (traceArg "foo" "x" call_number x)

traceArg :: ... -> a -> IO a

-}

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

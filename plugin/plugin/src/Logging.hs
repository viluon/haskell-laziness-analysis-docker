{-# LANGUAGE TupleSections #-}

module Logging
( traceEntry
, traceArg
, trace
, trace'
, trace''
, trace'''
, pprWithoutNull
) where

-- unsafe features
import System.IO.Unsafe (unsafePerformIO)

-- IO
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO
import System.Environment

-- utilities from base
import Data.Maybe

-- time
import Data.Time.Clock.System (getSystemTime, SystemTime(..))

-- ghc
import qualified Outputable as GHC (Outputable, ppr, showSDocUnsafe)

-- ghc-heap-view
import qualified GHC.HeapView as GHC

-- concurrency
import Control.Concurrent (myThreadId)

-- containers
import qualified Data.Map.Strict as M

-- debug
import qualified Debug.Trace as Dbg (trace)


data TraceSort = ArgTrace | EntryTrace deriving (Eq, Show)

{-# NOINLINE outputHandle #-}
outputHandle :: IORef Handle
outputHandle = unsafePerformIO $ do
  path <- fromMaybe "/tmp/trace.csv" <$> lookupEnv "HS_TRACING_OUTPUT"
  handle <- openFile path AppendMode
  writable <- hIsWritable handle
  if writable
  then newIORef handle
  else error $ "Dynamic tracing GHC plugin: Cannot write to \n'" ++ path ++ "'"

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

-- debugging utilities

colourfulTrace :: Int -> String -> a -> a
colourfulTrace c str = Dbg.trace $ concat ["\ESC[3", show c, "m", str, "\ESC[0m"]

trace : trace' : trace'' : trace''' : _ = colourfulTrace <$> [2..]

pprWithoutNull :: GHC.Outputable a => a -> String
pprWithoutNull = filter (/= '\0') . GHC.showSDocUnsafe . GHC.ppr

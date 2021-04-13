{- HLINT ignore "Use record patterns" -}

module TracingPlugin (plugin) where

import Rewriting (rewrite)

-- ghc
import qualified GhcPlugins as GHC
import qualified TcRnMonad  as GHC

-- debug
import Debug.Trace (trace)


plugin :: GHC.Plugin
plugin =  GHC.defaultPlugin { GHC.typeCheckResultAction = const install {- this is to get rid of CLI options -}
                            }

install :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
install _ env | trace "●●● rewriting-plugin entry ●●●" True = do
  new_binds <- rewrite $ GHC.tcg_binds env
  GHC.liftIO . putStrLn . GHC.showSDocUnsafe . GHC.ppr $ new_binds
  return env {GHC.tcg_binds = new_binds}
install _ _ = undefined

-- goal: rewrite argument references to go through trace
-- plan:
--   - [x] capture all bindings
--   - [x] rewrite refs
--   - [x] propagate scope (! misleading, we care about the surrounding function name) to ref sites
--     - how do we do this?
--   - [x] use ghc-heap-view to peek inside the arguments
--   - [x] add a global mutable map
--   - [x] count function calls via the map

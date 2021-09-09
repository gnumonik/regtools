module Main where

import Explore.Repl
import Lib (testhivepath)
import Explore.Plugins 
import Explore.CmdOpts 
import Types 


main :: IO ()
main = do --initRepl testhivepath
  Options hivePath mOpts <- getOpts
  case mOpts of 
    PLUGIN pluginPath outputPath ->
      runPlugin_ pluginPath outputPath hivePath 

    SHELL -> initRepl hivePath 

    CHECKHASH hashPath -> checkKeyHash hivePath hashPath 
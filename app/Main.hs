module Main where

import Explore.Repl ( initRepl, checkKeyHash_ )
import Lib (testhivepath)
import Explore.Plugins ( runPlugin_ ) 
import Explore.CmdOpts
    ( getOpts,
      ModeOptions(CHECKHASH, PLUGIN, SHELL),
      Options(Options) ) 


main :: IO ()
main = do --initRepl testhivepath
  Options hivePath mOpts <- getOpts
  case mOpts of 
    PLUGIN pluginPath  ->
      runPlugin_ pluginPath  hivePath 

    SHELL -> initRepl hivePath 

    CHECKHASH hashPath -> checkKeyHash_ hivePath hashPath 


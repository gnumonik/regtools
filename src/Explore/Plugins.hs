module Explore.Plugins where

import Prelude hiding (lex)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer 
import Explore.Magic 
import qualified Data.Text as T
import Data.Void
import Control.Monad (void)
import Text.Megaparsec.Char
import Data.Functor (($>))
import Types 
import Data.Kind 
import Data.Singletons 
import Data.Word
import Explore.Optics.Utils 
import Data.Singletons.Decide
import Explore.LexDSL 
import qualified Data.Map as M 
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Control.Monad.State.Strict as ST 
import Data.Constraint
import qualified Data.ByteString.Char8 as BC
import Explore.ParseDSL 
import qualified Data.Text.IO as TIO 
import Lib (readHive)
import Explore.EvalDSL 
import Control.Lens (set,_1)
import Data.Either 


type PluginArgs = [Some TypedVar]






-- The Command Line args version 
runPlugin_ :: PluginPath -> HivePath -> IO ()
runPlugin_ pPath hPath = do
  pluginRaw <- TIO.readFile pPath
  case lexPlugin pluginRaw of
    Left err -> putStrLn (errorBundlePretty err)
    Right toks -> case ST.runState (runParserT parsePlugin "" toks) emptyContext of 
      (Left err,cxt) -> putStrLn ("\n" <> errorBundlePretty err)
      (Right exprs,cxt) -> do 
        !hData <- readHive hPath 
        execPlugin putStrLn exprs hData 



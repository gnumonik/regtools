module Explore.Repl where

import System.Console.Haskeline 
import Explore.EvalDSL 
import Explore.LexDSL 
import Explore.ParseDSLYO 
import Control.Monad.Errors
import qualified Data.Text as T
import qualified Control.Monad.Trans.State.Strict as ST
import Explore.ExploreM
import Control.Monad.IO.Class
import Lib
import qualified Data.Map as M 
import Text.Megaparsec 
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Look 
import Debug.Trace 

type ReplM = InputT (ReaderT HiveData (ST.StateT ValMap IO))  

instance MonadState ValMap ReplM where 
  get = lift get
  put = lift . put

instance MonadLook HiveData ReplM where  
  look =  lift  ask 
  looks f = f <$> look 


initRepl :: FilePath -> IO () 
initRepl fPath = do 
  !hData <-  readHive fPath 
  let vMap = M.empty 
  ST.evalStateT (runReaderT (runInputT defaultSettings loop) hData) vMap  
 where 
   loop :: ReplM () 
   loop = do 
     outputStrLn "Enter a query or command."
     getInputLine "> " >>= \case 
      Nothing -> loop 
      Just line -> do 
        lexParseLine (T.pack line)
        loop 


lexParseLine :: T.Text -> ReplM ()
lexParseLine line  = trace "lexing" $ case runParser (some dsltoks) "" line of 
  Left err -> trace "lex err" $ outputStrLn (show err)
  Right lexed -> trace "lex sucess" $ case ST.evalState (runParserT dslExp "" lexed) M.empty of 
    Left err -> trace "parse err" $ outputStrLn (show err) 
    Right (Some expr) -> do 
      (e :: HiveData) <- look 
      s <- get 
      printer <- getExternalPrint 
      !(merr,s') <- liftIO $ ST.runStateT (evalDSL printer e expr) s  
      forM_ merr $ \err -> liftIO $ print merr 
      trace "eval complete" $ modify' $ \_ -> s' 

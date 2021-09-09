module Explore.Repl where

import System.Console.Haskeline 
import Explore.EvalDSL 
import Explore.LexDSL 
import Explore.ParseDSL
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
import Data.Time.Clock 
import Types 
import Data.List.NonEmpty as NE 
import qualified Data.Aeson as A 
import Explore.Optics.NK (checkHash)
import Control.Monad.Errors.Class
import qualified Data.ByteString as BS 

type ReplM = InputT (ReaderT HiveData (ST.StateT (ValMap,Context)IO))  

instance MonadState (ValMap,Context) ReplM where 
  get = lift get
  put = lift . put

instance MonadLook HiveData ReplM where  
  look =  lift  ask 
  looks f = f <$> look 

initRepl :: FilePath -> IO () 
initRepl fPath = do 
  putStrLn "\nInitializing RegTools\n"
  putStrLn $ "Loading Registry Hive File @" <> fPath <> "\n"
  t1 <- getCurrentTime 
  !hData <-  readHive fPath
  t2 <- getCurrentTime 
  let diffTime = diffUTCTime t2 t1  
  putStrLn  $ "Hive Loaded in " <> show diffTime <> " seconds"
  let vMap = M.empty
  putStrLn "Enter a query or command." 
  ST.evalStateT (runReaderT (runInputT defaultSettings loop) hData) (vMap,emptyContext)  
 where 
   loop :: ReplM () 
   loop = do
     outputStrLn "" 
     getInputLine "> " >>= \case 
      Nothing -> loop 
      Just line -> do 
        runLine (T.pack line) >>= \case 
          Nothing -> loop 
          Just ABORT -> pure () 

runLine :: T.Text -> ReplM (Maybe ABORT)
runLine line  = gets snd >>= \cxt1 -> case runParser (some dsltoks) "" line of 
  Left err    -> outputStrLn ("\n" <> errorBundlePretty err) >> pure Nothing 

  Right lexed -> case ST.runState (runParserT dslExp "" lexed) (cxt1,REPLMode) of

    (Left err,cxt)  ->  
      outputStrLn ("\n" <> errorBundlePretty err) >> pure Nothing 

    (Right (Some expr),cxt) -> do 
      (e :: HiveData) <- look 
      (s,_cxt) <- get 
      printer <- getExternalPrint 
      !(merr,s') <- liftIO $ ST.runStateT (evalDSL printer e expr) s
      case merr of 
        Left ABORT -> pure . Just $ ABORT 
        Right Nothing -> do 
          modify' $ \_ -> (s',fst cxt) 
          pure Nothing 
        Right (Just err) -> do 
          modify' $ \_ -> (s',fst cxt) 
          liftIO (printer . show $ err) >> pure Nothing    
      
 where 
   prettyErr :: forall s e
              . (VisualStream s, ShowErrorComponent e) 
             => ParseErrorBundle s e -> String
   prettyErr errBundle = let err = NE.head (bundleErrors errBundle)
                         in parseErrorTextPretty err 

-- split this out to its own module later 


type HashPath = FilePath 

checkKeyHash :: HivePath -> HashPath -> IO ()
checkKeyHash hvPath hshPath = do 
  putStrLn "\nInitializing RegTools in Hash Checking Mode\n"
  putStrLn $ "Loading Registry Hive File @" <> hvPath <> "\n"
  t1 <- getCurrentTime 
  !hData <-  readHive hvPath
  t2 <- getCurrentTime 
  let diffTime = diffUTCTime t2 t1  
  putStrLn  $ "\nHive Loaded in " <> show diffTime <> " seconds"
  putStrLn $ "\nLoading key hash file @" <> hshPath
  putStrLn $ "\nChecking hashes... "
  rawHashFile <- BS.readFile hshPath 
  case A.decodeStrict @KeyHashFileObj rawHashFile of  
    Nothing -> putStrLn "Error! Could not deserialize the hash file JSON"
    Just hashFile -> case hashFile of 
      OneKey kh -> do 
        runReaderT (runE $ checkHash kh) (hData,putStrLn)
        pure ()  
      ManyKeys khs -> do 
        runReaderT (runE $ mapM checkHash khs) (hData,putStrLn) 
        pure () 
  putStrLn "\nHash check complete."

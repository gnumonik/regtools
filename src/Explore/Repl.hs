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
import Explore.Optics.NK (checkHash, checkValHash)
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
  putStrLn  $ "Hive Loaded in " <> show diffTime <> " seconds\n"
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
runLine line  = gets snd >>= \cxt1 -> case runParser (some dslToks) "" line of 
  Left err    -> outputStrLn ("\n" <> errorBundlePretty err) >> pure Nothing 

  Right lexed -> case ST.runState (runParserT dslExp "" lexed) cxt1 of

    (Left err,_)  ->  
      outputStrLn ("\n" <> errorBundlePretty err) >> pure Nothing 

    (Right (Some expr), cxt) -> do 
      (e :: HiveData) <- look 
      s <- gets fst  
      printer <- getExternalPrint 
      (merr,s') <- liftIO $ ST.runStateT (evalDSL printer e expr) s
      case merr of 
        Just ABORT -> pure . Just $ ABORT 
        Nothing -> do 
          modify' $ const (s',cxt) 
          pure Nothing 

-- Move this somewhere else. Maybe?
type HashPath = FilePath 

checkKeyHash_ :: HivePath -> HashPath -> IO ()
checkKeyHash_ hvPath hshPath = do 
  putStrLn "\nInitializing RegTools in Hash Checking Mode\n"
  putStrLn $ "Loading Registry Hive File @" <> hvPath <> "\n"
  t1 <- getCurrentTime 
  !hData <-  readHive hvPath
  t2 <- getCurrentTime 
  let diffTime = diffUTCTime t2 t1  
  putStrLn  $ "\nHive Loaded in " <> show diffTime <> " seconds"
           <> "\nLoading key hash file @" <> hshPath
           <> "\nChecking hashes... "
  checkKeyHash putStrLn hData hshPath 




{--

OneKey
    ( KeyHash
        { _fqKeyName = "S-1-5-21-3199533274-2294187411-1904285961-1001_Classes\.mhtml"
        , _timeHash = "eb2c05ce6611f5e54cd3197b2a9ec4da"
        , _valuesHash =
            [ ValHash
                { _vHashName = "Content Type"
                , _vHashPath = "S-1-5-21-3199533274-2294187411-1904285961-1001_Classes//.mhtml"
                , _vHashData = "fb23d1a8f41117286e6df7ce5733aaca"
                }
            , ValHash
                { _vHashName = ""
                , _vHashPath = "S-1-5-21-3199533274-2294187411-1904285961-1001_Classes//.mhtml"
                , _vHashData = "3d75c97a6ec0a48a1619199645e35e3f"
                }
            ]
        }
    )

--}
module Explore.EvalDSL where

import Types
import Data.Word (Word32)
import Control.Lens 
import Lib 
import Explore.ExploreM 
import Explore.Optics.General
import ParseM
import qualified Data.Map as M
import Control.Monad.Look
import Data.List (foldl')
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Vector as V
import Control.Lens.Action
import Explore.ParseDSL
import Control.Monad.Errors 
import Control.Monad.Errors.Class 
import qualified Data.Text as T 
import qualified  Control.Monad.Trans.State.Strict as ST  
import Explore.Magic
import Data.Kind
import Unsafe.Coerce 
import Data.Singletons
import Control.Monad.State.Class
import qualified Control.Monad.Trans.Reader as R 
import Control.Monad.Reader.Class 
import Control.Monad.Trans.Class 
import Control.Monad.IO.Class 
import Data.Constraint
import Data.Singletons.Decide (decideEquality)
import qualified Data.Text.IO as TIO 
import Control.Monad.Reader
import Text.Megaparsec (errorBundlePretty, runParserT, runParser, some, between)
import Explore.LexDSL (sc, pluginToks)
type ValMap = M.Map T.Text DSLValue 

data DSLValue :: Type where 
  MkDSLValue :: (t ~ DSLToHask d, PrettyRefl d) => Sing d -> t -> DSLValue 

type EvalM a = ST.StateT ValMap IO  a

data ABORT = ABORT deriving (Show, Eq)

tvSing :: forall a. TypedVar a -> Sing a 
tvSing (MkTypedVar txt) = sing @ a 

lookupVal :: forall a. (String -> IO ()) -> TypedVar a -> EvalM (Either ABORT (Dict (PrettyRefl a),DSLToHask a))
lookupVal printer tv@(MkTypedVar txt) = get >>= \vals -> 
  case M.lookup txt vals of 
    Nothing -> do 
      liftIO . printer $ "FATAL ERROR! BOUND VALUE DOES NOT EXIST! ABORTING!"
      pure . Left $ ABORT 

    Just (MkDSLValue sV v) -> case decideEquality sV (tvSing tv) of 
      Just Refl -> pure . Right $ (Dict,v) 
      Nothing   -> do 
        liftIO . printer $ "FATAL ERROR! TYPE MISMATCH IN EVALUATION!\n"  
                        <> "(This should be impossible. If it happens it means the type checker " 
                        <>  "failed in some truly bizarre way. Pleas report this error if it occurs.)"
        pure . Left $ ABORT 

-- | Evaluates a DSL expression. First argument is the thread-safe 
--   printer function Haskeline generates 
evalDSL :: forall a. (String -> IO ()) -> HiveData -> DSLExp a -> EvalM (Either ABORT (Maybe QueryErrors))
evalDSL printer hData = \case 
  RootQuery fcs -> case withDict (focDict fcs) $ collapseFocus fcs of 
    MkBoxedMFold fld ->  do 
      liftIO (query printer hData fld)  
      pure . Right $ Nothing 

  VarQuery tv@(MkTypedVar txt) qb -> lookupVal printer tv >>= \case 
    Left ABORT -> pure . Left $ ABORT 
    Right (Dict,v)    -> case renderQB qb of
      MkBoxedMFold fld -> do 
        !_ <- liftIO $ query' printer hData v fld 
        pure . Right $ Nothing 

  Assign fcs (MkTypedVar txt) -> case withDict (focDict fcs) $ collapseFocus fcs of 
    MkBoxedMFold fld ->  do 
      x <- liftIO (query printer hData fld)
      case x of 
        Left err -> pure . Right . Just $ err
        Right (a :: DSLToHask a)  -> do 
          let sF = focSing fcs 
          modify (withDict (focDict fcs) $ M.insert txt (MkDSLValue sF a))
          pure . Right $ Nothing 

  Command cmd -> runCmd cmd 

 where

   runCmd :: DSLCommand  -> EvalM (Either ABORT (Maybe QueryErrors)) 
   runCmd = \case 
      EXIT -> pure . Left $ ABORT 
      
      LOAD _ _-> pure . Right $ Nothing --Load pretty much just exists for typechecking purposes 

      RUN inPath outPath -> do 
        liftIO $ runPlugin printer inPath outPath hData 
        pure . Right $ Nothing 

      HELP -> liftIO (printer "Help urself") >> pure (Right Nothing)

{-- PLUGINS 
    (Here temporarily pending module reorganization)
--}
type PluginM = ReaderT HiveData (ST.StateT ValMap IO)

type HivePath = FilePath 
type PluginPath = FilePath 
type OutputPath = FilePath 

runPlugin :: Printer -> PluginPath -> OutputPath -> HiveData -> IO ()
runPlugin f pPath oPath hData = loadPlugin f pPath oPath >>= \case 
  Left err   -> f . T.unpack $ err 
  Right exps -> execPlugin f exps hData  

-- this is ugly and dirty and bad and i need to fix it at some point
loadPlugin :: Printer -> FilePath -> FilePath -> IO (Either T.Text [Some DSLExp])
loadPlugin f pPath fPath = do
  pluginRaw <- TIO.readFile pPath
  case lexParsePlugin pluginRaw "test" of 
    Left err -> pure . Left $ err 
    Right _  -> pure $ lexParsePlugin pluginRaw fPath


lexParsePlugin :: T.Text -> FilePath -> Either T.Text [Some DSLExp]
lexParsePlugin pluginRaw outPath = case lexPlugin pluginRaw of
    Left err -> Left . T.pack $ errorBundlePretty err
    Right toks -> case ST.runState (runParserT parsePlugin "" toks) (emptyContext, PluginMode outPath) of 
      (Left err,cxt) -> Left . T.pack $ errorBundlePretty err
      (Right exprs,cxt) ->  Right $ exprs 
      

lexPlugin txt = runParser (sc >> some pluginToks) "" txt 

parsePlugin :: DSLParser [Some DSLExp]
parsePlugin = do 
  tk Plugin
  between (tk LCurly) (tk RCurly) (some dslExp)


execPlugin :: Printer -> [Some DSLExp] -> HiveData -> IO ()
execPlugin f exprs hData = ST.evalStateT (runReaderT (_execPlugin f exprs) hData) M.empty  

_execPlugin :: Printer -> [Some DSLExp] -> PluginM ()
_execPlugin _ []        = pure ()
_execPlugin f (Some x:xs) = do 
  vals   <- lift ST.get
  hData <- ask   
  (a,newVals) <- liftIO $ ST.runStateT (evalDSL putStrLn hData x) vals 
  case a of 
    Left ABORT -> do 
        liftIO . f $ "FATAL ERROR! ABORTING EXECUTION!"
      
    Right Nothing -> do 
        lift . ST.modify' $ const newVals 
        _execPlugin f xs 

    Right (Just qErrs) -> do
      liftIO $ f "Error(s) while running query:" 
      liftIO $ f (show qErrs)
      pure () -- it occurs to me now that this branch is redundant. will remove later 
        







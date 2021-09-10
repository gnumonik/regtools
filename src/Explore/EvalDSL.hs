module Explore.EvalDSL where

import Types
import Data.Word (Word32)
import Control.Lens ( type (:~:)(Refl)) 
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
import qualified Data.Text.Lazy as TL 
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
import Explore.LexDSL (sc, dslToks)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import Text.Pretty.Simple (pShow)
import Text.PrettyPrint.Leijen
    ( Pretty(pretty), displayS, renderPretty )
type ValMap = M.Map T.Text DSLValue 

data DSLValue :: Type where 
  MkDSLValue :: (t ~ DSLToHask d, PrettyRefl d) => Sing d -> t -> DSLValue 

type EvalM a = ST.StateT ValMap IO  a

data ABORT = ABORT deriving (Show, Eq)



lookupVal :: forall a. (String -> IO ()) -> TypedVar a -> EvalM (Either ABORT (Dict (PrettyRefl a),DSLToHask a, Sing a))
lookupVal printer tv@(MkTypedVar txt) = get >>= \vals -> 
  case M.lookup txt vals of 
    Nothing -> do 
      liftIO . printer $ "FATAL ERROR! BOUND VALUE DOES NOT EXIST! ABORTING!"
      pure . Left $ ABORT 

    Just (MkDSLValue sV v) -> case decideEquality sV (tvSing tv) of 
      Just Refl -> pure . Right $ (Dict,v,sV) 
      Nothing   -> do 
        liftIO . printer $ "FATAL ERROR! TYPE MISMATCH IN EVALUATION!\n"  
                        <> "(This should be impossible. If it happens it means the type checker " 
                        <>  "failed in some truly bizarre way. Pleas report this error if it occurs.)"
        pure . Left $ ABORT 

-- | Evaluates a DSL expression. First argument is the thread-safe 
--   printer function Haskeline generates 
evalDSL :: forall a. (String -> IO ()) -> HiveData -> DSLExp a  -> EvalM (Maybe ABORT)
evalDSL printer hData exp = evalPure exp >>= \case 
    Left _ -> pure . Just $ ABORT 
    _      -> pure Nothing 
 where
    catchErr f err = liftIO (f err) >> pure 
    evalPure :: forall a. DSLExp a -> EvalM (Either ABORT (DSLToHask a)) 
    evalPure = \case 
      RootQuery fcs -> case withDict (focDict fcs) $ collapseFocus fcs of 
        MkBoxedMFold fld ->  do 
          liftIO (query printer hData fld) >>= \case 
            Left err -> pure . Left $ ABORT 
            Right a  -> pure . Right $ a 

      VarQuery tv@(MkTypedVar txt) qb -> lookupVal printer tv >>= \case 
        Left ABORT -> pure . Left $  ABORT 
        Right (Dict,v,s)    -> case renderQB qb of
          MkBoxedMFold fld -> do 
            liftIO (query' printer hData v fld) >>= \case 
              Left err -> pure . Left $ ABORT 
              Right a  -> pure . Right $ a  

      Assign e (MkTypedVar txt) -> evalPure e >>= \case 
        Left ABORT -> pure . Left $ ABORT 
        Right v    ->  do 
                let sF = expSing e 
                modify (withDict (expDict e) $ M.insert txt (MkDSLValue sF v))
                pure . Right $ v

      IfThen e1 e2 e3 -> do 
        evalPure e1 >>= \case 
          Left ABORT -> pure . Left $ ABORT 
          Right b    -> if b then evalPure e2 else evalPure e3  

      DSLVar tv -> lookupVal printer tv >>= \case 
        Left ABORT -> pure . Left $ ABORT 
        Right (Dict,v,s) -> pure . Right $ v 

      Append l1 l2 -> 
        evalPure l1 >>= \case 
          Left _   -> pure $ Left ABORT 
          Right v1 -> evalPure l2 >>= \case 
            Left _   -> pure $ Left ABORT 
            Right v2 -> pure . Right $ v1 <> v2 
      Concat l -> 
        evalPure l >>= \case 
          Left _  -> pure $ Left ABORT 
          Right v -> pure . Right $ concat v 

      IsEmpty l1 -> 
        evalPure l1 >>= \case 
          Left _  -> pure $ Left ABORT 
          Right v -> pure . Right $ null v 

      Command c -> runCmd c >>= \case 
          Just _  -> pure $ Left ABORT 
          Nothing -> pure . Right $ () 
      
    runCmd :: DSLCommand  -> EvalM (Maybe ABORT) 
    runCmd = \case 
        SHOWTYPE e -> do 
          liftIO $ printer (show . fromSing . expSing $ e) 
          pure Nothing 

        EXIT -> pure . Just $ ABORT 

        HELP -> liftIO (printer "Help urself") >> pure Nothing

        WRITEJSON e fPath mstr -> do
          withDict (expDict e) $ 
            evalPure e >>= \case 
              Left ABORT -> pure . Just $ ABORT 
              Right a -> do  
                let f = printer 
                let js = case mstr of 
                          Nothing  -> A.toJSON a 
                          Just txt -> A.object [txt .= a] 
                liftIO $ f (TL.unpack . pShow $ js)
                liftIO $ A.encodeFile fPath js
                pure Nothing

        HASH Dict e fPath -> withDict (expDict e) $ do 
          evalPure e >>= \case 
            Left ABORT -> pure . Just $ ABORT 
            Right a -> do 
                let f = printer 
                let hashed =  mkHash a -- Needed to add a type family dependency/injectivity annotation for this to work
                liftIO . f . TL.unpack . pShow $ hashed 
                liftIO $ A.encodeFile fPath hashed 
                pure Nothing

        PPRINT e -> evalPure e >>= \case 
          Left ABORT -> pure . Just $ ABORT 
          Right a -> 
            let f = printer 
            in withDict (expDict e) 
              $ (liftIO 
                . f 
                . (<> "\n")
                . (\x -> displayS x  "") 
                . renderPretty 1.0 200 
                . pretty
                $ a) >> pure Nothing 

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
    Right toks -> case ST.runState (runParserT parsePlugin "" toks) emptyContext of 
      (Left err,cxt)    -> Left . T.pack $ errorBundlePretty err
      (Right exprs,cxt) -> Right $ exprs 

lexPlugin txt = runParser (sc >> some dslToks) "" txt 

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
    Just ABORT -> do 
        liftIO . f $ "FATAL ERROR! ABORTING EXECUTION!"
      
    Nothing -> do 
        lift . ST.modify' $ const newVals 
        _execPlugin f xs 








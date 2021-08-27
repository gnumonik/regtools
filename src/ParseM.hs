{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module ParseM where


import Data.Serialize 
import Data.Serialize.Get
import Data.Word 
import qualified Data.ByteString as BS 
import GHC.TypeLits 
import Data.Kind 
import Data.Proxy 
import System.IO 
import qualified Data.Vector as V
import Control.Lens 
import Types 
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Trans.Class
import qualified Data.Text as T 
import Control.Monad.Free.Church 
import Data.Functor.Compose 
import Control.Monad 
import Control.Applicative.Lift 
import Control.Monad.Reader.Class (MonadReader)
import Data.Functor.Constant 
import Data.Profunctor 
import Control.Concurrent.Async 
data ParseErr = ParseErr Word32 T.Text 

data ParseOutput a = ParseOutput Word32 Word32 a  

instance Functor ParseOutput where 
  fmap f (ParseOutput l s a) = ParseOutput l s (f a)



type FunSet = (Word32 -> Bool)

data RegEnv = RegEnv {_parsed       :: M.Map Word32 CellContent 
                     ,_rawCells     :: M.Map Word32 RawCell 
                     ,_parseErrs    :: [ParseErr]
                     ,_unparsed     :: FunSet}
makeLenses ''RegEnv 


data Parse a = Parse Word32 (Get a)

type Err a = Errors ParseErr a 

newtype ParseM a = ParseM (Compose (ReaderT Driver IO) (Errors ParseErr) a)

decompose :: ParseM a -> ReaderT Driver IO (Errors ParseErr a)
decompose (ParseM m) = getCompose m 

compose :: ReaderT Driver IO (Errors ParseErr a) -> ParseM a 
compose = ParseM . Compose 

withParseM :: forall a r. ((Compose (ReaderT Driver IO) (Errors ParseErr) a) -> r) -> ParseM a -> r 
withParseM f (ParseM m) = f m 

asReaderT :: forall x. ReaderT Driver IO x -> ParseM x 
asReaderT ma = ParseM . Compose $ ma >>= \a -> pure . Pure $ a 

asLift :: forall x. Errors ParseErr x -> ParseM x
asLift = \case 
  Pure a -> pure a 
  Other c -> ParseM . Compose . pure . failure $ getConstant c 


instance Functor ParseM where 
  fmap f (ParseM g) = ParseM . Compose $ (fmap . fmap) f (getCompose g)

instance Applicative ParseM where 
  pure a = ParseM . Compose $ pure (Pure a)

  (ParseM f) <*> (ParseM a) = ParseM . Compose $ 
    runErrors <$> getCompose f >>= \case 
      Left err -> pure $ failure err -- here's where we log them. maybe? 
      Right f'  -> runErrors <$> (getCompose a) >>= \case 
        Left err -> pure $ failure err 
        Right a' -> pure . Pure $ f' a' -- record success   

-- check whether this satisfies the monad laws 
-- when fewer tokes over the line
instance Monad ParseM where 
  return = pure 

  (ParseM a) >>= f = ParseM . Compose $ do 
    getCompose a >>= \x -> case runErrors x of 
      Left err -> logFailure err >> pure (failure err )

      Right a' -> case f a' of 
        ParseM m -> getCompose m

instance MonadIO ParseM where 
  liftIO = asReaderT . liftIO 


logFailure :: ParseErr -> ReaderT Driver  IO ()
logFailure (ParseErr w t) = ask >>= \e -> 
  liftIO . atomically . modifyTVar' e $ 
    over parseErrs (ParseErr w t :)

logSuccess :: forall a. IsCC a => ParseOutput a -> ReaderT Driver IO (ParseOutput a)
logSuccess out@(ParseOutput w s a) = ask >>= \e -> do 
  liftIO . atomically . modifyTVar' e $
      over parsed (M.insert w $ cc @a # a)
    . over unparsed (exclude (w,s)) 
  pure out 

liftE :: Either ParseErr a -> ParseM a 
liftE = \case 
  Left err -> asLift (failure err)
  Right a  -> pure a 

-- this is the most bizarre function i've ever written
unliftE :: ParseM a -> ParseM (Either ParseErr a) 
unliftE p =  do 
  let m' = decompose p
  x <- asReaderT m'
  liftE . extendE $ runErrors x 
 where 
   extendE :: Either ParseErr a -> Either ParseErr (Either ParseErr a)
   extendE e = pure e 

type Driver = TVar (RegEnv) 

exclude :: (Word32,Word32) -> (Word32 -> Bool) -> (Word32 -> Bool)
exclude (start,len) old x = old x && not (x >= start && x <= start + len) 

look :: ParseM Driver 
look = asReaderT ask 

parseError :: Word32 -> T.Text -> forall a. ParseM a
parseError w t = liftE (Left $ ParseErr w t)

-- need to check if it exists first to prevent duplication
parseCC :: IsCC a => Word32 -> Get a -> ParseM (ParseOutput a)
parseCC offset g =  look >>= (liftIO . readTVarIO) >>= \x -> 
  case x ^? (rawCells . ix offset) of 
    Nothing -> parseError offset "Raw cell doesn't exist"
    Just (RawCell _ s c) -> asReaderT 
                          . logSuccess
                          =<< (liftE
                          . bimap (ParseErr offset . T.pack) (ParseOutput offset s) 
                          . runGet g $ c)

parseCC' :: IsCC b => Word32 -> Get b -> ParseM b
parseCC' offset g = parseCC offset g >>= \case 
  ParseOutput _ _ o -> pure o 


runParseM :: Driver -> ParseM a -> IO (Either ParseErr a)
runParseM tvar parseM = runErrors <$> runReaderT (decompose parseM) tvar
 

type RegParser a = ParseM (ParseOutput a)



data ParseF w r s m a 
  = Asks (r -> r) (r -> a)
  | State (s -> (a,s))
  | Lift (m a)
  | Return a 
  | Log (Const w a)


newtype 




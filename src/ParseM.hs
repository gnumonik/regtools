{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module ParseM where


import Data.Serialize ( bytesRead, runGet, Get ) 
import Data.Word ( Word32 ) 
import qualified Data.ByteString as BS 
import Control.Lens
    ( (^?),
      (^.),
      view,
      (#),
      over,
      set,
      makeLenses,
      Bifunctor(bimap),
      Field1(_1) ) 
import Types ( HiveCell(HiveCell), IsCC(..) ) 
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT), MonadReader(ask) )
import Control.Concurrent.STM
    ( atomically, TVar, readTVarIO, modifyTVar' )
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad (MonadPlus(mzero))
import qualified Data.Text as T
import Control.Monad.Reader.Class ( MonadReader(ask) ) 
import Control.Monad.Trans.State ( StateT )
import Control.Monad.Errors ( ErrorsT )
import Control.Monad.Errors.Class
    ( MonadErrors(report, liftE, unliftE, runE, catchE) ) 
import qualified Data.Sequence as S  
import Control.Monad.State ( MonadIO(liftIO) )

data ParseErr = ParseErr Word32 ErrorType deriving Show

data ErrorType = CellTypeMismatch 
               | SerializeError T.Text deriving Show 

type ParseErrs = S.Seq ParseErr

data ParseOutput a = ParseOutput Word32 Word32 a  

unOut :: ParseOutput a -> a
unOut (ParseOutput _ _ a) = a 

instance Functor ParseOutput where 
  fmap f (ParseOutput l s a) = ParseOutput l s (f a)

deriving instance Show a => Show (ParseOutput a)

type FunSet = (Word32 -> Bool)

data RegEnv = RegEnv {_parsed       :: M.Map Word32 HiveCell
                     ,_rawBS        :: BS.ByteString 
                     ,_parseErrs    :: ParseErrs
                     ,_offset       :: Word32 
                     ,_unparsed     :: FunSet}
makeLenses ''RegEnv 

type Driver = TVar RegEnv

type ParseM = ErrorsT ParseErrs (ReaderT Driver IO)

exclude :: (Word32,Word32) -> (Word32 -> Bool) -> (Word32 -> Bool)
exclude (start,len) old x = old x && not (x >= start && x <= start + len) 

look ::  ParseM Driver 
look = unliftE ask  

parseError :: Word32 -> T.Text -> forall a. ParseM a
parseError w t = report . S.singleton $ ParseErr w (SerializeError t)

logSuccess :: forall a. IsCC a => ParseOutput a -> ReaderT Driver IO (ParseOutput a)
logSuccess x@(ParseOutput l s a) = ask >>= \e -> do 
  liftIO . atomically . modifyTVar' e 
    $ over parsed (M.insert l $ HiveCell s (cc @a # a))
    . over unparsed (exclude (s,l))
  pure x

parseM :: Word32 -> Get a -> ParseM (ParseOutput a)
parseM offset g = look >>= (liftIO . readTVarIO) >>= \x -> 
  let c =  BS.drop (fromIntegral offset) $ x ^. rawBS 
  in  liftE . toOutput offset . runGet (getContent g) $ c 

parseM' :: Word32 -> Get b -> ErrorsT ParseErrs (ReaderT Driver IO) b
parseM' l  = fmap (\(ParseOutput _ _ a) -> a) . parseM l

parseCC :: forall a. (IsCC a, Show a)=> Word32 -> Get a -> ParseM (ParseOutput a)
parseCC off g =  look >>= (liftIO . readTVarIO) >>= \x -> 
  let checkExists = M.lookup off $ x ^. parsed 
      c =  BS.drop (fromIntegral off) $ x ^. rawBS 
  in  case checkExists of 
        Nothing ->  unliftE 
                  . logSuccess
                  =<< (logError
                  . bimap (S.singleton . ParseErr off . SerializeError . T.pack) (uncurry (ParseOutput off)) 
                  $ runGet (getContent g) c)
        Just (HiveCell s a) -> case a ^? cc @a of
          Nothing -> report . S.singleton $ ParseErr off CellTypeMismatch 
          Just ax -> pure $ ParseOutput off s ax   

logError :: Show a => Either ParseErrs a -> ParseM a
logError = \case 
  Right a -> pure a
  Left err -> look >>= \e ->  do 
    liftIO . atomically . modifyTVar' e $ over parseErrs (err <>) 
    report  err

toOutput :: forall a.  Word32 -> Either String (Word32, a) -> Either (S.Seq ParseErr) (ParseOutput a)
toOutput offset = bimap (S.singleton . ParseErr offset . SerializeError . T.pack) (uncurry (ParseOutput offset))

getContent :: forall a. Get a -> Get (Word32,a)
getContent g =  do 
  a <- g 
  bread <- fromIntegral <$> bytesRead 
  pure (bread,a )

parseCC' :: (IsCC b, Show b) => Word32 -> Get b -> ParseM b
parseCC' offset g = datastart >>= \x -> 
  parseCC offset g >>= \case 
  ParseOutput _ _ o -> pure o 

between' :: Word32 -> Word32 -> ParseM (BS.ByteString)
between' w1 w2 = look >>= (liftIO . readTVarIO) >>= \x ->
  let z = x ^. offset 
      (w1',w2') = (w1+z,w2+z) 
  in pure $ BS.take (fromIntegral w2 - fromIntegral w1) $ BS.drop (fromIntegral w1) $ x ^. rawBS 
   
runParseM :: Driver -> ParseM a -> IO (Either ParseErrs a)
runParseM tvar parseM = runReaderT (runE  parseM) tvar 

parseWhen :: Bool -> ParseM a -> ParseM a 
parseWhen b !p = if b then p else report mempty 

datastart :: ParseM Word32 
datastart = look >>= fmap (view offset) . liftIO . readTVarIO 

continueWith ::  a -> ParseM a -> ParseM a
continueWith b !m = catchE (const b) id $! m


-----------------

-- this is a sketch of a type-safe-er interface for a binary blob parser that keeps track of location
-- as it turns out i don't think we need it but i'm keeping a copy of it it here at the moment because it may be 
-- necessary for future features 

{--
newtype Loc s i = Loc {getLoc :: i} deriving (Show, Eq, Num)

loc :: forall i s. Integral i => i -> Loc s i  
loc = Loc . fromIntegral 

type Parser e s m i a = ErrorsT e ((StateT (i,BS.ByteString) m)) a

type MonadParse e m i a = ErrorsT e ((StateT (i,BS.ByteString) m)) a

git :: forall a e m i. (Integral i, Monoid e, Monad m) => Get a -> forall s. Parser [String] s m i a 
git g =  do
  (i, bs) <- get  
  case runGet g' (BS.drop (fromIntegral i) bs) of 
    Left err -> report [err] 
    Right (a,len) -> do 
      modify $ over _1 (+ len)
      pure a 
 where 
   g' :: Get (a,i)
   g' = (,) <$> g <*> (fromIntegral <$> bytesRead)


parseLoc :: forall a e m i s. (Integral i, Monoid e, Monad m) 
         => i 
         -> Parser e s m i a 
         -> MonadParse e m i a 
parseLoc i p = do 
  modify $ set _1 i
  p

parse ::  Parser e s m i a  -> MonadParse e m i a 
parse p = p 

--}
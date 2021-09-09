module Explore.ExploreM where

import Data.Word
import Control.Lens 
import Control.Lens.Action 
import Data.Kind 
import Debug.Trace 
import Control.Monad.Errors 
import Control.Monad.Errors.Class 
import Control.Monad.Look 
import Control.Monad.Reader 

import qualified Data.Vector as V 
import qualified Data.Map as M 
import qualified Data.Sequence as Seq 
import qualified Data.Text as T 
import qualified Data.ByteString as BS 

import Types 
import Lib 
import Data.Time (UTCTime)
import Text.PrettyPrint.Leijen hiding ((<$>))
import Text.Hex
import Data.List (intercalate)
import Data.ByteString.Char8 (unpack)


getPrinter :: ExploreM Printer 
getPrinter = unliftE ask >>= pure . view _2 

-- | Run a Query on the target HiveData. (Function is like this to support GHCI use)
query :: (String -> IO ()) -> HiveData -> Query a -> IO (Either QueryErrors a)
query printer reg l  = 
  runReaderT (runE $! reg ^!? l) (reg,printer) >>= \case 
    Left !errs ->  pure . Left $! errs 
    Right Nothing -> pure . Left . Seq.singleton $! QueryError 0 "Query failed to match any targets"
    Right (Just !a) -> pure . Right $! a 

query' :: (String -> IO ()) -> HiveData -> a -> MFold a b -> IO (Either QueryErrors b)
query' printer reg v l = 
  runReaderT (runE $! v ^!? l) (reg,printer) >>= \case 
    Left !errs ->  pure . Left $! errs 
    Right Nothing -> pure . Left . Seq.singleton $! QueryError 0 "Query failed to match any targets"
    Right (Just !a) -> pure . Right $! a 

-- | Makes a monadic fold. I spent a while confused by the fact that Control.Lens.Action doesn't 
--   have an equivalent of 'folding' from Control.Lens. It turns out that MonadicFolds and monadic getters 
--   have the same type modulo (iirc) an applicative constraint for Folds, so this should work
mkMonadicFold :: (s -> m a) -> MonadicFold m s a 
mkMonadicFold = act 

-- | Throw a query err. Maybe we don't need to print them as they occur but can wait til the end? 
qErr :: forall a. QueryError -> ExploreM a 
qErr e@(QueryError _ msg) = do 
  liftIO . print $ msg 
  report . Seq.singleton $ e 

optionally :: a -> ExploreM a -> ExploreM a 
optionally b !m = catchE (const b) id $! m 
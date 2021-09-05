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


makeLenses ''HiveData

type Printer = String -> IO ()

-- | Type for query errors. Most of the time the Word32 argument will be '0' (in which case it should be ignore)
--   , but occasionally we can encode the location of the error. (Yes it'd be better if it were `Maybe Word32` but 
--   it's easier to adopt this convention and since we don't do anything stupendously important with the field it shouldn't matter)
data QueryError = QueryError Word32 T.Text deriving Show 

-- | type QueryErrors = Data.Sequence.Seq QueryError 
type QueryErrors = Seq.Seq QueryError 

-- | type ExploreM = ErrorsT QueryErrors (ReaderT HiveData IO)
--   The monad in which registry queries are run. ErrorsT wrapper around a (ReaderT HiveData IO) transformer stack 
type ExploreM = ErrorsT QueryErrors (ReaderT (HiveData,Printer) IO) 

-- | As with other ErrorT stacks, local is either impossible or difficult to define; 
--   MonadLook is simply a cut-down version of MonadReader that only supports ask/asks 
instance MonadLook HiveData ExploreM where 
  look = unliftE ask >>= pure . view _1  

  looks f = f <$> look 

getPrinter :: ExploreM Printer 
getPrinter = unliftE ask >>= pure . view _2 

-- | The "Queries" that we run are really 'MonadicFold's where the monad wrapping the fold is ExploreM 
type MFold s a = MonadicFold ExploreM s a 

-- | The type of an entire well-constructed query. Admittedly this is a bit odd; Query a ~ MFold HiveData a, and our 
--   ExploreM monad wraps a ReaderT with a HiveData context. Doing it this way allows for greater parsimony in 
--   type signatures and makes the UX a bit more coherent though. (This library isn't targeted at experience Haskell devs.)
type Query a = MFold HiveData a  

-- | Not sure if anything actually uses/needs to use this. Delete if not. 
newtype NotFound = NotFound Word32 deriving (Show, Eq, Ord)

-- | Not sure if anything actually uses/needs to use this. Delete if not. 
newtype WrongType = WrongType Word32 deriving (Show, Eq, Ord)

-- | Registry Key data type. This is a "thin" representation of a Registry Key, its values, and its subkeys. 
--   Generally, a query which is intended to be used by true end users (i.e. security professionals who aren't 
--   involved in byte-level registry forrensics) ought to return this. 
--   Also, this should have the Aeson instances (even if nothing else does)
data RegistryKey  = RegistryKey {
    _keyName    :: !BS.ByteString 
  , _keyParents :: ![BS.ByteString]
  , _keyTime    :: !UTCTime
  , _keyValues  :: ![(BS.ByteString,Value) ]
  , _subkeys    :: ![RegistryKey]
} deriving (Show, Eq)

-- | Note to self, redo the Pretty class with one of the fancy pprint libs 
instance Pretty RegistryKey where 
  pretty (RegistryKey n p t v sks) = text  "| Key Name: " <+> pretty n
                                <$$> text "| Path:" <+> formatPath p 
                                <$$> text "| TimeStamp: " <+> pretty t 
                                <$$> (text "| Values:" & (\x -> if null v then x <+> text "<NONE>" else  x <$$> (vcat $  formatVals v))) 
                                <$$> (text "| Subkeys:" & (\x -> if null sks then x <+> text "<NONE>" else x <$$> indent 3 (vsep . map pretty $  sks))) 
                                <$$> text "|------"
    where 
      formatPath :: [BS.ByteString] -> Doc 
      formatPath bs = text . (<> "\\") . intercalate "\\" . map unpack  $ bs 

      formatVals :: [(BS.ByteString,Value)] -> [Doc]
      formatVals vs = flip map vs $ \(nm,vl) -> indent 2 (
                text "|-----------"
          <$$>  text "|- Value Name:" <+> pretty nm 
          <$$>  text "|- Value Data:" <+> pretty vl )


-- | Run a Query on the target HiveData. (Function is like this to support GHCI use)
query :: (String -> IO ()) -> HiveData -> Query a -> IO (Either QueryErrors a)
query printer reg l  = 
  runReaderT (runE $! reg ^!? l) (reg,printer) >>= \case 
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
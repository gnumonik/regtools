{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass, TypeFamilyDependencies #-}
module Types where

import Data.Serialize
    ( getBytes,
      getWord16le,
      getWord32be,
      getWord32le,
      putWord32be,
      putWord64le,
      runGet,
      Serialize(..),
      Get, runPut, putWord32le ) 
import Data.Word ( Word16, Word32, Word64 ) 
import qualified Data.ByteString as BS 
import GHC.TypeLits ( KnownNat, Nat, natVal ) 
import Data.Kind 
import Data.Proxy ( Proxy(Proxy) ) 
import qualified Data.Vector as V
import Control.Lens ( makePrisms, makeLenses, Prism', view, _1, _2, over, (^.) ) 
import Data.Time.Clock
import Time
import Data.List 
import Text.PrettyPrint.Leijen hiding ((<$>))
import Text.Hex 
import qualified Data.Text as T
import Data.ByteString.Encoding
import Data.Char (ord)
import qualified Data.ByteString.Char8 as BC 
import Data.Singletons.TH 
import qualified Data.Aeson as A 
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Monad.Errors
import Control.Monad.Reader
import Control.Monad.Look
import qualified Control.Monad.State.Strict as ST 
import Control.Monad.Errors.Class
import Control.Lens.Action
import Data.Function ((&))
import Data.ByteString.Char8 (unpack)
import Text.Megaparsec
import GHC.Generics
import Data.Aeson ((.=))
import qualified Data.List.NonEmpty as NE 
import Crypto.Hash.MD5 
import Data.Constraint
{-- 
Notes: 

I should split up the API into "advanced" and "basic" queries aa

--}

$(singletons [d| 
  data CCTok = SKRec 
            | NKRec 
            | VKRec 
            | SKList 
            | VList 
            | Val deriving (Show, Generic, A.ToJSON, Eq, Ord)
  |])
tshow :: Show a => a -> Doc
tshow = text . show 

t :: String -> Doc
t = text 
instance Pretty UTCTime where 
  pretty = text . prettyTime 

-- orphan instance, I know it's bad :-( 

instance A.ToJSON BS.ByteString where 
  toJSON = A.toJSON . encodeHex  

instance A.ToJSON (Bytes n) where 
  toJSON (Bytes bs) = A.toJSON bs 

{----------------------------------------------
  ---------------------------------------------      
      REGISTRY DATA REPRESENTATION TYPES 
-----------------------------------------------
-----------------------------------------------}

-- on the assumption "get" from Data.Serialize always returns `n` bytes given an argument of n, 
-- if we get `Bytes n` through the `bytes` function, we know the index correctly describes the 
-- length of the bytestring. The registry is poorly documented and the role of many fields is 
-- unknown or poorly known, so this gives us a generic representation of the fields which
-- we can convert in a type safe manner to various alternatives representation based on the index length
data Bytes :: Nat -> Type  where 
  Bytes :: KnownNat n => BS.ByteString -> Bytes n

instance Pretty Word32 where 
  pretty = pretty . show 

instance KnownNat n => Serialize (Bytes n) where 
  put (Bytes  bs) = put bs 
  get = Bytes @n <$> getBytes (fromIntegral $ natVal (Proxy @n))

instance Show (Bytes n) where 
  show (Bytes  bs) = show bs 

instance Pretty (Bytes n) where 
  pretty (Bytes bs) = pretty bs  

instance Pretty BS.ByteString where 
  pretty = text . T.unpack . encodeHex

instance Pretty a => Pretty (Either String a) where 
  pretty a = case a of 
    Left err -> text $ "Error: " <> err <> "\n\n"
    Right a  -> pretty a

-- Needs a type application
bytes :: KnownNat n => Get (Bytes n)
bytes = get 

-- these are usafe Bytes that don't come from bytes
toWord32le :: Bytes 4 -> Word32 
toWord32le (Bytes bs) = case runGet getWord32le bs of 
  Left _  -> error "invalid Bytes 4. shouldn't be possible"
  Right w -> w

toWord32be :: Bytes 4 -> Word32 
toWord32be (Bytes  bs) = case runGet getWord32be bs of 
  Left _ -> error "invalid Bytes 4. shouldn't be possible"
  Right w -> w   

toWord16le :: Bytes 2 -> Word16 
toWord16le (Bytes bs) = case runGet getWord16le bs of 
  Left _  -> error "invalid Bytes 2. shouldn't be possible"
  Right w -> w 
  
instance Pretty a => Pretty (V.Vector a) where 
  pretty = pretty . V.toList 
  
data Registry 
  = Registry { _header :: RegistryHeader 
             , _hives  :: V.Vector HiveBin} deriving Show 
instance Pretty Registry where 
  pretty r = text "Registry: "
          <$$> pretty (_header r)
          <$$> pretty (_hives r)
 
data RegistryHeader 
  = RegistryHeader {
    _hdrMagicNumber :: Bytes 4 -- string "regf"
  , _seqNum1        :: Word32 -- matches seqnum2 if hive was properly synchronized 
  , _seqNum2        :: Word32 -- matches seqnum1 if hive was properly synchronized
  , _timeStamp      :: Bytes 8 -- NT 
  , _majorVersion   :: Bytes 4 
  , _minorVersion   :: Bytes 4 
  , _unknown1       :: Bytes 4 -- maybe type
  , _unknown2       :: Bytes 4 -- format? 
  , _offset1        :: Word32 -- offset to the first key record  
  , _offset2        :: Word32 -- pointer to the start of last hbin in file 
  , _unknown3       :: Bytes 4 -- "always 1"
  , _hiveName       :: Bytes 64
  , _unknown4       :: Bytes 16 
  , _unknown5       :: Bytes 16 
  , _unknown6       :: Bytes 4 -- Maybe flags 
  , _unknown7       :: Bytes 16 
  , _unknown8       :: Bytes 4 
  , _unknown9       :: Bytes 340 -- reserved? 
  , _checksum       :: Word32 -- checksum to all data in the header up to this point 
  , _unknown10      :: Bytes 3528 
  , _unknown11      :: Bytes 16 
  , _unknown12      :: Bytes 16 
  , _unknown13      :: Bytes 16 
  , _unknown14      :: Bytes 4 
  , _unknown15      :: Bytes 4 
  } deriving (Show, Generic, A.ToJSON)

instance Pretty RegistryHeader where 
  pretty rh = text "Registry Header:"
            <$$> t "MagicNumber:" <+> (text . show $ _hdrMagicNumber rh)
            <$$> t "SeqNum1:"     <+> (text . show $ _seqNum1 rh)
            <$$> t "SeqNum2:"     <+> (text . show $ _seqNum2 rh)
            <$$> t "TimeStamp:"   <+> (text . show $ _timeStamp rh)
            <$$> t "First Key Offset:" <+> (text . show $ _offset1 rh)
            <$$> t "Last HBIN Offset:" <+> (text . show $ _offset2 rh)
            <$$> t "Hive Name(?):"     <+> (text . show $ _hiveName rh)
            <$$> t "Checksum:"         <+> (text . show $ _checksum rh)

data HiveBin = HiveBin {_binHeader ::  HiveBinHeader 
                       ,_binCells  ::  V.Vector HiveCell} deriving Show 
instance Pretty HiveBin where 
  pretty h =  t "HiveBin:" 
           <$$> pretty (_binHeader h) 
           <$$> pretty (_binCells h)

data HiveBinHeader = HiveBinHeader {
    _binMagicNumber  :: Bytes 4 
  , _offsetFromFirst :: Word32 -- This bin's distance from the first hive bin 
  , _binSize         :: Word32 -- This hive bin's size (Multiple of 4096) 
  , _unknown16       :: Bytes 16 
  , _nextHiveOffset  :: Bytes 4 -- should be same as binSize 
} deriving (Show, Generic, A.ToJSON) 

instance Pretty HiveBinHeader where 
  pretty hb = text "Hive Bin:"
           <$$> t "Magic Number:" <+> (text . show $ _binMagicNumber hb)
           <$$> t "Offset From First Bin:" <+> (text . show $ _offsetFromFirst hb)
           <$$> t "Bin size (* 4096):" <+> (text . show $ _binSize hb)
           <$$> t "Next hive offset:" <+>  (text . show $ _nextHiveOffset hb)

data HiveCell = HiveCell {
    _cellSize    :: Word32 -- cell length (including these 4 bytes)
  , _cellContent :: CellContent  
} deriving Show 

instance Pretty HiveCell where 
  pretty hc = text "Hive Cell:" 
           <$$> t "Cell Size:"    <+> (text . show $ _cellSize hc)
           <$$> t "Cell Content:" 
           <$$> pretty (_cellContent hc)  

data CellContent = SK SKRecord 
                 | NK NKRecord 
                 | VK VKRecord 
                 | Subkeylist SubkeyList 
                 | Valuelist ValueList 
                 | ValueData Value deriving Show 

instance Pretty CellContent where 
  pretty cc = case cc of 
    SK x -> pretty x 
    NK x -> pretty x 
    VK x -> pretty x 
    Subkeylist x -> pretty x 
    Valuelist x  -> pretty x 
    ValueData bs -> text . show $ bs 

data SKRecord = SKRecord {
    _skMagicNumber :: Bytes 2 -- String "sk"
  , _skUnknown     :: Bytes 2 
  , _skOffset1     :: Bytes 4 -- pointer to previous SK record 
  , _skOffset2     :: Bytes 4 -- pointer to next SK record 
  , _refCount      :: Bytes 4 -- unsigned int 
  , _secDescrSize  :: Bytes 4 -- unsigned int 
  , _secDescriptor :: BS.ByteString 
} deriving (Show, Generic, A.ToJSON)

instance Pretty SKRecord where 
  pretty sk = text "SK Record"
           <$$> t "Magic Number:" <+>  tshow (_skMagicNumber sk)
           <$$> t "Previous SK Record Offset:" <+> tshow (_skOffset1 sk)
           <$$> t "Next SK Record Offset:" <+> tshow (_skOffset2 sk)
           <$$> t "Ref Count:" <+> tshow (_refCount sk)
           <$$> t "Security Descriptor Size:" <+> tshow (_secDescrSize sk)
           <$$> t "Security Descriptor:" <+> tshow (_secDescriptor sk)

data NKRecord = NKRecord {
    _nkMagicNum        :: Bytes 2 -- string "nk"
  , _nkFlags           :: Bytes 2 -- needs more structure 
  , _nkTimeStamp       :: UTCTime
  , _nkUnknown1        :: Bytes 4 
  , _nkOffset1         :: Word32 -- Parent NK record 
  , _stableSubkeys     :: Word32-- set to 0 on deletion  
  , _unstableSubkeys   :: Word32 
  , _stableSubkeyPtr   :: Word32 -- pointer to the subkey list 
  , _unstableSubkeyPtr :: Word32 
  , _numValues         :: Word32
  , _valueListPtr      :: Word32 -- pointer to the value list 
  , _skPtr             :: Word32 -- pointer to SK record  
  , _classNamePtr      :: Word32 -- Pointer to class name 
  , _maxBytesSubkey    :: Word32 
  , _maxLengthSubkey   :: Word32 
  , _maxValueNm        :: Word32 -- maximum bytes in a value name 
  , _maxValSize        :: Word32 -- maximum vaalue data size 
  , _nkUnknown2        :: Bytes 4 -- possibly some kind of runtime index 
  , _keyNameLength     :: Word16 -- length of key name 
  , _classNameLength   :: Word16 -- length of class name 
  , _keyString         :: BS.ByteString -- Key name. Stored in ASCII. Typically null-terminated.
} deriving (Show, Generic, A.ToJSON)

instance Pretty NKRecord where 
  pretty nk = text "NKRecord: "
            <$$> t "Magic Num:" <+> tshow (_nkMagicNum nk)
            <$$> t "Flags:" <+> tshow (_nkFlags nk)
            <$$> t "TimeStamp:" <+> pretty (_nkTimeStamp nk)
            <$$> t "Offset to Parent:" <+> tshow (_nkOffset1 nk)
            <$$> t "Number of Subkeys (Stable):" <+> tshow (_stableSubkeys nk)
            <$$> t "Number of Subkeys (Volatile):" <+> tshow (_unstableSubkeys nk)
            <$$> t "Subkeys Pointer (Stable):" <+> tshow (_stableSubkeyPtr nk)
            <$$> t "Subkeys Pointer (Volatile):" <+> tshow (_unstableSubkeyPtr nk)
            <$$> t "Number of Values:" <+> tshow (_numValues nk)
            <$$> t "Pointer to Value List:" <+> tshow (_numValues nk)
            <$$> t "Pointer to SK Record:" <+> tshow (_skPtr nk)
            <$$> t "Class Name Pointer:" <+> tshow (_classNamePtr nk)
            <$$> t "Key Name Length:" <+> tshow (_keyNameLength nk)
            <$$> t "Class Name Length:" <+> tshow (_classNameLength nk)
            <$$> t "Key String: " <+> tshow (_keyString nk)
         
data VKRecord = VKRecord {
    _vkMagicNum    :: Bytes 2 
  , _valNameLength :: Word16 
  , _dataLength    :: Word32 
  , _dataPtr       :: Word32 -- Pointer to Data 
  , _valueType     :: Word32 
  , _nameFlags     :: Bytes 2 -- "If the 0 bit is set, the value name is in ascii, otherwise utf16le"
  , _vkUnknown     :: Bytes 2 
  , _valName       :: BS.ByteString -- Value name. Stored in ascii and typically nul-terminated 
  , _value         :: Value
} deriving (Show, Generic, A.ToJSON)

instance Pretty VKRecord where 
  pretty vk = t "VK Record: "
           <$$> t "Magic Number:"      <+> tshow (_vkMagicNum vk)
           <$$> t "Value Name Length:" <+> tshow (_valNameLength vk)
           <$$> t "Data Length:"       <+> tshow (_dataLength vk)
           <$$> t "Data Pointer:"      <+> tshow (_dataPtr vk)
           <$$> t "Value Type:"        <+> tshow (_valueType vk)
           <$$> t "Name Flags:"        <+> tshow (_nameFlags vk)
           <$$> t "Value Name:"        <+> tshow (_valName vk)
           <$$> t "Value: "            <+> pretty (_value vk)
          
type ValueList = V.Vector Word32-- i think? 

data SubkeyList = SubkeyList {
    _subkeyListMagicNum :: Bytes 2 -- Magic number "lf" "lh" "ri" or "li"
  , _numElems           :: Word16 
  , _subkeyElems        :: V.Vector SubkeyElem 
} deriving (Show, Generic, A.ToJSON)

instance Pretty SubkeyList where 
  pretty skl = text "Subkey List: "
            <$$> t "Magic Number:" <+> tshow (_subkeyListMagicNum skl)
            <$$> t "NumElems:"     <+> tshow (_numElems skl)
            <$$> t "Subkeylist:"   <+> tshow (_subkeyElems skl)

data SubkeyElem = Ri Word32-- pointer to another subkey LIST  
                | Li Word32 -- pointer to a subkey 
                | Lf Word32 Word32 -- first arg is a pointer to an NK record, second is a hash value (computed differently for Lf and Lh)
                | Lh Word32 Word32 -- first arg is a pointer to an NK record, second is a hash value (computed differently for Lf and Lh) deriving Show 
  deriving (Show, Generic, A.ToJSON)

data Value =     REG_NONE BS.ByteString -- 0x0
               | REG_SZ BS.ByteString --  0x1 UTF-16 little-endian string 
               | REG_EXPAND_SZ BS.ByteString -- 0x2 UTF-16 little-endian string w/ system path variable (e.g. "%SYSTEMROOT%") escapes 
               | REG_BINARY BS.ByteString -- 0x3 
               | REG_DWORD Word32 -- 0x4 Word32, Little Endian 
               | REG_DWORD_LITTLE_ENDIAN Word32 -- 0x4
               | REG_DWORD_BIG_ENDIAN Word32 -- 0x5 Word32, Big Endian 
               | REG_LINK BS.ByteString -- 0x6 Symbolic link, stored as a UTF-16le string 
               | REG_MULTI_SZ BS.ByteString -- 0x7 A List of UTF-16le strings. Each string is NUL ("\x00\x00") terminated and the list itself is NUL terminated, resulting in a total of four 0-bytes at the end 
               | REG_RESOURCE_LIST BS.ByteString -- 0x8 "A series of nested arrays (unknown format)"
               | REG_FULL_RESOURCE_DESCRIPTOR BS.ByteString  -- 0x9 "A series of nested arrays (unknown format)"
               | REG_RESOURCE_REQUIREMENTS_LIST BS.ByteString -- 0xA "A series of nested arrays (unknown format)"
               | REG_QWORD Word64 -- 0xB Little Endian Word64
  deriving (Show, Generic, Eq)

instance A.ToJSON Value where 
  toJSON = \case 
    REG_NONE bs                       -> f "REG_NONE" bs 

    REG_SZ bs                         -> u16 "REG_SZ" bs -- this is a string 4 realz 

    REG_EXPAND_SZ bs                  -> u16 "REG_EXPAND_SZ" bs 

    REG_BINARY bs                     -> f "REG_BINARY" bs 

    REG_DWORD w                       -> A.object ["REG_DWORD" .= (T.pack $ show w)]

    REG_DWORD_LITTLE_ENDIAN w         -> A.object ["REG_DWORD_LITTLE_ENDIAN" .= (T.pack $ show w)]

    REG_DWORD_BIG_ENDIAN w            -> A.object ["REG_DWORD_BIG_ENDIAN" .= (T.pack $ show w)]

    REG_LINK bs                       -> u16 "REG_LINK" bs 

    REG_MULTI_SZ bs                   -> f "REG_MULTI_SZ" bs 

    REG_RESOURCE_LIST bs              -> f "REG_RESOURCE_LIST" bs -- Need to make the list of strings parser

    REG_FULL_RESOURCE_DESCRIPTOR bs   -> f "REG_FULL_RESOURCE_DESCRIPTOR" bs 

    REG_RESOURCE_REQUIREMENTS_LIST bs -> f "REG_RESOURCE_REQUIREMENTS_LIST" bs  

    REG_QWORD w                       -> A.object ["REG_QWORD" .= (T.pack $ show w)]
   where 
     bsToUTF16le bs = decode utf16le bs 

     u16 :: T.Text -> BS.ByteString -> A.Value 
     u16 c b = A.object [c .= (bsToUTF16le b)]

     f :: T.Text -> BS.ByteString -> A.Value 
     f c b = A.object [c .= (encodeHex b)]
-- wtb generics.sop but i dont want the dependency 
instance Pretty Value where 
  pretty = \case 
    REG_NONE bs                       -> f "REG_NONE" bs 
    REG_SZ bs                         -> u16 "REG_SZ" bs -- this is a string 4 realz 
    REG_EXPAND_SZ bs                  -> u16 "REG_EXPAND_SZ" bs 
    REG_BINARY bs                     -> f "REG_BINARY" bs 
    REG_DWORD w                       -> t "REG_DWORD" <+> brackets (tshow w)
    REG_DWORD_LITTLE_ENDIAN w         -> t "REG_DWORD_LITTLE_ENDIAN" <+> brackets (tshow w)
    REG_DWORD_BIG_ENDIAN w            -> t "REG_DWORD_BIG_ENDIAN" <+> brackets (tshow w)
    REG_LINK bs                       -> u16 "REG_LINK" bs 
    REG_MULTI_SZ bs                   -> f "REG_MULTI_SZ" bs 
    REG_RESOURCE_LIST bs              -> f "REG_RESOURCE_LIST" bs -- Need to make the list of strings parser
    REG_FULL_RESOURCE_DESCRIPTOR bs   -> f "REG_FULL_RESOURCE_DESCRIPTOR" bs 
    REG_RESOURCE_REQUIREMENTS_LIST bs -> f "REG_RESOURCE_REQUIREMENTS_LIST" bs  
    REG_QWORD w                       -> t "REG_QWORD" <+> brackets (tshow w)
   where 
     bsToUTF16le bs = T.unpack $ decode utf16le bs 

     u16 :: String -> BS.ByteString -> Doc 
     u16 c b = t c <+> brackets (t . bsToUTF16le $ b)

     f :: String -> BS.ByteString -> Doc 
     f c b =  t c <+> brackets (t . T.unpack $  encodeHex b) 

makePrisms ''CellContent
makeLenses ''RegistryHeader 
makeLenses ''HiveBin 
makeLenses ''HiveBinHeader 
makeLenses ''HiveCell 
makeLenses ''SKRecord 
makeLenses ''NKRecord 
makeLenses ''VKRecord 
makeLenses ''SubkeyList 
makePrisms ''SubkeyElem 
makePrisms ''Value

class IsCC a where 
  cc :: Prism' CellContent a 
  cTok :: CCTok 

instance IsCC SKRecord where 
  cc = _SK 
  cTok = SKRec 

instance IsCC NKRecord where 
  cc = _NK 
  cTok = NKRec 

instance IsCC VKRecord where 
  cc = _VK 
  cTok = VKRec 

instance IsCC SubkeyList where 
  cc = _Subkeylist 
  cTok = SKList 

instance IsCC ValueList where 
  cc = _Valuelist
  cTok = VList  

instance IsCC Value where 
  cc = _ValueData  
  cTok = Val 

{----------------------------------------------
  ---------------------------------------------      
      REGISTRY DESERIALIZATION PARSER TYPES 
-----------------------------------------------
-----------------------------------------------}

data ParseErr = ParseErr Word32 CCTok ErrorType deriving Show

data ErrorType = CellTypeMismatch 
               | SerializeError T.Text deriving Show 

type ParseErrs = S.Seq ParseErr

data ParseOutput a = ParseOutput Word32 Word32 a  

-- | Used to calculate "dead zones" in the registry that no known node 
--   points to. Potentially useful for forensics. 
newtype OccupiedSpace = OccupiedSpace {getSpace :: M.Map Word32 Word32} deriving (Show, Generic, A.ToJSON, Eq)

instance Functor ParseOutput where 
  fmap f (ParseOutput l s a) = ParseOutput l s (f a)

deriving instance Show a => Show (ParseOutput a)

type FunSet = (Word32 -> Bool)

data RegEnv = RegEnv {_parsed       :: M.Map Word32 HiveCell
                     ,_rawBS        :: BS.ByteString 
                     ,_parseErrs    :: ParseErrs
                     ,_offset       :: Word32 
                     ,_spaceMap     :: OccupiedSpace}
makeLenses ''RegEnv 

type Driver = TVar RegEnv

type ParseM = ErrorsT ParseErrs (ReaderT Driver IO)

instance MonadLook Driver (ErrorsT ParseErrs (ReaderT Driver IO)) where 
  look = unliftE ask 

  looks f = f <$> (unliftE ask)

data HiveData = HiveData {_rHeader :: RegistryHeader 
                         ,_hHeader :: HiveBinHeader 
                         ,_hEnv    :: RegEnv} 
instance Pretty HiveData where 
  pretty (HiveData rH hH hEnv) 
    =       pretty rH
      <$$>  pretty hH 
      <$$>  text "Hive Data Size (bytes):" <+> (text . show . BS.length $ _rawBS hEnv)
      <$$>  text "Number of Cells:" <+> (text . show . M.size $ _parsed hEnv)

instance A.ToJSON HiveData where 
  toJSON hd = A.object [ "regheader"  .= A.toJSON (_rHeader hd)
                       , "hiveheader" .= A.toJSON (_hHeader hd)]
type Offset = Word32 

type Vec = V.Vector 

{----------------------------------------------
  ---------------------------------------------      
      REGISTRY QUERY/OPTIC TYPES 
-----------------------------------------------
-----------------------------------------------}

makeLenses ''HiveData

type Printer = String -> IO ()

-- | Type for query errors. Most of the time the Word32 argument will be '0' (in which case it should be ignored)
--   , but occasionally we can encode the location of the error. (Yes it'd be better if it were `Maybe Word32` but 
--   it's easier to adopt this convention and since we don't do anything stupendously important with the field it shouldn't matter)
data QueryError = QueryError Word32 T.Text deriving Show 

-- | type QueryErrors = Data.Sequence.Seq QueryError 
type QueryErrors = S.Seq QueryError 

-- | type ExploreM = ErrorsT QueryErrors (ReaderT HiveData IO)
--   The monad in which registry queries are run. ErrorsT wrapper around a (ReaderT HiveData IO) transformer stack 
type ExploreM = ErrorsT QueryErrors (ReaderT (HiveData,Printer) IO) 

-- | As with other ErrorT stacks, local is either impossible or difficult to define; 
--   MonadLook is simply a cut-down version of MonadReader that only supports ask/asks 
instance MonadLook HiveData ExploreM where 
  look = unliftE ask >>= pure . view _1  

  looks f = f <$> look 

-- | The "Queries" that we run are really 'MonadicFold's where the monad wrapping the fold is ExploreM 
type MFold s a = MonadicFold ExploreM s a 

-- | Monadic Predicate
type MPRed a = a -> ExploreM Bool

-- | The type of an entire well-constructed query. Admittedly this is a bit odd; Query a ~ MFold HiveData a, and our 
--   ExploreM monad wraps a ReaderT with a HiveData context. Doing it this way allows for greater parsimony in 
--   type signatures and makes the UX a bit more coherent though. (This library isn't targeted at experience Haskell devs.)
type Query a = MFold HiveData a  

-- | Not sure if anything actually uses/needs to use this. Delete if not. 
newtype NotFound = NotFound Word32 deriving (Show, Generic, A.ToJSON, Eq, Ord)

-- | Not sure if anything actually uses/needs to use this. Delete if not. 
newtype WrongType = WrongType Word32 deriving (Show, Generic, A.ToJSON, Eq, Ord)

data SubkeyData = Truncated | Empty | Subkeys (NE.NonEmpty RegistryKey)
  deriving (Show, Generic, A.ToJSON)

-- | "Fully Qualified" Registry Value. Contains the path to the value's parent and the VK record
data FQValue = FQValue {_fqValName :: BS.ByteString 
                       ,_fqValData :: Value 
                       ,_fqValPath :: BS.ByteString 
                       ,_fqVK      :: VKRecord} deriving (Show)

instance A.ToJSON FQValue where 
  toJSON fqv = A.object ["Val Name" .= A.toJSON (BC.unpack $ _fqValName fqv)
                        ,"Val Path" .= A.toJSON (BC.unpack $ _fqValPath fqv) 
                        ,"Val Data" .= A.toJSON (_fqValData fqv)]
instance Pretty FQValue 
 where 
  pretty (FQValue nm dat pth _) 
    =       text "|-----------"
      <$$>  text "|- Value Path:" <+> text (BC.unpack pth)
      <$$>  text "|- Value Name:" <+> text (BC.unpack nm) 
      <$$>  text "|- Value Data:" <+> pretty dat 

data ValHash = ValHash {_vHashName :: T.Text 
                       ,_vHashPath :: T.Text 
                       ,_vHashData :: T.Text} deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

-- | Registry Key data type. This is a "thin" representation of a Registry Key, its values, and its subkeys. 
--   Generally, a query which is intended to be used by true end users (i.e. security professionals who aren't 
--   involved in byte-level registry forrensics) ought to return this. 
--   Also, this should have the Aeson instances (even if nothing else does)
data RegistryKey  = RegistryKey {
    _keyName    :: !BS.ByteString 
  , _keyParents :: ![BS.ByteString]
  , _keyTime    :: !UTCTime
  , _keyValues  :: ![FQValue]
  , _subkeys    :: !SubkeyData
  , _keyNode    :: !NKRecord 
} deriving (Show, Generic)

data KeyHash = KeyHash {
    _hshKeyName   :: !T.Text
  , _timeHash     :: !T.Text
  , _valuesHash   :: ![ValHash]
} deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)


data KeyHashFileObj = OneKey KeyHash 
                    | ManyKeys [KeyHash]
                    | ManyVals [ValHash]
  deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)

instance A.ToJSON RegistryKey where 
  toJSON (RegistryKey kName kParents kTime kVals subKS _) 
    = A.object ["Key Name"   .= A.toJSON (T.pack . BC.unpack $ kName)
                ,"Path"      .= A.toJSON (formatPath_ kParents)
                ,"TimeStamp" .= A.toJSON (prettyTime kTime)
                ,"Values"    .= A.toJSON kVals 
                ,"Subkeys"   .= A.toJSON subKS]
   where 
     formatPath_ bs = {-- (<> "\\") . --} intercalate "\\" . map unpack  $ bs 

instance Pretty RegistryKey where 
  pretty (RegistryKey n p t v sks _) =  text "" 
                                <$$>  text "| Key Name:"   <+> text (BC.unpack n)
                                <$$>  text "| Path:"       <+> formatPath p 
                                <$$>  text "| TimeStamp: " <+> pretty t 
                                <$$> (text "| Values:" & (\x -> if null  v 
                                                                then x <+> text "<NONE>" 
                                                                else  x <$$> vsep (formatVals v))) 
                                <$$> (text "| Subkeys:" & (\x -> case sks of
                                                                  Empty      -> x <+> text "<NONE>" 
                                                                  Truncated  -> x <+> text "<TRUNCATED>"
                                                                  Subkeys ne -> x <$$> indent 3 (vsep . map pretty . NE.toList $  ne))) 
                                <$$> text "|------"

formatPath :: [BS.ByteString] -> Doc 
formatPath bs = text . (<> "\\") . intercalate "\\" . map unpack  $ (reverse bs) 

formatVals :: [FQValue] -> [Doc]
formatVals fqvs = go fqvs' 
 where 
  fqvs' = map (\x -> (_fqValName x, _fqValData x)) fqvs
  
  go :: [(BS.ByteString,Value)] -> [Doc]
  go vs = flip map vs $ \(nm,vl) -> indent 2 (
            text "|-----------"
      <$$>  text "|- Value Name:" <+> text (BC.unpack nm)
      <$$>  text "|- Value Data:" <+> pretty vl )

{----------------------------------------------
  ---------------------------------------------      
      DSL LEXER TYPES
-----------------------------------------------
-----------------------------------------------}

type Lexer = Parsec Void T.Text Tok 

type Lexer_ = Parsec Void T.Text ()

-- | A DSL Lexeme. 
data Tok 
  = Query 
  | Let 
  | Equal
  | Pipe 
  | LParen
  | If 
  | Then 
  | Else 
  | RParen 
  | LCurly 
  | RCurly 
  | Plugin
  | ConcatTok 
  | AppendTok 
  | IsEmptyTok 
  | CmdTok CmdToken 
  | QBTok QBToken 
  | IntLike Int 
  | LitString T.Text 
  | Name T.Text deriving (Show, Generic, A.ToJSON, Eq, Ord)

instance VisualStream [Tok] where 
  showTokens _ t = show t 

instance TraversableStream [Tok] where 
  reachOffsetNoLine n (PosState {..}) = newPosState
    where 
      newPosState = PosState { pstateInput = drop n pstateInput 
                             , pstateOffset = pstateOffset + n 
                             , pstateSourcePos = let oldCol = sourceColumn pstateSourcePos 
                                                 in pstateSourcePos {sourceColumn = mkPos $ unPos oldCol + n}
                             ,..}

-- | Tokens for DSL Commands 
data CmdToken 
  = HelpTok 
  | LoadTok 
  | RunTok
  | ExitTok 
  | WriteJSONTok 
  | PrintTok 
  | WriteHashTok 
  | PrintStrTok
  | CheckHashTok 
  | ShowTypeTok  deriving (Show, Generic, A.ToJSON, Eq, Ord) 

-- | Tokens for query builders 
data QBToken 
  = Root
  | SubKeys  
  | KeyPath 
  | MatchName 
  | MatchValName 
  | MatchValData
  | Expand
  | Vals
  | Map 
  | Select 
  | ConcatMap deriving (Show, Generic, A.ToJSON, Eq, Ord)

{----------------------------------------------
 ----------------------------------------------

               "MAGIC"/DSL PARSER TYPES 

  (Lets us parse things in a quasi-dependently-typed manner)

-----------------------------------------------
-----------------------------------------------}

-- Typelevel magic. There are two ways we can parse a sequence of lenses: 
--    1) A massive amount of unreadable TH + ridiculous typeclass sorcery + probably some unsafecoerce 
--    2) Fake dependent types. 
-- We're gonna go with 2 here cuz I do not feel like pouring hover th splice output 
-- (I went with 1 on a different project. It was a baaaaad time.)
$(singletons [d| 
  data DSLType 
    = ROOT 
    | REGKEY 
    | VAL
    | LIST DSLType
    | BOOL
    | EFFECT 
       deriving (Show, Generic, A.ToJSON, Eq)
 |])

assertPretty :: forall (a :: DSLType)
              . Sing a -> Dict (PrettyRefl a)
assertPretty = \case 
  SROOT -> Dict 
  SREGKEY -> Dict 
  SVAL    -> Dict 
  SLIST a -> case assertPretty a of 
    Dict  -> Dict 
  SBOOL   -> Dict  
  SEFFECT -> Dict           

type DSLToHask :: DSLType -> Type 
type family DSLToHask x = y | y -> x  where 
  DSLToHask 'VAL        = FQValue
  DSLToHask 'ROOT       = HiveData 
  DSLToHask 'REGKEY     = RegistryKey 
  DSLToHask ('LIST a)   = [DSLToHask a]
  DSLToHask 'BOOL       = Bool
  DSLToHask 'EFFECT     = ()
  
data SomeQB :: Type where 
  SomeQB :: QueryBuilder a b -> SomeQB 

type PrettyRefl a = (Pretty (DSLToHask a), A.ToJSON (DSLToHask a))

-- | Builds queries. 
--   The constraints are stupid and only exist to 
--   let us derive a PrettyRefl dictionary for the return type. 
--   We could just pass around dictionaries or match on the constructors 
--   but doing it this way saves us space in a few ways. 

type JSONTag = T.Text

data QueryBuilder :: DSLType -> DSLType -> Type where 
  -- Need a "filterValues" function

  VALS          :: PrettyRefl ('LIST 'VAL)   => QueryBuilder 'REGKEY ('LIST 'VAL) 

  ROOTCELL      :: PrettyRefl 'REGKEY => QueryBuilder 'ROOT 'REGKEY 

  EXPAND        :: PrettyRefl 'REGKEY => Maybe Word32 -> QueryBuilder 'REGKEY 'REGKEY 

  SUBKEYS       :: PrettyRefl ('LIST 'REGKEY) => QueryBuilder 'REGKEY ('LIST 'REGKEY)

  KEYPATH       :: PrettyRefl ('LIST 'REGKEY) => [String] -> QueryBuilder 'REGKEY ('LIST 'REGKEY)

  MATCHKEYNAME  :: PrettyRefl 'BOOL => BS.ByteString -> QueryBuilder 'REGKEY 'BOOL 

  MATCHVALNAME  :: PrettyRefl 'BOOL => BS.ByteString -> QueryBuilder 'VAL 'BOOL 

  MATCHVALDATA  :: PrettyRefl 'BOOL 
                => T.Text -- the encoding varies based on the value type
                -> QueryBuilder 'VAL 'BOOL 

  MAP           :: (PrettyRefl ('LIST b), PrettyRefl a)
                => QueryBuilder a b 
                -> QueryBuilder ('LIST a) ('LIST b)

  SELECT        :: (PrettyRefl a)
                => QueryBuilder a 'BOOL
                -> QueryBuilder ('LIST a) ('LIST a)

  CONCATMAP     :: (PrettyRefl a, PrettyRefl ('LIST b))
                => QueryBuilder a ('LIST b) 
                -> QueryBuilder ('LIST a) ('LIST b)

  COMPOSED      :: (PrettyRefl b)
                => CompositeQB a b 
                -> QueryBuilder a b 

type QBConstraint a = (SingI a, PrettyRefl a)

-- As it turns out, we could replace the 'Focus' GADT with this, but 
-- it'd require redoing a decent amount of the obscure type level magic, 
-- and I don't wanna do that atm cuz it's hard 
data CompositeQB :: DSLType -> DSLType -> Type where 
  QBZ :: (PrettyRefl a, PrettyRefl b, SingI a, SingI b) 
      => QueryBuilder a b -> CompositeQB a b 
  QBS :: (PrettyRefl c, SingI b, SingI c)
      => CompositeQB a b -> QueryBuilder b c -> CompositeQB a c 

-- | This isn't *exactly* a Bazaar/Context from Control.Lens but it 
--   it sorta serves the same purpose. Kinda. By analogy. 
data Focus :: DSLType -> Type where 
  FocusZ  :: QBConstraint a 
          =>  QueryBuilder 'ROOT a
          ->  Focus a 
  FocusS :: QBConstraint a => Focus t -> QueryBuilder t a -> Focus a 

-- | We need this to recover type information (and to avoid 
--   a big ole mess of 'BRO UR TYPES R IMPREDICATIVE' errors)
data BoxedMFold :: Type -> Type -> Type where 
  MkBoxedMFold ::  MFold a b -> BoxedMFold a b

-- | Type environment for type checking 
data Context = Context {_tyCxt :: M.Map T.Text (Some TypeDict)
                       ,_plCxt :: M.Map T.Text FilePath} 
emptyContext = Context M.empty M.empty 

-- | Typed Variables 
data TypedVar :: DSLType -> Type where 
  MkTypedVar :: SingI (a :: DSLType) => T.Text -> TypedVar a 

-- | Singleton with a constraint 
data TypeDict :: DSLType -> Type where 
  MkTypeDict :: PrettyRefl a =>  Sing a -> TypeDict a 

class DSLHashable (a :: DSLType) where 
  mkHash :: DSLToHask a -> KeyHashFileObj

-- | Commands
data DSLCommand :: Type where 
  EXIT      :: DSLCommand 
  HELP      :: DSLCommand 
  PPRINT    :: DSLExp a -> DSLCommand
  PRINTSTR  :: T.Text -> DSLCommand 
  WRITEJSON :: DSLExp a -> FilePath -> Maybe T.Text -> DSLCommand
  HASH      :: Dict (DSLHashable a) -> DSLExp a -> FilePath -> DSLCommand  
  SHOWTYPE  :: DSLExp a -> DSLCommand
  CHKHASH   :: FilePath -> DSLCommand 

-- | DSL Expressions
data DSLExp :: DSLType -> Type where
  DSLVar    :: TypedVar a -> DSLExp a 

  RootQuery :: Focus a -> DSLExp a

  VarQuery  :: TypedVar a -> QueryBuilder a b -> DSLExp b 

  Assign    :: DSLExp a -> TypedVar a -> DSLExp a

  IfThen    :: DSLExp 'BOOL -> DSLExp a -> DSLExp a -> DSLExp a 

  Append    :: DSLExp ('LIST a) -> DSLExp ('LIST a) -> DSLExp ('LIST a)

  Concat    :: DSLExp ('LIST ('LIST a)) -> DSLExp ('LIST a)

  IsEmpty   :: DSLExp ('LIST a) -> DSLExp 'BOOL 

  Command   :: DSLCommand -> DSLExp 'EFFECT 

-- | For when you have a thing but GHC won't let you know what it is that you have 
data Some :: (k -> Type) -> Type where 
  Some :: forall k (a :: k) (c :: k -> Type)
        . c a -> Some c 

type DSLParser a = ParsecT Void [Tok] (ST.State Context) a  

makeLenses ''RegistryKey

makeLenses ''KeyHash

makeLenses ''Context

makeLenses ''FQValue

makeLenses ''ValHash 
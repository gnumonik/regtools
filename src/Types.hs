module Types where

import Data.Serialize
    ( getBytes,
      getWord16le,
      getWord32be,
      getWord32le,
      runGet,
      Serialize(..),
      Get ) 
import Data.Word ( Word16, Word32, Word64 ) 
import qualified Data.ByteString as BS 
import GHC.TypeLits ( KnownNat, Nat, natVal ) 
import Data.Kind ( Type ) 
import Data.Proxy ( Proxy(Proxy) ) 
import qualified Data.Vector as V
import Control.Lens ( makePrisms, makeLenses, Prism' ) 
import Data.Time.Clock
import Time
import Data.List 
import Text.PrettyPrint.Leijen hiding ((<$>))
import Text.Hex 
import qualified Data.Text as T
import Data.ByteString.Encoding
import Data.Char (ord)
import qualified Data.ByteString.Char8 as BC 

tshow :: Show a => a -> Doc
tshow = text . show 

t :: String -> Doc
t = text 
instance Pretty UTCTime where 
  pretty = text . prettyTime 

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
  pretty = pretty . show 

instance Pretty BS.ByteString where 
  pretty = text . BC.unpack

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
  } deriving Show 

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
} deriving Show 

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
} deriving Show 

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
} deriving Show 

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
} deriving Show 

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
} deriving Show 

instance Pretty SubkeyList where 
  pretty skl = text "Subkey List: "
            <$$> t "Magic Number:" <+> tshow (_subkeyListMagicNum skl)
            <$$> t "NumElems:"     <+> tshow (_numElems skl)
            <$$> t "Subkeylist:"   <+> tshow (_subkeyElems skl)

data SubkeyElem = Ri Word32-- pointer to another subkey LIST  
                | Li Word32 -- pointer to a subkey 
                | Lf Word32 Word32 -- first arg is a pointer to an NK record, second is a hash value (computed differently for Lf and Lh)
                | Lh Word32 Word32 -- first arg is a pointer to an NK record, second is a hash value (computed differently for Lf and Lh) deriving Show 
  deriving Show 

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
  deriving (Show, Eq)

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

data CCTok = SKRec 
           | NKRec 
           | VKRec 
           | SKList 
           | VList 
           | Val deriving (Show, Eq)

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



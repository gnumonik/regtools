{-# LANGUAGE TupleSections #-}
module Lib where

import Data.Serialize 
import Data.Serialize.Get
import Data.Word 
import qualified Data.ByteString as BS 
import GHC.TypeLits 
import Data.Kind 
import Data.Proxy 
import System.IO 
import qualified Data.Vector as V 
import Control.Applicative.Combinators 
import Types
import Control.Monad
import Data.Functor ((<&>))
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Data.ByteString.Char8
import Control.Monad.Trans.Class 
import Debug.Trace 
type Parser st a = ReaderT st Get a 

newtype RegistryBody = RegistryBody BS.ByteString -- offsets start from the first hbin, this is the regristry minus the header 

type RegParser a = Parser BS.ByteString a 



w32 :: Get Word32
w32 = getWord32le 

w16 :: Get Word16 
w16 = getWord16le 

parseWith :: Parser st a -> st -> BS.ByteString -> Either String a 
parseWith p st = runGet (runReaderT p st)
                  

parseSlice :: Word32 -> Word32 -> Get a -> RegParser a 
parseSlice offset len pst = asks  (BS.take (fromIntegral len) . BS.drop (fromIntegral offset +4)) >>= \chonk ->  
  case runGet pst chonk of 
    Left err -> fail err 
    Right res -> pure res 

dropParse :: Word32 -> Get a ->  RegParser a 
dropParse offset pst = asks (BS.drop $ fromIntegral offset + 4)  >>= \chonk ->  case runGet pst chonk of 
    Left err -> fail err 
    Right res -> pure res 

testhivepath :: FilePath 
testhivepath = "/home/gsh/Downloads/SeanStuff/hkeyclassesroot"

testParseHeader :: IO ()
testParseHeader = BS.readFile testhivepath >>= \bs -> Prelude.putStr . pretty $ runGet registry bs 



registry :: Get Registry 
registry = do 
  hdr <- registryHeader 
  hive <- V.fromList <$> some hiveBin 
  pure $ Registry hdr hive 



registryHeader :: Get RegistryHeader 
registryHeader = trace "\nregistryHeader\n" $ do -- intentionally not doing applicative style here cuz i'm sure i'll make a mistake i'll never catch  
  magicNumber   <- bytes @4 
  seqNum1       <- w32
  seqNum2       <- w32
  timeStamp     <- bytes @8 
  majorVersion  <- bytes @4  
  minorVersion  <- bytes @4  
  unknown1      <- bytes @4  
  unknown2      <- bytes @4  
  offset1       <- bytes @4  
  offset2       <- w32  
  unknown3      <- bytes @4  
  hiveName      <- bytes @64
  unknown4      <- bytes @16
  unknown5      <- bytes @16
  unknown6      <- bytes @4  
  unknown7      <- bytes @16 
  unknown8      <- bytes @4 
  unknown9      <- bytes @340 
  checksum      <- w32
  unknown10     <- bytes @3528 
  unknown11     <- bytes @16 
  unknown12     <- bytes @16 
  unknown13     <- bytes @16 
  unknown14     <- bytes @4 
  unknown15     <- bytes @4 
  pure $ RegistryHeader magicNumber 
                        seqNum1 
                        seqNum2 
                        timeStamp 
                        majorVersion 
                        minorVersion 
                        unknown1 
                        unknown2 
                        (toWord32le offset1) 
                        offset2 
                        unknown3 
                        hiveName 
                        unknown4 
                        unknown5 
                        unknown6 
                        unknown7 
                        unknown8 
                        unknown9
                        checksum 
                        unknown10 
                        unknown11 
                        unknown12
                        unknown13
                        unknown14 
                        unknown15 


hiveBin :: Get HiveBin 
hiveBin  = trace "hivebin" $ do 
  rest <- lookAhead allRemainingBytes
  magicNum       <- bytes @4 
  offset         <- w32
  binsize        <- w32
  unknown        <- bytes @16 
  nextHiveOffset <- bytes @4
  cells <- runReaderT (some hiveCell) rest  
  let result = HiveBin magicNum offset binsize unknown nextHiveOffset (V.fromList cells) 
  trace (pretty result) $ pure result 

hiveSize :: Get Int 
hiveSize = lookAhead $ do -- make this shorter once i know it works 
  _magicNum       <- bytes @4 
  _offset         <- bytes @4 
  binsize        <- bytes @4 
  pure . (* 4096) . fromIntegral . toWord32le $ binsize 

hiveCell :: RegParser HiveCell 
hiveCell = trace "hiveCell" $ do 
  size <- lift w32
  content <- cellContent
  let result = HiveCell size content  
  trace (pretty result) $ pure result 
  
cellContent :: RegParser CellContent 
cellContent =  trace "cellContent" $ do 
  (Bytes magicN) <- lift . lookAhead $ bytes @2 
  case unpack magicN of 
    "sk" -> SK <$> skRecord
    "nk" -> NK <$> nkRecord
    "vk" -> VK <$> vkRecord 
    other -> let subkeylists = ["ri","li","lh","lf"]
             in if other `Prelude.elem` subkeylists 
                then  Subkeylist <$> lift subkeyList 
                else fail $ "cellContent failure\nparsed magic number: " <> unpack magicN  --choice [ Valuelist <$> valueList 
                       --     , RawDataBlocks <$> rawDataBlocks] -- this is 100% wrong but i can't fix it til i know more about how these things point to each other 


skRecord :: RegParser SKRecord 
skRecord = do 
  magicNum <- lift $ bytes @2 
  unknown  <- lift $ bytes @2 
  offset1  <- lift $ bytes @4 
  offset2  <- lift $ bytes @4 
  refCount <- lift $ bytes @4 
  secDescSize <- lift $ bytes @4 
  secDescr    <- lift $ getBytes (fromIntegral $ toWord32le secDescSize)
  pure $ SKRecord magicNum unknown offset1 offset2 refCount secDescSize secDescr 

nkRecord :: RegParser NKRecord
nkRecord = do 
  magicNum        <- lift $ bytes @2 
  flags           <- lift $ bytes @2 
  timeStamp       <- lift $ bytes @8 
  unknown1        <- lift $ bytes @4
  offset1         <- lift w32
  stableSubkeys   <- lift w32
  unstableSubkeys <- lift w32
  stablePtr       <- lift w32
  unstablePtr     <- lift w32
  numValues       <- lift w32
  valueListPtr    <- lift w32
  skPtr           <- lift w32
  classNamePtr    <- lift w32
  maxBytesSubkey  <- lift w32
  maxLengthSubkey <- lift w32
  maxValueNm      <- lift w32
  maxValSize      <- lift w32
  unknown2        <- lift $ bytes @4 
  keyNameLength   <- lift w32
  classNameLength <- lift w32
  keyString       <- lift $ getBytes (fromIntegral keyNameLength)
  subkeys         <- if stableSubkeys > 0 
                     then Just <$> dropParse stablePtr subkeyList 
                     else pure Nothing 
  values          <- if numValues > 0 
                     then Just <$> dropParse valueListPtr valueList 
                     else pure Nothing  
  pure $ NKRecord 
         magicNum 
         flags 
         timeStamp 
         unknown1 
         offset1 
         stableSubkeys 
         unstableSubkeys 
         stablePtr 
         unstablePtr
         numValues 
         valueListPtr 
         skPtr 
         classNamePtr 
         maxBytesSubkey 
         maxLengthSubkey 
         maxValueNm 
         maxValSize 
         unknown2 
         keyNameLength 
         classNameLength
         keyString 
         subkeys 
         values 
         

vkRecord :: RegParser VKRecord 
vkRecord = do 
  magicNum  <- lift $ bytes @2 
  nameLen   <- lift w16
  dataLen   <- lift w32
  dataPtr   <- lift w32
  valType   <- lift w32
  nameFlags <- lift $ bytes @2 
  unknown   <- lift $ bytes @2 
  valName   <- lift $ getBytes (fromIntegral  nameLen) 
  val       <- parseSlice dataPtr dataLen (value valType)
  pure $ VKRecord magicNum nameLen dataLen dataPtr valType nameFlags unknown valName val  


subkeyList :: Get SubkeyList 
subkeyList = trace "subkeyList" $ do 
  magicNum <- bytes @2 
  numElems     <- w16
  elems        <- replicateM (fromIntegral numElems) (subkeyElem magicNum)
  pure $ SubkeyList magicNum numElems (V.fromList elems) 

subkeyElem :: Bytes 2 -> Get SubkeyElem 
subkeyElem magicN@(Bytes w) 
   | w == ri = Ri <$> bytes @4 
   | w == li = Li <$> bytes @4 
   | w == lh = Lh <$> bytes @4 <*> bytes @4  
   | w == lf = Lf <$> bytes @4 <*> bytes @4 
   | otherwise = error $ "Invalid subkey magic number: " <> show magicN 
 where 
   ri = pack "ri"
   li = pack "li"
   lh = pack "lh"
   lf = pack "lf"

value :: Word32 -> Get Value 
value w = case w of 
  0x0 -> REG_NONE <$> allRemainingBytes 
  0x1 -> REG_SZ <$> allRemainingBytes 
  0x2 -> REG_EXPAND_SZ <$> allRemainingBytes
  0x3 -> REG_BINARY <$> allRemainingBytes
  0x4 -> REG_DWORD <$> w32
  0x5 -> REG_DWORD_BIG_ENDIAN <$> getWord32be
  0x6 -> REG_LINK <$> allRemainingBytes
  0x7 -> REG_MULTI_SZ <$> allRemainingBytes
  0x8 -> REG_RESOURCE_LIST <$> allRemainingBytes
  0x9 -> REG_FULL_RESOURCE_DESCRIPTOR <$> allRemainingBytes 
  0xA -> REG_RESOURCE_REQUIREMENTS_LIST <$> allRemainingBytes
  0xB -> REG_QWORD <$> getWord64le
  other -> fail $ show other <> " is not a valid value identifier"



valueList :: Get ValueList 
valueList = V.fromList <$> some (bytes @4)

rawDataBlocks :: Get BS.ByteString 
rawDataBlocks = allRemainingBytes -- hopefully it works out like this

allRemainingBytes = remaining >>= getBytes
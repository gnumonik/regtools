{-# LANGUAGE TupleSections, RecordWildCards #-}
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
import qualified Data.Map as M 
import Control.Concurrent.STM 
import Control.Monad.Par.IO 
import Control.Lens 
import Control.Monad.IO.Class
import Data.Maybe 
import Control.Monad.Trans.Maybe 
import ParseM 
import Control.Concurrent.Async


nullPointer = 0xFFFFFFFF

maybeNull x = if x == nullPointer then Nothing else Just x  

okPtr :: Int -> Word32 -> Bool
okPtr len x = x /= nullPointer && x <= fromIntegral len  

w32 :: Get Word32
w32 = getWord32le 

w16 :: Get Word16 
w16 = getWord16le 

testhivepath :: FilePath 
testhivepath = "/home/gsh/Downloads/SeanStuff/hkeyclassesroot"

testParseHeader :: IO ()
testParseHeader = do 
  bs <-  BS.readFile testhivepath
  reg <- registry bs 
  Prelude.writeFile "/home/gsh/testregistry.txt"  $ pretty reg 

peekTest :: (Int,Int) ->  IO ()
peekTest i = do 
  bs <- BS.readFile testhivepath 
  res <- peekRange bs i 
  print res 
type Offset = Word32 

peekRange :: BS.ByteString -> (Int,Int) ->  IO (Either String BS.ByteString)
peekRange bs (s,e)  = case runGetPartial registryHeader bs of  
  Fail err _ -> pure (Left err) 
  Done hdr rest1 -> do 
        let o = fromIntegral $ _offset1 hdr 
        pure . Right . BS.take (e-s) . BS.drop (s) $ (BS.drop o rest1)
  _ -> error "boom"

registry :: BS.ByteString -> IO (Either String Registry) 
registry bs = case runGetPartial registryHeader bs of  
  Fail err _ -> pure (Left err) 
  Done hdr rest1 -> trace ("offset = " <> show (_offset1 hdr)) $ trace ("beginning of raw: " <> show (BS.take 100 rest1)) $ do 
      e <- initEnv (_offset1 hdr) rest1
      parseReg hiveBin e >>= \case 
        Nothing -> pure . Left $ "error - failed to parse registry"
        Just bin -> pure . Right $ Registry hdr (V.singleton bin)  
  _ -> pure . Left $ "Error - runGetPartial returned continuation"


type Chunk = Bytes 4096

chunk :: Get Chunk 
chunk = bytes @4096 

chunks :: Get (V.Vector (Word32,Chunk))
chunks = do 
  pos   <- fromIntegral bytesRead 
  chonk <- chunk 
  left  <- remaining  
  if left < 4096
    then pure $ V.singleton (pos,chonk)
    else V.cons (pos,chonk) <$> chunks  

rawCell :: Word32 -> Get RawCell 
rawCell location = do 
  size    <-   lookAhead w32 
  content <- getBytes . fromIntegral $ size 
  pure $ RawCell location size content  

initDriver :: IO Driver
initDriver = do 
  




registryHeader :: Get RegistryHeader 
registryHeader = do -- intentionally not doing applicative style here cuz i'm sure i'll make a mistake i'll never catch  
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


hiveBin ::  Get HiveBin 
hiveBin  = trace "hivebin" $ do
  raw      <- getHive 
  case runGet hiveBuilder raw of 
    Left err -> error "boom"
    Right hbin -> trace "hbin header ok" $ do 
      hiveCell 0 >>= \rootCell -> pure $ hbin (V.singleton rootCell)
  where 
    hiveBuilder :: Get (V.Vector HiveCell -> HiveBin) 
    hiveBuilder = do 
      magicNum       <- bytes @4 
      offset         <- w32
      binsize        <- w32
      unknown        <- bytes @16 
      nextHiveOffset <- bytes @4
      pure $ \c -> HiveBin magicNum offset binsize unknown nextHiveOffset c


hiveSize :: Get Int 
hiveSize = lookAhead $ do -- make this shorter once i know it works 
  _magicNum       <- bytes @4 
  _offset         <- bytes @4 
  binsize         <- bytes @4 
  pure . (* 4096) . fromIntegral . toWord32le $ binsize 




  
cellContent :: Get CellContent
cellContent offset =  trace "cellContent" $ bytes' @2 offset >>= \(Bytes magicN) ->
    case unpack magicN of 
      "sk" ->  SK <$> parseDrop offset skRecord
      "nk" ->  NK <$> nkRecord offset 
      "vk" ->  VK <$> vkRecord offset 
      other -> let subkeylists = ["ri","li","lh","lf"]
              in if other `Prelude.elem` subkeylists 
                  then  Subkeylist <$> subkeyList offset  
                  else trace ("Error: Cell Content Failure at Location " <> show offset <> " Magic Number: " <> unpack magicN) mzero--error $ "cellContent failure\nparsed magic number: " <> unpack magicN  --choice [ Valuelist <$> valueList 
                        --     , RawDataBlocks <$> rawDataBlocks] -- this is 100% wrong but i can't fix it til i know more about how these things point to each other 

skRecord :: Get SKRecord
skRecord = trace "skRecord" $ do 
  magicNum <- bytes @2 
  unknown  <- bytes @2 
  offset1  <- bytes @4 
  offset2  <- bytes @4 
  refCount <- bytes @4 
  secDescSize <- bytes @4 
  secDescr    <- getBytes (fromIntegral $ toWord32le secDescSize)
  let size = 2 + 2 + 4 + 4 + 4 + 4 + toWord32le secDescSize
  pure $ SKRecord magicNum unknown offset1 offset2 refCount secDescSize secDescr 

nkRecord :: Word32 -> RegParser NKRecord
nkRecord offset = trace ("nkRecord @" <> show offset) $ do 

    nk@(ParseOutput loc size nkb) <- parseCC offset nkBuilder  

    let stableSubkeys   = _stableSubkeys nkb
    let unstableSubkeys = _unstableSubkeys nkb
    let stablePtr       = _stableSubkeyPtr nkb
    let unstablePtr     = _unstableSubkeyPtr nkb

    e <- look 
    
    a1 <- trace ("parsing stable subkeylist @" <> show stablePtr) 
        liftIO . async 
          $ when (stableSubkeys > 0) 
            $ void 
              $ runParseM e (parseCC' stablePtr subkeyList) 
                     
    a2 <- trace ("parsing stable subkeylist @" <> show stablePtr) 
        liftIO . async 
          $ when (unstableSubkeys > 0) 
            $ void 
              $ runParseM e (parseCC' unstablePtr subkeyList)

    a3 <- trace "bop" 
        liftIO . async 
          $ when (_numValues nkb > 0)
            $ void 
              $ runParseM e (parseCC (_valueListPtr nkb) valueList) 
                       

    _ <- liftIO $ mapM_ wait [a1,a2,a3]

    pure nk

 where 
   nkBuilder :: Get NKRecord -- Get (Sized (Maybe SubkeyList -> Maybe SubkeyList -> Maybe ValueList -> V.Vector NKRecord -> V.Vector VKRecord -> NKRecord))
   nkBuilder = trace "nkBuilder" $ do 
      magicNum        <- bytes @2 
      flags           <- bytes @2 
      timeStamp       <- bytes @8 
      unknown1        <- bytes @4
      offset1         <- w32
      stableSubkeys   <- w32
      unstableSubkeys <- w32
      stablePtr       <- w32
      unstablePtr     <- w32
      numValues       <- w32
      valueListPtr    <- w32
      skPtr           <- w32
      classNamePtr    <- w32
      maxBytesSubkey  <- w32
      maxLengthSubkey <- w32
      maxValueNm      <- w32
      maxValSize      <- w32
      unknown2        <- bytes @4 
      keyNameLength   <- w32
      classNameLength <- w32
      keyString       <- getBytes (fromIntegral keyNameLength)
      bRead           <- bytesRead 
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


subkeysWithList :: SubkeyList -> ParseM ()
subkeysWithList skl = trace ("subkeysWithList: " <> pretty skl) 
                    $ V.mapM_ go  (_subkeyElems skl) 
  where 
    go :: SubkeyElem -> ParseM ()
    go  ske =  case ske of 
          Ri off   -> subkeyList off >>= \ skl -> subkeysWithList skl 
          Li off   -> void $ nkRecord off 
          Lh off _ -> void $ nkRecord off 
          Lf off _ -> void $ nkRecord off 
    getNK :: V.Vector NKRecord -> Word32 -> Get (V.Vector NKRecord )
    getNK acc off =  nkRecord off >>= \nk -> pure $ acc `V.snoc` nk 


valuesWithList :: ValueList -> Get (V.Vector VKRecord)
valuesWithList vl = trace "valuesWithList" $ V.foldM' go V.empty vl 
  where 
    go :: V.Vector VKRecord -> Word32 -> Get (V.Vector VKRecord) 
    go acc off = vkRecord off >>= \vk  -> pure $ acc `V.snoc` vk 
         

vkRecord :: Word32 -> Get VKRecord
vkRecord off = trace "vkRecord" $  do 
      magicNum  <- bytes @2 
      nameLen   <- w16
      dataLen   <- w32
      dataPtr   <- w32
      valType   <- w32
      nameFlags <- bytes @2 
      unknown   <- bytes @2 
      valName   <- getBytes (fromIntegral  nameLen)
      bRead     <- fromIntegral <$> bytesRead 
      pure  $ VKRecord magicNum nameLen dataLen dataPtr valType nameFlags unknown valName (REG_NONE BS.empty)

subkeyList :: Word32 -> RegParser SubkeyList
subkeyList  off = trace "subkeyList" $ parseCC off getSKL 
  where 
    getSKL :: Get SubkeyList 
    getSKL = do 
      magicNum     <- elemMagic -- bytes @2 
      numElems     <- w16
      elems        <- trace ("numElems: " <> show numElems) $ if numElems == 0 then pure [] else replicateM (fromIntegral numElems) (subkeyElem magicNum)
      bRead        <- trace "boop" $ fromIntegral <$> bytesRead 
      pure $ SubkeyList magicNum numElems (V.fromList elems) 

    elemMagic :: Get (Bytes 2) 
    elemMagic = trace "elemMagic" $ bytes @2 >>= \b@(Bytes bs) -> 
      let unpacked = unpack bs 
      in  trace unpacked $ if unpacked `Prelude.elem` ["ri","li","lh","lf"] 
          then pure b 
          else fail $ "invalid subkey magic number: " <> unpacked 

subkeyElem :: Bytes 2 -> Get SubkeyElem 
subkeyElem magicN@(Bytes w) 
   | w == ri = Ri <$> w32
   | w == li = Li <$> w32
   | w == lh = Lh <$> w32 <*> w32
   | w == lf = Lf <$> w32 <*> w32
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
valueList = V.fromList <$> some w32

rawDataBlocks :: Get BS.ByteString 
rawDataBlocks = allRemainingBytes -- hopefully it works out like this

allRemainingBytes = remaining >>= getBytes
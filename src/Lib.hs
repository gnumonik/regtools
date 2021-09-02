{-# LANGUAGE TupleSections, RecordWildCards, BinaryLiterals #-}
module Lib where

import Data.Serialize
    ( bytesRead,
      getBytes,
      getWord16le,
      getWord32be,
      getWord32le,
      getWord64le,
      lookAhead,
      remaining,
      runGetPartial,
      Get,
      Result(Done, Fail, Partial) ) 
import Data.Word ( Word16, Word32 ) 
import qualified Data.ByteString as BS 

import qualified Data.Vector as V 
import Control.Applicative.Combinators ( some ) 

import Control.Monad ( replicateM )
import Data.Functor ((<&>))
import Data.ByteString.Char8 ( ByteString, foldl', pack )
import qualified Data.Map as M 
import Control.Concurrent.STM ( newTVarIO, readTVarIO ) 
import Control.Lens ( (^.) ) 
import qualified Data.Sequence as S

import ParseM
    ( RegEnv(RegEnv),
      ParseOutput(..),
      ParseErrs,
      unOut,
      ParseM,
      Driver,
      parseErrs,
      parsed,
      parseCC,
      parseCC',
      between',
      runParseM,
      parseWhen,
      datastart,
      continueWith, OccupiedSpace (OccupiedSpace) ) 
import Types
import Control.Monad (void)
import Time
import Data.Bits 

checkVKPointer :: Word32 -> Bool 
checkVKPointer w = w .&. 0b10000000000000000000000000000000 /= 0b10000000000000000000000000000000 

data HiveData = HiveData {_rHeader :: RegistryHeader 
                         ,_hHeader :: HiveBinHeader 
                         ,_hEnv    :: RegEnv} 

peek :: (Word32 -> Word32) -> Word32 -> IO ()
peek f x = case sort (x,f x) of {(a,b) -> peekTest b a}
  where 
    sort (a,b) = if a > b then (a,b) else (b,a)

nullPointer :: Num p => p
nullPointer = 0xFFFFFFFF

maybeNull :: (Eq a, Num a) => a -> Maybe a
maybeNull x = if x == nullPointer then Nothing else Just x  

okPtr :: Int -> Word32 -> Bool
okPtr len x = x /= nullPointer && x <= fromIntegral len  

w32 :: Get Word32
w32 = getWord32le 

w16 :: Get Word16 
w16 = getWord16le 

hiveData :: FilePath -> IO HiveData 
hiveData fPath = do 
  bs <- BS.readFile testhivepath 
  registry bs >>= \case 
    Left err -> error err 
    Right (rH,hH,tv) -> do 
      e <- readTVarIO tv
      pure $ HiveData rH hH e 

testhivepath :: FilePath 
testhivepath = "/home/gsh/Downloads/SeanStuff/hkeyclassesroot"

testParseHeader :: IO ()
testParseHeader = do 
  bs <-  BS.readFile testhivepath
  registry bs >>= \case 
    Left err -> print "Error" -- err 
    Right (r,h,tv) -> do 
      e <- readTVarIO tv 
      let cells = M.size (e ^. parsed)
      Prelude.putStrLn $ "Successfully parsed " <> show cells <> " keys"

peekTest :: Word32 -> Word32 ->  IO ()
peekTest w1 w2 = do 
  bs <- BS.readFile testhivepath 
  peekRange bs w1 w2 >>= \case 
    Left errs -> print errs 
    Right bstring -> do 
      Prelude.putStrLn . format $ bstring 
 where 
   format :: BS.ByteString -> String 
   format bs = snd $ foldl' (\(n,acc) x -> (n+1,acc <> "\n" <> show n <> ": " <> show x <> "\n")) (w1,"") bs 

type Offset = Word32 

peekRange :: BS.ByteString -> Word32 -> Word32 ->  IO (Either ParseErrs BS.ByteString)
peekRange bs w1 w2 = case initE bs of 
  Left err -> error "boom"
  Right (rH,hH,rEnv) -> do 
    tvar <- newTVarIO rEnv
    runParseM tvar (between' w1 w2)

registry :: BS.ByteString -> IO (Either String (RegistryHeader,HiveBinHeader,Driver)) 
registry bs = case initE bs of 
  Left err -> pure . Left $ err 
  Right (rH,hH,rEnv) -> do 
    tvar <- newTVarIO rEnv 
    ecells <- runParseM tvar (nkRecord $ _offset1 rH) 
    case ecells of 
      Left errs                  -> pure . Left . show $ errs 
      Right (ParseOutput l s nk) -> do  
        let reg = Registry rH (V.singleton $ HiveBin hH (V.singleton $ HiveCell s $ NK nk))
        pure . Right $ (rH,hH,tvar)

type Vec = V.Vector 

initE :: BS.ByteString -> Either String (RegistryHeader, HiveBinHeader, RegEnv) 
initE bs = case runGetPartial mkChunks' bs of 
  Fail err bs -> Left err  
  Partial _   -> Left "partial return"
  Done (h1,h2) bs' -> Right $ (h1,h2, RegEnv M.empty (BS.drop 4 bs') S.empty (_offset1 h1) (OccupiedSpace $ M.empty)) 

mkChunks' :: Get (RegistryHeader, HiveBinHeader)
mkChunks' = do 
  rHdr <- registryHeader 
  hHdr <- lookAhead hiveBinHeader -- Most of the documentation is ambiguous, but testing confirms that the offsets 
                                  -- in registry records are offsets from the START OF THE BIN HEADER
                                  -- ergo this has to be lookAhead. Don't change it.
  pure (rHdr,hHdr)

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
  let rslt = RegistryHeader magicNumber 
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
  pure rslt 

hiveBinHeader:: Get HiveBinHeader
hiveBinHeader = do 
  magicNum       <- bytes @4 
  offset         <- w32
  binsize        <- w32
  unknown        <- bytes @16 
  nextHiveOffset <- bytes @4
  pure $ HiveBinHeader magicNum offset binsize unknown nextHiveOffset 

hiveSize :: Get Int 
hiveSize = lookAhead $ do -- make this shorter once i know it works 
  _magicNum       <- bytes @4 
  _offset         <- bytes @4 
  binsize         <- bytes @4 
  pure . (* 4096) . fromIntegral . toWord32le $ binsize 

skRecord :: Get SKRecord
skRecord = do 
  magicNum    <- bytes @2 
  unknown     <- bytes @2 
  offset1     <- bytes @4 
  offset2     <- bytes @4 
  refCount    <- bytes @4 
  secDescSize <- bytes @4 
  secDescr    <- getBytes (fromIntegral $ toWord32le secDescSize)
  pure $ SKRecord magicNum unknown offset1 offset2 refCount secDescSize secDescr 

nkRecord :: Word32 -> ParseM (ParseOutput NKRecord)
nkRecord offset = do 

    nk@(ParseOutput loc size nkb) <- parseCC offset nkBuilder  

    let stableSubkeys   = _stableSubkeys nkb
    let unstableSubkeys = _unstableSubkeys nkb
    let stablePtr       = _stableSubkeyPtr nkb
    let unstablePtr     = _unstableSubkeyPtr nkb
    let numVals         = _numValues nkb 
    let vlPtr           = _valueListPtr nkb 
    let secPtr          = _skPtr nkb 


    !stableChldrn  <- continueWith (V.empty :: Vec NKRecord) 
                      $ parseWhen (stableSubkeys > 0 && stablePtr /= 0 && stablePtr /= (maxBound :: Word32)) 
                      $ parseCC' stablePtr subkeyList >>= subkeysWithList 
                     
    !volatileChldrn <- continueWith (V.empty :: Vec NKRecord) 
                      $ parseWhen (unstableSubkeys > 0 && unstablePtr /= 0 && unstablePtr /= (maxBound :: Word32))  
                      $ parseCC' unstablePtr subkeyList >>= subkeysWithList 

    !values <- continueWith V.empty 
             $ parseWhen (numVals > 0 && vlPtr /= 0 && vlPtr /= (maxBound :: Word32)) 
             $ valueList vlPtr numVals >>= valuesWithList 

    !skRec  <- continueWith Nothing 
             $  parseWhen (secPtr /= (maxBound :: Word32) && secPtr /= 0)
             $  Just 
            <$> parseCC' secPtr skRecord 
                       
    pure nk

 where
   nkBuilder :: Get NKRecord 
   nkBuilder = do 
      magicNum        <- bytes @2 
      flags           <- bytes @2 
      timeStamp       <- getWord64le 
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
      keyNameLength   <- w16
      classNameLength <- w16
      keyString       <- getBytes (fromIntegral keyNameLength)
      bRead           <- bytesRead 
      pure $ NKRecord 
            magicNum 
            flags 
            (convertTime timeStamp) 
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

subkeysWithList :: SubkeyList -> ParseM (V.Vector NKRecord )
subkeysWithList skl = V.foldM' go V.empty (_subkeyElems skl) 
  where 
    go :: V.Vector NKRecord -> SubkeyElem -> ParseM (V.Vector NKRecord) 
    go acc ske =  case ske of 
          Ri off   -> continueWith acc $  do 
            (ParseOutput _ _ skl) <- parseCC off subkeyList
            nks <- subkeysWithList skl 
            pure $ acc <> nks 
          Li off   -> continueWith acc $ nkRecord off >>= pure . (\x -> V.cons x acc) . unOut  
          Lh off _ -> continueWith acc $ nkRecord off >>= pure . (\x -> V.cons x acc) . unOut  
          Lf off _ -> continueWith acc $ nkRecord off >>= pure . (\x -> V.cons x acc) . unOut   

valuesWithList :: ValueList -> ParseM (V.Vector VKRecord)
valuesWithList vl =  do 
  vks <- V.foldM' go V.empty vl
  V.foldM'_  getValData () vks    
  pure vks 
  where 
    getValData :: () -> VKRecord -> ParseM ()
    getValData acc vk = do 
      let vLen  = vk ^. dataLength 
      let vType = vk ^. valueType 
      let vPtr  = vk ^. dataPtr 
      continueWith acc . void $! parseWhen (checkVKPointer vLen) $ parseCC vPtr (valueData vLen vType)

    go :: V.Vector VKRecord -> Word32 -> ParseM (V.Vector VKRecord) 
    go acc off = continueWith acc $ parseCC off  (vkRecord off) >>= \(ParseOutput _ _ vk)  -> pure $ acc `V.snoc` vk 

vkRecord :: Word32 -> Get VKRecord
vkRecord off =  do 
      magicNum  <- bytes @2 
      nameLen   <- w16
      dataLen   <- w32
      dataPtr   <- w32
      valType   <- w32
      nameFlags <- bytes @2 
      unknown   <- bytes @2 
      valName   <- getBytes (fromIntegral  nameLen)
      pure  $ VKRecord magicNum nameLen dataLen dataPtr valType nameFlags unknown valName (REG_NONE BS.empty)

subkeyList :: Get SubkeyList
subkeyList = getSKL 
  where 
    getSKL :: Get SubkeyList 
    getSKL = do 
      magicNum     <- bytes @2 
      numElems     <- w16
      elems        <- if numElems == 0 && okMagic magicNum then pure [] else replicateM (fromIntegral numElems) (subkeyElem magicNum)
      bRead        <- fromIntegral <$> bytesRead 
      pure $ SubkeyList magicNum numElems (V.fromList elems) 

    okMagic :: Bytes 2 -> Bool 
    okMagic (Bytes bs) =  
      let    ri = pack "ri"
             li = pack "li"
             lh = pack "lh"
             lf = pack "lf"
             ok x = x `Prelude.elem` [ri,li,lh,lf]
      in  ok bs 

subkeyElem :: Bytes 2 -> Get SubkeyElem 
subkeyElem magicN@(Bytes w) 
   | w == ri = Ri <$> w32
   | w == li = Li <$> w32
   | w == lh = Lh <$> w32 <*> w32
   | w == lf = Lf <$> w32 <*> w32
   | otherwise = fail $ "Invalid subkey magic number: " <> show magicN 
 where 
   ri = pack "ri"
   li = pack "li"
   lh = pack "lh"
   lf = pack "lf"

valueData :: Word32 -> Word32 -> Get Value 
valueData len w 
  = let rest = getBytes (fromIntegral len) 
    in case w of 
        0x0 -> REG_NONE <$> rest 
        0x1 -> REG_SZ <$> rest
        0x2 -> REG_EXPAND_SZ <$> rest
        0x3 -> REG_BINARY <$> rest
        0x4 -> REG_DWORD <$> w32
        0x5 -> REG_DWORD_BIG_ENDIAN <$> getWord32be
        0x6 -> REG_LINK <$> rest
        0x7 -> REG_MULTI_SZ <$> rest
        0x8 -> REG_RESOURCE_LIST <$> rest
        0x9 -> REG_FULL_RESOURCE_DESCRIPTOR <$> rest
        0xA -> REG_RESOURCE_REQUIREMENTS_LIST <$> rest
        0xB -> REG_QWORD <$> getWord64le
        other -> fail $ show other <> " is not a valid value identifier"

valueList :: Word32 -> Word32 -> ParseM ValueList
valueList start len = parseCC' start (getVL len) 
 where 
  getVL :: Word32 -> Get ValueList 
  getVL len = V.fromList <$> replicateM (fromIntegral len) w32

rawDataBlocks :: Get BS.ByteString 
rawDataBlocks = allRemainingBytes -- hopefully it works out like this

allRemainingBytes :: Get ByteString
allRemainingBytes = remaining >>= getBytes

{-# LANGUAGE TupleSections, RecordWildCards #-}
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
      continueWith ) 
import Types
    ( Value(REG_QWORD, REG_NONE, REG_SZ, REG_EXPAND_SZ, REG_BINARY,
            REG_DWORD, REG_DWORD_BIG_ENDIAN, REG_LINK, REG_MULTI_SZ,
            REG_RESOURCE_LIST, REG_FULL_RESOURCE_DESCRIPTOR,
            REG_RESOURCE_REQUIREMENTS_LIST),
      SubkeyElem(..),
      SubkeyList(SubkeyList, _subkeyElems),
      ValueList,
      VKRecord(VKRecord),
      NKRecord(NKRecord, _stableSubkeys, _unstableSubkeys,
               _stableSubkeyPtr, _unstableSubkeyPtr, _valueListPtr, _numValues),
      SKRecord(SKRecord),
      CellContent(NK),
      HiveCell(HiveCell),
      HiveBinHeader(HiveBinHeader),
      HiveBin(HiveBin),
      RegistryHeader(RegistryHeader, _offset1),
      Registry(Registry),
      Bytes(..),
      Pretty(pretty),
      bytes,
      toWord32le )

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
      {-- let output = pretty r 
            <> "\nErrors: \n"
            <>  show (e ^. parseErrs)
            <> "\n\n\n"
            <> "Cells: \n"
            <> show (e ^. parsed)
      Prelude.writeFile "/home/gsh/testregistry.txt" output --}

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

type Chunk = Bytes 4096

chunk :: Get (Word32,Chunk) 
chunk =  do 
  bRead <- bytesRead 
  b <- bytes @4096 
  pure $ (fromIntegral bRead,b)

type Vec = V.Vector 

initE :: BS.ByteString -> Either String (RegistryHeader, HiveBinHeader, RegEnv) 
initE bs = case runGetPartial mkChunks' bs of 
  Fail err bs -> Left err  
  Partial _   -> Left "partial return"
  Done (h1,h2) bs' -> Right $ (h1,h2, RegEnv M.empty (BS.drop 4 bs') S.empty (_offset1 h1) (const True)) 

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
  magicNum <- bytes @2 
  unknown  <- bytes @2 
  offset1  <- bytes @4 
  offset2  <- bytes @4 
  refCount <- bytes @4 
  secDescSize <- bytes @4 
  secDescr    <- getBytes (fromIntegral $ toWord32le secDescSize)
  let size = 2 + 2 + 4 + 4 + 4 + 4 + toWord32le secDescSize
  pure $ SKRecord magicNum unknown offset1 offset2 refCount secDescSize secDescr 

nkRecord :: Word32 -> ParseM (ParseOutput NKRecord)
nkRecord offset = do 
    strt <- datastart 

    nk@(ParseOutput loc size nkb) <- parseCC offset nkBuilder  

    let stableSubkeys   = _stableSubkeys nkb
    let unstableSubkeys = _unstableSubkeys nkb
    let stablePtr       = _stableSubkeyPtr nkb
    let unstablePtr     = _unstableSubkeyPtr nkb


    !stableChldrn  <- continueWith (V.empty :: Vec NKRecord) 
                      $ parseWhen (stableSubkeys > 0 && stablePtr /= 0 && stablePtr /= (maxBound :: Word32)) 
                      $ parseCC' stablePtr subkeyList >>= subkeysWithList 
                     
    !volatileChldrn <- continueWith (V.empty :: Vec NKRecord) 
                      $ parseWhen (unstableSubkeys > 0 && unstablePtr /= 0 && unstablePtr /= (maxBound :: Word32))  
                      $ parseCC' unstablePtr subkeyList >>= subkeysWithList 

    !values <- continueWith V.empty 
             $ parseWhen (_numValues nkb > 0 && _valueListPtr nkb /= 0 && _valueListPtr nkb /= (maxBound :: Word32)) 
             $ parseCC' (_valueListPtr nkb) (V.fromList <$> replicateM (fromIntegral (_numValues nkb)) w32) >>= valuesWithList 
                       
    pure nk

 where
   nkBuilder :: Get NKRecord -- Get (Sized (Maybe SubkeyList -> Maybe SubkeyList -> Maybe ValueList -> V.Vector NKRecord -> V.Vector VKRecord -> NKRecord))
   nkBuilder = do 
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
valuesWithList vl =  V.foldM' go V.empty vl 
  where 
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

allRemainingBytes :: Get ByteString
allRemainingBytes = remaining >>= getBytes


testCell :: ByteString
testCell = pack "x\255\255\255nk,\NUL \239y\166\135\152\215\SOH\NUL\NUL\NUL\NUL\255\255\255\255\&1\DC2\NUL\NUL\NUL\NUL\NUL\NUL\b\220\201\STX\255\255\255\255\NUL\NUL\NUL\NUL\255\255\255\255\&8\237W\NUL\255\255\255\255\154\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL6\NUL\NUL\NULS-1-5-21-3199533274-2294187411-1904285961-1001_Classes\NUL\NUL\NUL\255\255\255sk\NUL\NUL\136jT\NUL\232\tX\NUL\SOH\NUL\NUL\NUL\232\NUL\NUL\NUL\SOH\NUL\EOT\144\208\NUL\NUL\NUL\220\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\NUL\NUL\NUL\STX\NUL\188\NUL\ACK\NUL\NUL\NUL\NUL\ETX$\NUL?\NUL\SI\NUL\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\ENQ\NAK\NUL\NUL\NUL\218\NUL\181\190\147\133\190\136\t\EM\129q\233\ETX\NUL\NUL\NUL\ETX\DC4\NUL?\NUL\SI\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\ENQ\DC2\NUL\NUL\NUL\NUL\ETX\CAN\NUL?\NUL\SI\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\ENQ \NUL\NUL\NUL \STX\NUL\NUL\NUL\ETX\DC4\NUL\EM\NUL\STX\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\ENQ\f\NUL\NUL\NUL\NUL\ETX\CAN\NUL\EM\NUL\STX\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\SI\STX\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\ETX8\NUL\EM\NUL\STX\NUL\SOH\n\NUL\NUL\NUL\NUL\NUL\SI\ETX\NUL\NUL\NUL\NUL\EOT\NUL\NUL\176\&1\128?l\188cL<\224P\209\151\f\161b\SI\SOH\203\EM~z\166\192\250\230\151\241\EM\163\f\206\SOH\SOH\NUL\NUL\NUL\NUL\NUL\ENQ\DC2\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\ENQ\DC2\NUL\NUL\NUL\168\255\255\255nk \NUL\164b\182.\223\172\213\SOH\NUL\NUL\NUL\NUL \NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\252W\NUL\255\255\255\255\DLE\NUL\NUL\NULH\253W\NUL \240W\NUL\255\255\255\255\CAN\NUL\NUL\NUL\NUL\NUL\NUL\NULJ\NUL\NUL\NULv\STX\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL*\EM\129q\233\ETX\NUL\NUL\224\254\255\255sk\NUL\NUL \216\186\NUL@\NAK\NUL\NULA\DC1\NUL\NUL\b\SOH\NUL\NUL\SOH\NUL\EOT\132\208\NUL\NUL\NUL\236\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\NUL\NUL\NUL\STX\NUL\188\NUL\ACK\NUL\NUL\NUL\NUL\DC3$\NUL?\NUL\SI\NUL\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\ENQ\NAK\NUL\NUL\NUL\218\NUL\181\190\147\133\190\136\t\EM\129q\233\ETX\NUL\NUL\NUL\DC3\DC4\NUL?\NUL\SI\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\ENQ\DC2\NUL\NUL\NUL\NUL\DC3\CAN\NUL?\NUL\SI\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\ENQ \NUL\NUL\NUL \STX\NUL\NUL\NUL\DC3\DC4\NUL\EM\NUL\STX\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\ENQ\f\NUL\NUL\NUL\NUL\DC3\CAN\NUL\EM\NUL\STX\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\SI\STX\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\DC38\NUL\EM\NUL\STX\NUL\SOH\n\NUL\NUL\NUL\NUL\NUL\SI\ETX\NUL\NUL\NUL\NUL\EOT\NUL\NUL\176\&1\128?l\188cL<\224P\209\151\f\161b\SI\SOH\203\EM~z\166\192\250\230\151\241\EM\163\f\206\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\ENQ\NAK\NUL\NUL\NUL\218\NUL\181\190\147\133\190\136\t\EM\129q\233\ETX\NUL\NUL\SOH\ENQ\NUL\NUL\NUL\NUL\NUL\ENQ\NAK\NUL\NUL\NUL\218\NUL\181\190\147\133\190\136\t\EM\129q\SOH\STX\NUL\NUL\248\255\255\255\168\b\NUL\NUL\248\255\255\255\136\&5X\NUL\168\255\255\255nk \NUL\164b\182.\223\172\213\SOH\NUL\NUL\NUL\NUL\168\SOH\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\224\ENQX\NUL\255\255\255\255\NUL\NUL\NUL\NUL\255\255\255\255 \240W\NUL\255\255\255\255\&8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NULshell\NUL\NUL\NUL\248\255\255\255\176\NAKX\NUL\248\255\255\255\144\SYNX\NUL\168\255\255\255nk \NUL\ENQ\162-\165\135\152\215\SOH\NUL\NUL\NUL\NUL0\ETX\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\ACK\NUL\NUL\255\255\255\255\STX\NUL\NUL\NUL0\EOT\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\SO\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NULVSCode\NUL\NUL\232\255\255\255vk\NUL\NUL \NUL\NUL\NUL\b\EOT\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\216\255\255\255O\NULp\NULe\NULn\NUL \NULw\NUL&\NULi\NULt\NULh\NUL \NULC\NULo\NULd\NULe\NUL\NUL\NUL\NUL\NUL\NUL\NUL\240\255\255\255\240\ETX\NUL\NUL@\EOT\NUL\NUL\NUL\NUL\NUL\NUL\224\255\255\255vk\EOT\NUL\128\NUL\NUL\NUL`\EOT\NUL\NUL\STX\NUL\NUL\NUL\SOH\NUL\NUL\NULIcon\NUL\NUL\NUL\NULx\255\255\255C\NUL:\NUL\\\NULU\NULs\NULe\NULr\NULs\NUL\\\NULS\NULe\NULa\NULn\NUL\\\NULA\NULp\NULp\NULD\NULa\NULt\NULa\NUL\\\NULL\NULo\NULc\NULa\NULl\NUL\\\NULP\NULr\NULo\NULg\NULr\NULa\NULm\NULs\NUL\\\NULM\NULi\NULc\NULr\NULo\NULs\NULo\NULf\NULt\NUL \NULV\NULS\NUL \NULC\NULo\NULd\NULe\NUL\\\NULC\NULo\NULd\NULe\NUL.\NULe\NULx\NULe\NUL\NUL\NUL\NUL\NUL\NUL\NUL\240\255\255\255lf\SOH\NUL\128\SOHX\NULDrop\168\255\255\255nk \NUL\ENQ\162-\165\135\152\215\SOH\NUL\NUL\NUL\NUL\152\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\255\SOH\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\142\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\NUL\NULcommand\NUL\232\255\255\255vk\NUL\NUL\142\NUL\NUL\NULh\ENQ\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NULh\255\255\255\"\NULC\NUL:\NUL\\\NULU\NULs\NULe\NULr\NULs\NUL\\\NULS\NULe\NULa\NULn\NUL\\\NULA\NULp\NULp\NULD\NULa\NULt\NULa\NUL\\\NULL\NULo\NULc\NULa\NULl\NUL\\\NULP\NULr\NULo\NULg\NULr\NULa\NULm\NULs\NUL\\\NULM\NULi\NULc\NULr\NULo\NULs\NULo\NULf\NULt\NUL \NULV\NULS\NUL \NULC\NULo\NULd\NULe\NUL\\\NULC\NULo\NULd\NULe\NUL.\NULe\NULx\NULe\NUL\"\NUL \NUL\"\NUL%\NUL1\NUL\"\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\248\255\255\255P\ENQ\NUL\NUL\240\255\255\255lf\SOH\NUL\248\EOT\NUL\NULcomm\168\255\255\255nk \NUL\164b\182.\223\172\213\SOH\NUL\NUL\NUL\NUL\168\SOH\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL8\NAKX\NUL\255\255\255\255\NUL\NUL\NUL\NUL\255\255\255\255 \240W\NUL\255\255\255\255*\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\NUL\NULshellex\NUL\232\255\255\255vk\NUL\NUL\STX\NUL\NUL\128\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\128\152\255\255\255nk \NUL\142\&7\229\\\DC1\CAN\215\SOH\NUL\NUL\NUL\NUL\CAN\ACK\NUL\NUL\n\NUL\NUL\NUL\NUL\NUL\NUL\NUL8\DC3X\NUL\255\255\255\255\NUL\NUL\NUL\NUL\255\255\255\255 \240W\NUL\255\255\255\255L\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC3\NUL\NUL\NULContextMenuHandlers\NUL\NUL\NUL\NUL\SI\160\255\255\255nk \NUL\178\201\242cr\151\215\SOH\NUL\NUL\NUL\NUL\136\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\255\SOH\NUL\NUL\NUL\192\a\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULN\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL FileSyncEx\NUL\255\255\255\255\232\255\255\255vk\NUL\NULN\NUL\NUL\NULh\a\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL42\168\255\255\255{\NULC\NULB\NUL3\NULD\NUL0\NULF\NUL5\NUL5\NUL-\NULB\NULC\NUL2\NULC\NUL-\NUL4\NULC\NUL1\NULA\NUL-\NUL8\NUL5\NULE\NULD\NUL-\NUL2\NUL3\NULE\NULD\NUL7\NUL5\NULB\NUL5\NUL1\NUL0\NUL6\NULB\NUL}\NUL\NUL\NULs\NULC\NULa\NUL\248\255\255\255P\a\NUL\NUL\248\255\255\255\248\vX\NUL\248\255\255\255\CAN\SOX\NUL\168\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL \NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\248\ESCX\NUL\255\255\255\255\STX\NUL\NUL\NUL\144\ESCX\NUL \240W\NUL\255\255\255\255\RS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL\DC4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL.3fr\SOH\NUL\NUL\NUL\240\255\255\255\136\n\NUL\NUL\200\n\NUL\NUL\144\GSX\NUL\248\255\255\255\192\SYN\NUL\NUL\160\255\255\255nk \NUL\ACK o\RS\211\137\215\SOH\NUL\NUL\NUL\NUL\216\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\255\SOH\NUL\NUL\NUL \ETX\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NULOpenWithProgids\NUL\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL04AppX9rkaq77s0jzh1tyccadx9ghba15r6t3h\NUL\NULfa\240\255\255\255lf\SOH\NUL\DLE\FSX\NUL{e35\168\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL \NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\CAN\RSX\NUL\255\255\255\255\EOT\NUL\NUL\NULx\GSX\NUL\NUL\STX\NUL\NUL\255\255\255\255\RS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL(\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL.3g2\SOH\SOH\NUL\NUL\232\255\255\255vk\NUL\NUL\DLE\NUL\NUL\NULh\t\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\CAN\NUL\208\255\255\255V\NULL\NULC\NUL.\NUL3\NULg\NUL2\NUL\NUL\NULs\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NUL2\NUL\NUL\NULW\NULi\NUL\248\255\255\255\168\RSX\NUL\248\255\255\255\144\USX\NUL\216\255\255\255vk\n\NUL(\NUL\NUL\NUL\208\t\NUL\NUL\SOH\NUL\NUL\NUL\SOH\NUL8\NULVLC.backup3\NULd\NUL8\NUL\208\255\255\255W\NULM\NULP\NUL1\NUL1\NUL.\NULA\NULs\NULs\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NUL2\NUL\NUL\NULr\NULo\NUL\240\255\255\255\200\SO\NUL\NUL\b\SI\NUL\NULh$X\NUL\248\255\255\255\128%X\NUL\248\255\255\255h&X\NUL\248\255\255\255\b9X\NUL\160\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL\248\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\255\ETX\NUL\NUL\NUL0\b\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NULOpenWithProgids\NUL\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NULelAppX6eg8h5sxqq90pv53845wmnbewywdqq5hm\NUL\NUL\NUL\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\ETX\NULAppXk0g4vb8gvt7b93tg50ybcy892pge6jmt\NUL\NUL\NUL\NUL\248\255\255\255\144 X\NUL\248\255\255\255p\"X\NUL\168\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL \NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\224!X\NUL\255\255\255\255\EOT\NUL\NUL\NUL@!X\NUL\NUL\STX\NUL\NUL\255\255\255\255\RS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL(\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL.3gp\EM\163\f\206\232\255\255\255vk\NUL\NUL\DLE\NUL\NUL\NUL\136\v\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\208\255\255\255V\NULL\NULC\NUL.\NUL3\NULg\NULp\NUL\NUL\NULs\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NULP\NUL\NUL\NULe\NULn\NUL\240\255\255\255v\NULi\NULd\NULe\NULo\NUL\NUL\NUL\216\255\255\255vk\n\NUL(\NUL\NUL\NUL\240\v\NUL\NUL\SOH\NUL\NUL\NUL\SOH\NULe\NULVLC.backupe\NULs\NUL/\NUL\208\255\255\255W\NULM\NULP\NUL1\NUL1\NUL.\NULA\NULs\NULs\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NULP\NUL\NUL\NULnk \NUL\160\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL\CAN\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\255\ETX\NUL\NUL\NUL\192\f\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NULOpenWithProgidsL\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\DC4\NULAppX6eg8h5sxqq90pv53845wmnbewywdqq5h\SOH\NUL\NUL\NUL\240\255\255\255\128\f\NUL\NUL\208\f\NUL\NULX!X\NUL\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\DLE\NULAppXk0g4vb8gvt7b93tg50ybcy892pge6jmt\NUL\NUL\NUL\NUL\248\255\255\255X#X\NUL\248\255\255\255\144(X\NUL\168\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL \NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\240$X\NUL\255\255\255\255\EOT\NUL\NUL\NULP$X\NUL\NUL\STX\NUL\NUL\255\255\255\255\RS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL(\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL.3gp2\f\161b\232\255\255\255vk\NUL\NUL\DC2\NUL\NUL\NUL\144\r\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\CAN\NUL\208\255\255\255V\NULL\NULC\NUL.\NUL3\NULg\NULp\NUL2\NUL\NUL\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NUL2\NUL\NUL\NUL\200\DC4\NUL\NUL\240\255\255\255v\NULi\NULd\NULe\NULo\NUL\NUL\NUL\216\255\255\255vk\n\NUL(\NUL\NUL\NUL\248\r\NUL\NUL\SOH\NUL\NUL\NUL\SOH\NUL\STX\NULVLC.backup\NUL\NUL\NUL\NUL\NUL\NUL\208\255\255\255W\NULM\NULP\NUL1\NUL1\NUL.\NULA\NULs\NULs\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NUL2\NUL\NUL\NUL9jj1\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NULAppXqj98qxeaynz6dv4459ayz6bnqxbyaqcs\NUL\NUL\NUL\NUL\160\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL \r\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\255\ETX\NUL\NUL\NUL\NUL\n\NUL\NUL\NUL\STX\NUL\NUL\255\255\255\255\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NULOpenWithProgids\206\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NULcLAppX6eg8h5sxqq90pv53845wmnbewywdqq5h\SOH\SOH\NUL\NUL\192\255\255\255vk$\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NULs.AppXk0g4vb8gvt7b93tg50ybcy892pge6jmtal.B\168\255\255\255nk \NUL`#\235B\228\172\213\SOH\NUL\NUL\NUL\NUL \NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL(X\NUL\255\255\255\255\EOT\NUL\NUL\NUL`'X\NUL\NUL\STX\NUL\NUL\255\255\255\255\RS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL(\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL.3gpp\163\f\206\232\255\255\255vk\NUL\NUL\DC2\NUL\NUL\NUL\184\SI\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\208\255\255\255V\NULL\NULC\NUL.\NUL3\NULg\NULp\NULp\NUL\NUL\NULo\NULc\NULF\NULi\NULl\NULe\NUL.\NUL3\NULG\NULP\NUL\NUL\NULW\NULM\NUL\248\255\255\255x)X\NUL\248\255\255\255\232*X\NUL\248\255\255\255X;X\NULhbin\NUL\DLE\NUL\NUL\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
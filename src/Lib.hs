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


type FunSet = (Word32 -> Bool)

hoist :: forall a. Maybe a -> RegParser a 
hoist m =  lift (MaybeT . pure $ m)

upon :: Bool -> RegParser a -> RegParser a 
upon b m = if b then m else mzero 

exclude :: (Word32,Word32) -> (Word32 -> Bool) -> (Word32 -> Bool)
exclude (start,len) old x = old x && not (x >= start && x <= start + len) 

type Parser st a = ReaderT st Get a 

type RegParser a = ReaderT (TVar RegEnv) (MaybeT IO) a 

parseReg :: RegParser a -> TVar RegEnv -> IO (Maybe a)
parseReg m e = runMaybeT $ runReaderT m e    

data RegEnv = RegEnv {_dataOffset :: Word32 
                     ,_parsed     :: M.Map Word32 (Either String CellContent) 
                     ,_rawHive    :: BS.ByteString 
                     ,_unparsed   :: FunSet}
makeLenses ''RegEnv 

nothing :: forall a. RegParser a 
nothing = mzero 

dataStart :: MonadIO m => ReaderT (TVar RegEnv) m Word32
dataStart = ask >>= (liftIO . readTVarIO) >>= pure . view dataOffset 

data Sized ::  Type -> Type where 
  Sized:: Word32 -> a -> Sized a  

rawLength :: RegParser Int 
rawLength = ask >>= (liftIO . readTVarIO) >>= \e -> pure $ BS.length (e ^. rawHive) 

getHive :: RegParser BS.ByteString 
getHive = ask >>= (liftIO . readTVarIO) >>= \e -> pure $ e ^. rawHive 

initEnv :: Word32 -> BS.ByteString -> IO (TVar RegEnv)
initEnv off bs = newTVarIO $ RegEnv {_dataOffset = off 
                                    ,_parsed = M.empty 
                                    ,_rawHive =  bs 
                                    ,_unparsed = const True}

logError :: Word32 -> String -> RegParser ()
logError loc err = ask >>= \e -> liftIO . atomically . modifyTVar' e $ 
  over parsed (M.insert loc (Left err))

excludeM :: (Word32,Word32) -> RegParser ()
excludeM w = ask >>= \e -> liftIO . atomically . modifyTVar' e $ 
  over unparsed (exclude w)

updateParsed :: forall a. IsCC a =>  Word32 -> a -> RegParser ()
updateParsed w c = ask >>= \e -> liftIO . atomically . modifyTVar' e $ 
  over parsed (M.insert w (Right $ cc @a # c))

lookupParsed :: Word32 -> RegParser (Maybe CellContent)
lookupParsed w = ask >>= (liftIO . readTVarIO) >>= \e -> pure $ e ^? (parsed . ix w . _Right)

nullPointer = 0xFFFFFFFF

maybeNull x = if x == nullPointer then Nothing else Just x  

okPtr :: Int -> Word32 -> Bool
okPtr len x = x /= nullPointer && x <= fromIntegral len  

w32 :: Get Word32
w32 = getWord32le 

w16 :: Get Word16 
w16 = getWord16le 

w32' :: Word32 -> RegParser Word32
w32' off = runGet w32 <$> dropM off >>= \case 
  Left err -> logError off err >> excludeM (off,4) >> mzero 
  Right w  -> pure w 

bytes' :: forall n. KnownNat n => Word32 -> RegParser (Bytes n)
bytes' off = runGet (bytes @n) <$> dropM off >>= \case 
  Left err -> logError off err >> excludeM (off,fromIntegral $ natVal (Proxy @n)) >> mzero 
  Right b  -> pure  b 

peekBytes :: forall n. KnownNat n => Word32 -> RegParser (Bytes n)
peekBytes off = runGet (lookAhead $ bytes @n) <$> dropM off >>= \case 
  Left err -> logError off err >> excludeM (off,fromIntegral $ natVal (Proxy @n)) >> mzero
  Right b  -> pure  b 

withSlice :: Word32 -> Word32 -> Get a -> RegParser a
withSlice off len f = (runGet f) <$> sliceM off len >>= \case 
  Left err -> logError off err >> excludeM (off,len) >> mzero 
  Right a  -> pure a 

  
parseSlice :: forall a. IsCC a => Word32 -> Word32 -> Get a -> RegParser a
parseSlice offset len p = do 
  mItem <- lookupParsed offset 
  case mItem of 
    Nothing -> do 
        slice <- sliceM offset len 
        case runGet p slice of 
          Left err -> trace ("slice parse error @" <> show offset <> "," <> show len) $ logError offset err >> excludeM (offset,len) >> hoist Nothing 
          Right c  -> updateParsed offset (cc @a # c) >> excludeM (offset,len) >> pure c
    Just item -> hoist $ item ^? cc @a

parseDrop :: forall a. IsCC a => Word32 -> Get (Sized a) -> RegParser a
parseDrop offset p = do 
  mItem <- lookupParsed offset 
  case mItem of 
    Nothing -> do 
      dropped <- dropM offset 
      case runGet p dropped of 
        Left err -> trace ("drop parse error @" <> show offset <> err) $ logError offset err >> excludeM (offset,offset) >> mzero
        Right (Sized len c) -> updateParsed offset (cc @a # c) >> excludeM (offset,len) >> pure  c
    Just item -> hoist $ item ^? cc @a

dropM ::  Word32 -> RegParser BS.ByteString
dropM offset = do 
  e <- ask >>= (liftIO . readTVarIO)
  let o = fromIntegral $ _dataOffset e 
  let rawhive  = BS.drop o  $ _rawHive e 
  pure . BS.drop (fromIntegral offset) $ rawhive 

sliceM :: Word32 -> Word32 -> RegParser BS.ByteString 
sliceM offset len = do 
  e <- ask >>= (liftIO . readTVarIO)
  let o = fromIntegral $ _dataOffset e  
  let rawhive   = BS.drop o $ _rawHive e 
  pure . BS.take (fromIntegral len) . BS.drop (fromIntegral offset) $ rawhive  


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


hiveBin ::  RegParser HiveBin 
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

hiveCell :: Word32 -> RegParser HiveCell
hiveCell offset = trace "hiveCell" $ w32' offset >>= \size ->
  cellContent (offset+4) >>= \x -> pure $ HiveCell size x  


  
cellContent :: Word32 -> RegParser CellContent
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

skRecord :: Get (Sized SKRecord) 
skRecord = trace "skRecord" $ do 
  magicNum <- bytes @2 
  unknown  <- bytes @2 
  offset1  <- bytes @4 
  offset2  <- bytes @4 
  refCount <- bytes @4 
  secDescSize <- bytes @4 
  secDescr    <- getBytes (fromIntegral $ toWord32le secDescSize)
  let size = 2 + 2 + 4 + 4 + 4 + 4 + toWord32le secDescSize
  pure . Sized size $ SKRecord magicNum unknown offset1 offset2 refCount secDescSize secDescr 

nkRecord :: Word32 -> RegParser NKRecord
nkRecord offset = trace ("nkRecord @" <> show offset) $ parseDrop offset nkBuilder >>= \nkb -> do 

    let stableSubkeys   = _stableSubkeys nkb
    let unstableSubkeys = _unstableSubkeys nkb
    let stablePtr       = _stableSubkeyPtr nkb
    let unstablePtr     = _unstableSubkeyPtr nkb

    len <- rawLength 

    
    
    subkeylist1      <- upon (stableSubkeys > 0 && okPtr len stablePtr)
                        $ trace ("parsing stable subkeylist @" <> show stablePtr) (cell @SubkeyList) (stablePtr - 32 )
                     

    --subkeylist2      <- trace "wham" $ upon (unstableSubkeys > 0 && okPtr len unstablePtr)
    --                    $ trace ("parsing volatile subkeylist @" <> show unstablePtr) (cell @SubkeyList) (unstablePtr)
                         

    valuelist       <- trace "bop" $ upon (_numValues nkb > 0 && okPtr len (_valueListPtr nkb))
                       $ parseSlice (_valueListPtr nkb) (_numValues nkb) valueList 
                       

    subkeys         <- V.concat <$> mapM subkeysWithList  [subkeylist1] -- ,subkeylist2]

    values          <- valuesWithList valuelist 

    let res = nkb {_subkeylistStable = Just subkeylist1
                  ,_subkeylistVol = Nothing
                  ,_valueList =  Just valuelist
                  ,_subkeys =  subkeys
                  ,_values  =  values}

    updateParsed offset res 

    pure  $ res 

 where 
   nkBuilder :: Get (Sized NKRecord)-- Get (Sized (Maybe SubkeyList -> Maybe SubkeyList -> Maybe ValueList -> V.Vector NKRecord -> V.Vector VKRecord -> NKRecord))
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
      pure . Sized (fromIntegral bRead) $ NKRecord 
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
            Nothing Nothing Nothing V.empty V.empty 

subkeysWithList :: SubkeyList -> RegParser (V.Vector NKRecord)
subkeysWithList skl = trace ("subkeysWithList: " <> pretty skl) $ V.foldM' go V.empty (_subkeyElems skl) 
  where 
    go :: V.Vector NKRecord -> SubkeyElem -> RegParser (V.Vector NKRecord)
    go acc ske =  case ske of 
          Ri off   ->  subkeyList off >>= \ skl -> subkeysWithList skl 
          Li off   -> getNK acc off 
          Lh off _ -> getNK acc off 
          Lf off _ -> getNK acc off 
    getNK :: V.Vector NKRecord -> Word32 -> RegParser (V.Vector NKRecord )
    getNK acc off =  nkRecord off >>= \nk -> pure $ acc `V.snoc` nk 


valuesWithList :: ValueList -> RegParser (V.Vector VKRecord)
valuesWithList vl = trace "valuesWithList" $ V.foldM' go V.empty vl 
  where 
    go :: V.Vector VKRecord -> Word32 -> RegParser (V.Vector VKRecord) 
    go acc off = vkRecord off >>= \vk  -> pure $ acc `V.snoc` vk 
         

vkRecord :: Word32 -> RegParser VKRecord
vkRecord off = trace "vkRecord" $ parseDrop off vkBuilder >>= \vkb -> 
    withSlice (_dataPtr vkb) (_dataLength vkb) (value $ _valueType vkb) >>= \v -> 
      let res = vkb {_value = v}
      in  updateParsed off res >> pure res 
 where 
   vkBuilder :: Get (Sized (VKRecord))
   vkBuilder = do 
      magicNum  <- bytes @2 
      nameLen   <- w16
      dataLen   <- w32
      dataPtr   <- w32
      valType   <- w32
      nameFlags <- bytes @2 
      unknown   <- bytes @2 
      valName   <- getBytes (fromIntegral  nameLen)
      bRead     <- fromIntegral <$> bytesRead 
      pure . Sized bRead $ VKRecord magicNum nameLen dataLen dataPtr valType nameFlags unknown valName (REG_NONE BS.empty)

subkeyList :: Offset -> RegParser SubkeyList
subkeyList off = trace ("subkeylist @" <> show (off)) $ parseDrop (off) skBuilder 
  where 
    skBuilder :: Get (Sized SubkeyList)
    skBuilder =  trace "subkeyList" $ do 
      magicNum     <- elemMagic -- bytes @2 
      numElems     <- w16
      elems        <- trace ("numElems: " <> show numElems) $ if numElems == 0 then pure [] else replicateM (fromIntegral numElems) (subkeyElem magicNum)
      bRead        <- trace "boop" $ fromIntegral <$> bytesRead 
      pure . Sized bRead $ SubkeyList magicNum numElems (V.fromList elems) 

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


cell :: forall c. IsCC c => Offset -> RegParser  c
cell o = trace ("cell: " <> show o ) $ hiveCell o >>= \(HiveCell s c)  -> hoist $  c ^? cc @c  


valueList :: Get ValueList 
valueList = V.fromList <$> some w32

rawDataBlocks :: Get BS.ByteString 
rawDataBlocks = allRemainingBytes -- hopefully it works out like this

allRemainingBytes = remaining >>= getBytes
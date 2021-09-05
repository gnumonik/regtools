module Explore.Optics.NK where



import Explore.ExploreM
import Types
import Lib
import qualified Data.Vector as V
import Data.Word (Word32)
import Control.Lens ( (^?), (^.), to )
import Explore.Optics.Root
import Explore.Optics.General
import Control.Monad.Look
import Control.Lens.Action
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Serialize
import Debug.Trace
import Explore.Optics.VK
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Control.Lens.Getter (Getter)


stblSubkeys :: MFold NKRecord [NKRecord]
stblSubkeys = mkMonadicFold go 
  where 
    f ::  HiveData -> [NKRecord] -> SubkeyElem -> [NKRecord]
    f hData acc ske = case ske of 
      Ri w -> case hData ^? (hiveCell w . content @SubkeyList) of 
                Nothing -> acc 
                Just skl -> acc <> V.foldl' (f hData) [] (skl ^. subkeyElems) 
      Li w    -> lookupChild hData acc w 
      Lf w w2 -> lookupChild hData acc w 
      Lh w w2 -> lookupChild hData acc w 

    lookupChild :: HiveData -> [NKRecord] -> Word32 -> [NKRecord]
    lookupChild hData acc w = case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> acc
               Just nk -> nk : acc  

    go :: NKRecord -> ExploreM [NKRecord]
    go nk = if ok 
            then look >>= \hData -> case hData ^? (hiveCell stblPtr . content @SubkeyList) of   
                          Nothing -> pure []
                          Just skl -> pure $ V.foldl' (f hData)  [] (skl ^. subkeyElems) 
            else pure []
      where 
            stblPtr = nk ^. stableSubkeyPtr
            numSK   = _stableSubkeys nk
            ok      = numSK /= 0 && stblPtr /= 0 && stblPtr /= (maxBound :: Word32)

volSubkeys :: MFold NKRecord [NKRecord]
volSubkeys = mkMonadicFold go 
  where
    
    f ::  HiveData -> [NKRecord] -> SubkeyElem -> [NKRecord]
    f hData acc ske = case ske of 
      Ri w -> case hData ^? (hiveCell w . content @SubkeyList) of 
                Nothing -> acc 
                Just skl -> acc <> V.foldl' (f hData) [] (skl ^. subkeyElems) 
      Li w    -> lookupChild hData acc w 
      Lf w w2 -> lookupChild hData acc w 
      Lh w w2 -> lookupChild hData acc w 

    lookupChild :: HiveData -> [NKRecord] -> Word32 -> [NKRecord]
    lookupChild hData acc w = case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> acc
               Just nk -> nk : acc  

    go :: NKRecord -> ExploreM [NKRecord]
    go nk = if ok 
            then look >>= \hData -> case hData ^? (hiveCell volPtr . content @SubkeyList) of   
                          Nothing -> pure []
                          Just skl -> pure $ V.foldl' (f hData)  [] (skl ^. subkeyElems) 
            else pure []
      where 
            volPtr  = nk ^. unstableSubkeyPtr
            numSK   = _unstableSubkeys nk
            ok      = numSK /= 0 && volPtr /= 0 && volPtr /= (maxBound :: Word32)

allSubkeys :: MFold NKRecord [NKRecord]
allSubkeys = mkMonadicFold $ \nk -> do 
  stbl <- fromMaybe [] <$> nk ^!? stblSubkeys 
  vol  <- fromMaybe [] <$> nk ^!? volSubkeys
  pure $ stbl <> vol 


-- | Get all value-record cells pointed to by the focused NKRecord 
vks :: MFold NKRecord [VKRecord]
vks = mkMonadicFold go 
  where 
    go :: NKRecord -> ExploreM [VKRecord]
    go nk = if ok 
            then look >>= \hData -> 
              case hData ^? (hiveCell vlPtr . content @ValueList) of 
                        Nothing -> pure [] 
                        Just vList -> vList ^!? (lookupMany @VKRecord) >>= \case 
                          Nothing -> pure [] 
                          Just xs -> pure xs 
            else pure []
      where 
        vlPtr = nk ^. valueListPtr 
        numVs = nk ^. numValues 
        ok    = numVs /= 0 && vlPtr /= 0 && vlPtr /= (maxBound :: Word32)

type Depth = Word32

-- | Takes a depth argument and produces a RegistryKey data type 
--   where the "root" is the input NKRecord. The RegistryKey contains 
--   subkeys of the input NKRecord up to 
--   the designated depth (if they exist)
kvs :: Depth -> MFold NKRecord RegistryKey
kvs depth = mkMonadicFold runKVs 
  where 

    runKVs :: NKRecord -> ExploreM RegistryKey 
    runKVs nk = do
      hData <- look  
      let acc = hData ^. getParentPath nk  
      getKVs 0 acc nk

    go :: [BS.ByteString] -> Depth -> MFold NKRecord RegistryKey 
    go acc d = mkMonadicFold (getKVs d acc)

    getKVs :: Depth -> [BS.ByteString] -> NKRecord -> ExploreM RegistryKey 
    getKVs d acc nk = do 
      let keyStr = nk ^. keyString 
      let timeStamp = nk ^. nkTimeStamp 
      myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped namedVal)
      children <- getChildren (keyStr:acc) (d+1) nk 
      pure $ RegistryKey keyStr (reverse acc) timeStamp myVals children 

    getChildren :: [BS.ByteString] -> Depth -> NKRecord -> ExploreM [RegistryKey]
    getChildren acc d nk 
      | d > depth = pure []
      | otherwise = do 
          sks1 <- fromMaybe [] <$> nk ^!? stblSubkeys 
          sks2 <- fromMaybe [] <$> nk ^!? volSubkeys 
          let allSks = sks1 <> sks2
          case allSks of 
            [] -> pure []
            sks ->  fromMaybe [] <$> sks ^!? mapped (go acc d)

allKVs :: MFold NKRecord RegistryKey
allKVs = kvs (maxBound :: Word32)

getParentPath :: NKRecord -> Getter HiveData [BS.ByteString] 
getParentPath nk = to runPath  
  where 
    runPath :: HiveData ->  [BS.ByteString]
    runPath hData =  go hData [] (nk ^. nkOffset1)

    go :: HiveData -> [BS.ByteString] -> Word32 -> [BS.ByteString]
    go hData acc w 
      | w == (maxBound :: Word32) || w == 0 = reverse acc 
      | otherwise = case hData ^? (hiveCell w . content @NKRecord) of 
          Nothing -> reverse acc 
          Just nk -> let acc' = (nk ^. keyString):acc 
                     in go hData acc' (nk ^. nkOffset1)

-- Breaks the organizing scheme but idk where else to put it 

-- | Take a list of Strings representing a key path 
--   and search the registry for the key at the 
--   designated path  
keyPath :: [String] -> Query NKRecord
keyPath kpath = mkMonadicFold (runKeyPath kpath) 
  where 
    runKeyPath :: [String] -> HiveData -> ExploreM NKRecord
    runKeyPath [] _ = qErr $ QueryError 0 "Error: Empty key path"
    runKeyPath xs hData = hData ^!? rootCell >>= \case 
      Nothing -> qErr $ QueryError 0 "Error: Root cell not found in Key Path lookup query" 
      Just root -> go root xs 

    go :: NKRecord -> [String] -> ExploreM NKRecord
    go nk []       = pure $ nk 
    go nk (kp:kps) 
      = nk ^!? (allSubkeys . selectFirst (\n -> n ^. keyString == BC.pack kp)) >>= \case 
          Just nextNK -> go nextNK kps 
          Nothing -> qErr . QueryError 0 $ T.pack ("The key: " <> kp <> " was not found in the volatile or stable subkeylist of its designated parent")



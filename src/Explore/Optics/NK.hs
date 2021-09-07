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
import qualified Data.List.NonEmpty as NE

stblSubkeys :: MFold RegistryKey [RegistryKey]
stblSubkeys = mkMonadicFold go 
  where 
    f ::  HiveData -> [RegistryKey] -> SubkeyElem -> ExploreM [RegistryKey]
    f hData acc ske = case ske of 
      Ri w -> case hData ^? (hiveCell w . content @SubkeyList) of 
                Nothing -> pure acc 
                Just skl -> (acc <>) <$>  V.foldM' (f hData) [] (skl ^. subkeyElems) 
      Li w    -> lookupChild hData acc w 
      Lf w w2 -> lookupChild hData acc w 
      Lh w w2 -> lookupChild hData acc w 

    lookupChild :: HiveData -> [RegistryKey] -> Word32 -> ExploreM [RegistryKey]
    lookupChild hData acc w = case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> pure acc
               Just nk -> do 
                 rKey <- mkRKey hData nk 
                 pure $ (rKey : acc)  
      where 
        mkRKey :: HiveData -> NKRecord -> ExploreM RegistryKey
        mkRKey hData nk = do 
          let keyStr = nk ^. keyString 
          let timeStamp = nk ^. nkTimeStamp
          let parentPath = hData ^. (getParentPath nk) 
          myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped namedVal)
          pure $ RegistryKey keyStr parentPath timeStamp myVals Truncated nk

    go :: RegistryKey -> ExploreM [RegistryKey]
    go (RegistryKey kn kp kt kv sks nk) = if ok 
            then look >>= \hData -> case hData ^? (hiveCell stblPtr . content @SubkeyList) of   
                          Nothing -> pure []
                          Just skl -> V.foldM' (f hData)  [] (skl ^. subkeyElems) 
            else pure []
      where 
            stblPtr = nk ^. stableSubkeyPtr
            numSK   = _stableSubkeys nk
            ok      = numSK /= 0 && stblPtr /= 0 && stblPtr /= (maxBound :: Word32)

volSubkeys :: MFold RegistryKey [RegistryKey]
volSubkeys = mkMonadicFold go 
  where 
    f ::  HiveData -> [RegistryKey] -> SubkeyElem -> ExploreM [RegistryKey]
    f hData acc ske = case ske of 
      Ri w -> case hData ^? (hiveCell w . content @SubkeyList) of 
                Nothing -> pure acc 
                Just skl -> (acc <>) <$>  V.foldM' (f hData) [] (skl ^. subkeyElems) 
      Li w    -> lookupChild hData acc w 
      Lf w w2 -> lookupChild hData acc w 
      Lh w w2 -> lookupChild hData acc w 

    lookupChild :: HiveData -> [RegistryKey] -> Word32 -> ExploreM [RegistryKey]
    lookupChild hData acc w = case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> pure acc
               Just nk -> do 
                 rKey <- mkRKey hData nk 
                 pure $ (rKey : acc)  
      where 
        mkRKey :: HiveData -> NKRecord -> ExploreM RegistryKey
        mkRKey hData nk = do 
          let keyStr = nk ^. keyString 
          let timeStamp = nk ^. nkTimeStamp
          let parentPath = hData ^. (getParentPath nk) 
          myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped namedVal)
          pure $ RegistryKey keyStr parentPath timeStamp myVals Truncated nk

    go :: RegistryKey -> ExploreM [RegistryKey]
    go (RegistryKey kn kp kt kv sks nk) = if ok 
            then look >>= \hData -> case hData ^? (hiveCell volPtr . content @SubkeyList) of   
                          Nothing -> pure []
                          Just skl -> V.foldM' (f hData)  [] (skl ^. subkeyElems) 
            else pure []
      where 
            volPtr  = nk ^. unstableSubkeyPtr
            numSK   = _unstableSubkeys nk
            ok      = numSK /= 0 && volPtr /= 0 && volPtr /= (maxBound :: Word32)

allSubkeys :: MFold RegistryKey [RegistryKey]
allSubkeys = mkMonadicFold $ \nk -> do 
  stbl <- fromMaybe [] <$> nk ^!? stblSubkeys 
  vol  <- fromMaybe [] <$> nk ^!? volSubkeys
  pure $ stbl <> vol 



type Depth = Word32

-- | Takes a depth argument and produces a RegistryKey data type 
--   where the "root" is the input RegistryKey. The output key contains 
--   subkeys of the input Key up to the designated depth (if they exist)
kvs :: Depth -> MFold RegistryKey RegistryKey
kvs depth = mkMonadicFold runKVs 
  where 

    runKVs :: RegistryKey -> ExploreM RegistryKey 
    runKVs rKey@(RegistryKey _ _ _ _ _ nk) = do
      hData <- look  
      let acc = hData ^. getParentPath nk  
      getKVs 0 acc rKey

    go :: [BS.ByteString] -> Depth -> MFold RegistryKey RegistryKey 
    go acc d = mkMonadicFold (getKVs d acc)

    getKVs :: Depth -> [BS.ByteString] -> RegistryKey -> ExploreM RegistryKey 
    getKVs d acc rKey@(RegistryKey _ _ _ _ _ nk) = do 
      let keyStr = nk ^. keyString 
      let timeStamp = nk ^. nkTimeStamp 
      myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped namedVal)
      children <- getChildren (keyStr:acc) (d+1) rKey 
      pure $ RegistryKey keyStr (reverse acc) timeStamp myVals children nk

    getChildren :: [BS.ByteString] -> Depth -> RegistryKey -> ExploreM SubkeyData
    getChildren acc d nk 
      | d > depth = pure Truncated 
      | otherwise = do 
          sks1 <- fromMaybe [] <$> nk ^!? stblSubkeys 
          sks2 <- fromMaybe [] <$> nk ^!? volSubkeys 
          let allSks = sks1 <> sks2
          case allSks of 
            [] -> pure Empty
            sks ->  sks ^!? mapped (go acc d) >>= \case 
                Nothing -> pure Empty 
                Just sksxs -> case NE.nonEmpty sksxs of 
                  Nothing -> pure Empty 
                  Just ne' -> pure $ Subkeys ne' 


allKVs :: MFold RegistryKey RegistryKey
allKVs = kvs (maxBound :: Word32)

-- | Take a list of Strings representing a key path 
--   and search the registry for the key at the 
--   designated path  
keyPath :: [String] -> Query RegistryKey 
keyPath kpath = mkMonadicFold (runKeyPath kpath) 
  where 
    runKeyPath :: [String] -> HiveData -> ExploreM RegistryKey
    runKeyPath [] _ = qErr $ QueryError 0 "Error: Empty key path"
    runKeyPath xs hData = hData ^!? rootCell >>= \case 
      Nothing -> qErr $ QueryError 0 "Error: Root cell not found in Key Path lookup query" 
      Just root -> do 
        rKey <- mkRKey hData (root ^. keyNode) 
        go rKey xs 

    go :: RegistryKey -> [String] -> ExploreM RegistryKey
    go rk []       = pure rk 
    go rk@(RegistryKey _ _ _ _ _ nk) (kp:kps) 
      = rk ^!? (allSubkeys . selectFirst (\n -> n ^. (keyNode . keyString )== BC.pack kp)) >>= \case 
          Just nextNK -> go nextNK kps 
          Nothing -> qErr . QueryError 0 $ T.pack ("The key: " <> kp <> " was not found in the volatile or stable subkeylist of its designated parent")

    mkRKey :: HiveData -> NKRecord -> ExploreM RegistryKey
    mkRKey hData nk = do 
          let keyStr = nk ^. keyString 
          let timeStamp = nk ^. nkTimeStamp
          let parentPath = hData ^. (getParentPath nk) 
          myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped namedVal)
          pure $ RegistryKey keyStr parentPath timeStamp myVals Truncated nk
          
matchKeyName :: BS.ByteString -> MFold [RegistryKey] [RegistryKey]
matchKeyName bs = select (nameContains bs)

nameContains :: BS.ByteString -> RegistryKey -> Bool 
nameContains bs rk = containsSubstring bs (_keyNode rk ^. keyString)

valNameContains :: BS.ByteString -> RegistryKey -> Bool 
valNameContains bs rKey = or $ map (containsSubstring bs . fst) (rKey ^. keyValues)

valDataContains :: BS.ByteString -> BS.ByteString -> RegistryKey -> Bool 
valDataContains nmStr dataStr rKey 
  = let matchNm = filter (containsSubstring nmStr . fst) (rKey ^. keyValues)
    in not . null $ filter (go . snd) matchNm  
 where 
    go :: Value -> Bool
    go = \case 
        REG_NONE bs                       -> f bs 
        REG_SZ bs                         -> f bs 
        REG_EXPAND_SZ bs                  -> f bs 
        REG_BINARY bs                     -> f bs
        REG_DWORD w                       -> f (runPut . putWord32le $ w)
        REG_DWORD_LITTLE_ENDIAN w         -> f (runPut . putWord32le $ w)
        REG_DWORD_BIG_ENDIAN w            -> f (runPut . putWord32be $ w)
        REG_LINK bs                       -> f bs 
        REG_MULTI_SZ bs                   -> f bs 
        REG_RESOURCE_LIST bs              -> f bs 
        REG_FULL_RESOURCE_DESCRIPTOR bs   -> f bs 
        REG_RESOURCE_REQUIREMENTS_LIST bs -> f bs 
        REG_QWORD w                       -> f (runPut . putWord64le $ w)
    f = containsSubstring dataStr
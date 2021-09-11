module Explore.Optics.NK where



import Explore.ExploreM
import Types
import Lib
import qualified Data.Vector as V
import Data.Word (Word32)
import Control.Lens ( (^?), (^.), to, set )
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
import Explore.Optics.Utils
import Control.Monad.IO.Class
import Hash (hashKey, hashVal)
import qualified Data.List.NonEmpty as NE 
import qualified Data.Map.Strict as M
import Data.List (find)
import qualified Data.Text.Encoding as TE

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
      hData <- look 
      let keyStr = nk ^. keyString 
      let timeStamp = nk ^. nkTimeStamp 
      let parentPath = hData ^. (getParentPath nk)
      let valPath    = BS.concat parentPath <> BC.pack "\\" <> keyStr 
      myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped (namedVal valPath))
      children <- getChildren (keyStr:acc) (d+1) rKey 
      pure $ RegistryKey keyStr (reverse acc) timeStamp  myVals children nk

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
keyPath :: [String] -> MFold RegistryKey [RegistryKey] 
keyPath kpath = mkMonadicFold (runKeyPath kpath) 
  where 
    runKeyPath :: [String] -> RegistryKey -> ExploreM [RegistryKey]
    runKeyPath [] _ = qErr $ QueryError 0 "Error: Empty key path"
    runKeyPath xs@(z:zs) rKey
      | z == "<$ROOT$>" && not (null zs) = look >>= \hData ->  hData ^!? rootCell >>= \case 
          Nothing -> qErr $ QueryError 0 "Error: Root cell not found in Key Path lookup query" 
          Just root -> go root zs 
      
      | z == "<$ROOT$>" && null zs = look >>= \hData ->  hData ^!? rootCell >>= \case 
          Nothing -> qErr $ QueryError 0 "Error: Root cell not found in Key Path lookup query" 
          Just root -> pure [root] 

      | otherwise = go rKey xs 

    go :: RegistryKey -> [String] -> ExploreM [RegistryKey]
    go rk []       = pure [rk] 
    go rk@(RegistryKey _ _ _ _ _ nk) (kp:kps) 
      = rk ^!? (allSubkeys . selectFirst (\n -> n ^. (keyNode . keyString ) == BC.pack kp)) >>= \case 
          Just nextNK -> go nextNK kps 
          Nothing -> do 
            f <- getPrinter 
            liftIO . f $ "The key: [" <> kp <> "] was not found in the volatile or stable subkeylist of its designated parent"
                       <> "\n(I.e. the `key` query returned an empty list."
            pure []

checkHash :: KeyHash ->  ExploreM ()
checkHash kHash  = do
  root <- getRootCell 
  f <- getPrinter   
  let kPath = toKeyPath . T.unpack $ kName 
  root ^!? keyPath kPath >>= \case 
    Just [rKey] -> do 
      let kHash' = hashKey rKey
      if kHash == kHash' 
        then pure ()
        else diffHash f kHash kHash'
    _ -> liftIO . f $ "The Key @" <> show kPath <> " cannot be found in the Registry"  
 where 
   kName = kHash ^. hshKeyName
   diffHash :: (String -> IO ()) -> KeyHash -> KeyHash -> ExploreM ()
   diffHash f k1 k2 = diffTime >> diffVals (k1 ^. valuesHash) (k2 ^. valuesHash)
    where 
      diffTime = if (k1 ^. timeHash) /= (k2 ^. timeHash)
                 then liftIO . f . T.unpack $ kName <> " - " <> "TimeStamp changed"
                 else pure ()

      diffVals vxs vys 
        = if (k1 ^. valuesHash) /= (k2 ^. valuesHash)
            then do 
              liftIO . f . T.unpack $ kName <> " - " <> "Values changed"
              liftIO . f $ ""
              mapM_ checkValHash vxs 
            else pure ()

-- this is unnecessarily inefficient and should be fixed 
checkValHash :: ValHash -> ExploreM () 
checkValHash vh = do 
  f     <- getPrinter 
  root <- getRootCell 
  root ^!? keyPath (toKeyPath . T.unpack $ vh ^. vHashPath) >>= \case  
    Just [rKey] -> liftIO $ do 
      case find (\x -> x ^. vHashName == vh ^. vHashName) (map hashVal $ rKey ^. keyValues) of 
        Nothing -> f $ "The value named " 
                    <> T.unpack (vh ^. vHashName) 
                    <> " located at "
                    <> T.unpack (vh ^. vHashPath)
                    <> " no longer exists"
        Just vh' -> let hashedData' = vh' ^. vHashData 
                    in if hashedData' == (vh ^. vHashData)
                       then pure ()
                       else do 
                         f $ "The value named "
                             <> T.unpack (vh ^. vHashName)
                             <> " located at "
                             <> T.unpack (vh ^. vHashPath) 
                             <> " has changed." 
    _  -> liftIO . f $ "The parent key of the value named " 
                      <> T.unpack (vh ^. vHashName) 
                      <> " located at "
                      <> T.unpack (vh ^. vHashPath) 
                      <> " no longer exists"

mkSubKeys :: [RegistryKey] -> SubkeyData 
mkSubKeys rks = maybe Empty Subkeys (NE.nonEmpty rks)

filterSubkeys :: MFold RegistryKey Bool -> MFold RegistryKey RegistryKey 
filterSubkeys p = mkMonadicFold go 
  where 
    go :: RegistryKey -> ExploreM RegistryKey 
    go rKey = do 
      rKey ^!? allSubkeys >>= \case 
        Nothing -> pure rKey 
        Just sks -> do 
          sks' <- catMaybes <$> mapM (\x -> x ^!? p >>= \case {Just True -> pure $ Just x ; _ -> pure Nothing}) sks 
          pure $ set subkeys (mkSubKeys sks) rKey  

filterValues :: MFold FQValue Bool -> MFold RegistryKey RegistryKey 
filterValues p = mkMonadicFold go 
  where 
    go :: RegistryKey -> ExploreM RegistryKey 
    go rKey = do 
      let vals = rKey ^. keyValues
      newVals <- catMaybes 
                <$> forM vals (\x -> x ^!? p >>= \case 
                                  Just True -> pure $ Just x
                                  _ -> pure Nothing)
      pure $ set keyValues newVals rKey   
      
          
matchKeyName :: BS.ByteString -> MFold [RegistryKey] [RegistryKey]
matchKeyName bs = select (nameContains bs)

nameContains :: BS.ByteString -> RegistryKey -> Bool 
nameContains bs rk = containsSubstring bs (_keyNode rk ^. keyString)

valNameContains :: BS.ByteString -> FQValue -> Bool 
valNameContains bs fqVal = containsSubstring bs (fqVal ^. fqValName)

valDataContains :: T.Text -> FQValue -> Bool 
valDataContains dataStr fqVal 
  = go (fqVal ^. fqValData)
 where 
    go :: Value -> Bool
    go = \case 
        REG_NONE bs                       -> f bs 
        REG_SZ bs                         -> u16 bs 
        REG_EXPAND_SZ bs                  -> u16 bs 
        REG_BINARY bs                     -> f bs
        REG_DWORD w                       -> f (runPut . putWord32le $ w)
        REG_DWORD_LITTLE_ENDIAN w         -> f (runPut . putWord32le $ w)
        REG_DWORD_BIG_ENDIAN w            -> f (runPut . putWord32be $ w)
        REG_LINK bs                       -> u16 bs 
        REG_MULTI_SZ bs                   -> f bs 
        REG_RESOURCE_LIST bs              -> f bs 
        REG_FULL_RESOURCE_DESCRIPTOR bs   -> f bs 
        REG_RESOURCE_REQUIREMENTS_LIST bs -> f bs 
        REG_QWORD w                       -> f (runPut . putWord64le $ w)
    f = containsSubstring (TE.encodeUtf8 dataStr)

    u16 = containsSubstring (TE.encodeUtf16LE dataStr)

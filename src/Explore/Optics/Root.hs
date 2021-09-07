module Explore.Optics.Root where

import Types
import Data.Word (Word32)
import Control.Lens 
import Lib 
import Explore.ExploreM 
import Explore.Optics.General
import ParseM
import qualified Data.Map as M
import Control.Monad.Look
import Data.List (foldl')
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Vector as V
import Control.Lens.Action
import Data.Serialize
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Control.Monad.Errors.Class
import qualified Data.Sequence as S 

-- A bunch of these are written in a somewhat roundabout manner to improve the UX. 
-- (If you're familiar with Control.Lens these probably don't look like most of the other Folds 
-- you've seen, even sand the monadic effects)

-- | Requires a type application.
regItem :: forall a. IsCC a => Word32 -> Query a 
regItem w = mkMonadicFold go 
  where 
    go :: HiveData -> ExploreM a 
    go hData = case hData ^? (hiveCell w . content @a) of 
      Nothing -> qErr $ QueryError w "RegItem lookup failed. Target doesn't exist or is wrong type."
      Just a  -> pure a 

-- | Gets the root cell of the registry. 
rootCell :: Query RegistryKey 
rootCell = mkMonadicFold go 
  where 
    go :: HiveData -> ExploreM RegistryKey 
    go hData = do 
      let nk = hData ^? (hEnv . parsed . ix (hData ^. (hEnv . offset)) . content @NKRecord)
      printer <- getPrinter 
      case nk of 
        Nothing -> report . S.singleton $ QueryError 0 "Error: Root Cell Not Found"
        Just aNK -> mkRKey hData aNK 

    mkRKey :: HiveData -> NKRecord -> ExploreM RegistryKey
    mkRKey hData nk = do 
          let keyStr = nk ^. keyString 
          let timeStamp = nk ^. nkTimeStamp
          let parentPath = hData ^. (getParentPath nk) 
          myVals <- fromMaybe [] <$> nk ^!? (vks . concatMapped namedVal)
          pure $ RegistryKey keyStr parentPath timeStamp myVals Truncated nk

namedVal :: MFold VKRecord [(BS.ByteString,Value)]
namedVal = mkMonadicFold go 
  where 
    go :: VKRecord -> ExploreM [(BS.ByteString,Value)]
    go vk = let valNm = vk ^. valName
            in if checkVKPointer (vk ^. dataLength)
            then look >>= \hData -> 
              hData ^!? regItem @Value (vk ^. dataPtr) >>= \case 
                Nothing -> pure []
                Just vx -> pure [(valNm,vx)] 

            else case runGet (valueData 4 (vk ^. valueType)) (runPut . putWord32le $ vk ^. dataPtr) of 
                  Left err -> do 
                    printer <- getPrinter 
                    liftIO $ printer err
                    pure []
                  Right v  -> pure [(valNm,v)]
                  
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

hiveCell :: Word32 -> Fold HiveData HiveCell 
hiveCell i = folding go 
  where 
    go :: HiveData -> Maybe HiveCell 
    go hData = hData ^? (hEnv . parsed . ix i)

lookupMany :: forall a f. (IsCC a, Foldable f) => MFold (f Word32)  [a]
lookupMany = mkMonadicFold $ \as -> look >>= \hData -> pure $ foldl' (go hData) [] as 
  where 
    go :: HiveData -> [a] -> Word32 -> [a]
    go hData acc w = case hData ^? (hiveCell w . content @a) of 
      Nothing -> acc 
      Just a  -> a:acc 

-- | Insanely slow. Probably no way to make it appreciably faster. 
findCell :: forall a. IsCC a => (a -> Bool) -> Query [a]
findCell p  = mkMonadicFold go 
  where 
    go :: HiveData  -> ExploreM [a]
    go hData =  pure . map snd $ M.foldlWithKey f [] (hData ^. (hEnv . parsed))
      where 
        f :: [(Word32,a)] -> Word32 -> HiveCell -> [(Word32,a)]
        f acc i c = case c ^? content @a of
          Nothing -> acc 
          Just a  -> if p a then (i,a):acc else acc 


deadZones :: Word32 -> Query [(Word32,Word32)]
deadZones  minSize = mkMonadicFold runDeadZones 

  where 
    runDeadZones :: HiveData -> ExploreM [(Word32,Word32)] 
    runDeadZones hData =case occupied hData of 
        [] -> pure []
        (x:xs) -> pure $ (go hData) [] (snd x) xs

    occupied hData = hData ^. (hEnv . spaceMap . andThen (M.toList . getSpace))
    
    go :: HiveData -> [(Word32,Word32)] -> Word32 -> [(Word32, Word32)] -> [(Word32,Word32)]
    go _ acc _ [] = acc 
    go hData acc lastEnd ((nextStart,nextEnd):rest) 
      | (lastEnd+1) == nextStart = go hData acc nextEnd rest 
      | abs (nextStart - lastEnd) < minSize = go hData acc nextEnd rest 
      | otherwise            = go hData ((lastEnd+1,nextStart-1):acc) nextEnd rest       

nonEmptyDeadZoneData :: Word32 -> Query [(Word32,Word32,BS.ByteString)]
nonEmptyDeadZoneData minSize = mkMonadicFold runThisThing 
  where 
    runThisThing :: HiveData -> ExploreM [(Word32,Word32,BS.ByteString)]
    runThisThing hData = hData ^!? deadZones minSize >>= \case 
      Nothing -> pure [] 
      Just dzs -> pure $ foldl' (go hData) [] $ dzs 

    go :: HiveData -> [(Word32,Word32,BS.ByteString)]-> (Word32, Word32) -> [(Word32,Word32,BS.ByteString)]
    go hData acc (s,e) = let bs = hData ^. (hEnv . rawBS . andThen (BS.take (fromIntegral $ e-s) . BS.drop (fromIntegral s)))
                   in  if BS.all (== fromIntegral (ord '\NUL')) bs 
                       then acc 
                       else (s,e,bs):acc


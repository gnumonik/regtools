module Explore where

import Data.Word 
import Types hiding (stableSubkeys)
import Lib ( HiveData, checkVKPointer, valueData ) 
import ParseM 
import Control.Lens 
import qualified Data.Map as M 
import qualified Data.Vector as V 
import Data.Foldable (Foldable(foldl'))
import qualified Data.ByteString as BS
import Data.Kind 
import Debug.Trace 
import Data.Char (ord)
import Control.Monad.State 
import Control.Monad.Reader 
import Data.Serialize
import Data.Maybe (catMaybes, mapMaybe)
import Control.Lens.Action 

makeLenses ''HiveData 

data ExploreState :: Type -> Type  where 
   ExploreState :: IsCC a => {
    _hData :: HiveData 
  , _focus :: a 
} -> ExploreState a 

type ExploreM = ReaderT HiveData IO 

type MFold s a = MonadicFold ExploreM s a 

newtype NotFound = NotFound Word32 deriving (Show, Eq, Ord)

newtype WrongType = WrongType Word32 deriving (Show, Eq, Ord)

query :: Monad m => r -> Acting (ReaderT r m) (Leftmost a) r a -> m (Maybe a)
query reg l = runReaderT (reg ^!? l) reg 

mkMonadicFold :: (s -> m a) -> MonadicFold m s a 
mkMonadicFold = act 


takin' :: Int -> Fold [a] [a]
takin' n = folding (Identity . take n)

pPrint :: Pretty a => MFold a ()
pPrint = mkMonadicFold $ liftIO . putStrLn . ("\n\n" <>) . pretty 

rootCell :: Fold HiveData NKRecord 
rootCell = folding go 
  where 
    go :: HiveData -> Maybe NKRecord 
    go hData = hData ^? (hEnv . parsed . ix (hData ^. (hEnv . offset)) . content @NKRecord)

hiveCell :: Word32 -> Fold HiveData HiveCell 
hiveCell i = folding go 
  where 
    go :: HiveData -> Maybe HiveCell 
    go hData = hData ^? (hEnv . parsed . ix i)

content :: forall a. IsCC a => Fold HiveCell a 
content = folding go 
  where
    go :: HiveCell -> Maybe a 
    go (HiveCell s c) = c ^? cc @a 

regItem :: forall a. IsCC a => Word32 -> Fold HiveData a 
regItem w = folding (\hData -> hData ^? (hiveCell w . content @a))

andThen :: forall a b. (a -> b) -> Fold a b 
andThen f = folding (\a -> Identity $ f a)

andThenM :: forall a b. (a -> ExploreM b) -> MFold a b 
andThenM = mkMonadicFold  

mappin' :: forall a b. (a -> b) -> Fold [a] [b]
mappin' f = andThen (map f) 

mapped :: forall a b. (MFold a b) -> MFold [a] [b]
mapped f = mkMonadicFold go 
  where 
    go :: [a] -> ExploreM [b]
    go xs = catMaybes <$> mapM (^!? f) xs

concatMapped :: forall a b. (Fold a [b]) -> Fold [a] [b]
concatMapped f = folding go 
  where 
    go xs = Just . concat $ mapMaybe (^? f) xs 


stableSubkeys :: MFold NKRecord [NKRecord]
stableSubkeys = mkMonadicFold go 
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
            then ask >>= \hData -> case hData ^? (hiveCell stblPtr . content @SubkeyList) of   
                          Nothing -> pure []
                          Just skl -> pure $ V.foldl' (f hData)  [] (skl ^. subkeyElems) 
            else pure []
      where 
            stblPtr = nk ^. stableSubkeyPtr
            numSK   = _stableSubkeys nk
            ok      = numSK /= 0 && stblPtr /= 0 && stblPtr /= (maxBound :: Word32)

volatileSubkeys :: MFold NKRecord [NKRecord]
volatileSubkeys = mkMonadicFold go 
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
            then ask >>= \hData -> case hData ^? (hiveCell volPtr . content @SubkeyList) of   
                          Nothing -> pure []
                          Just skl -> pure $ V.foldl' (f hData)  [] (skl ^. subkeyElems) 
            else pure []
      where 
            volPtr  = nk ^. stableSubkeyPtr
            numSK   = _stableSubkeys nk
            ok      = numSK /= 0 && volPtr /= 0 && volPtr /= (maxBound :: Word32)

vks :: MFold NKRecord [VKRecord]
vks = mkMonadicFold go 
  where 
    go :: NKRecord -> ExploreM [VKRecord]
    go nk = if ok 
            then ask >>= \hData -> 
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

val :: MFold VKRecord (Maybe Value) 
val  = mkMonadicFold go 
  where 
    go :: VKRecord -> ExploreM (Maybe Value) 
    go vk = if checkVKPointer (vk ^. dataLength)
            then ask >>= \hData -> pure $ hData ^? regItem @Value (vk ^. dataPtr)
            else case runGet (valueData 4 (vk ^. valueType)) (runPut . putWord32le $ vk ^. dataPtr) of 
                  Left err -> trace err $ pure Nothing 
                  Right v  -> pure $ Just v 
 
lookupMany :: forall a f. (IsCC a, Foldable f) => MFold (f Word32)  [a]
lookupMany = mkMonadicFold $ \as -> ask >>= \hData -> pure $ foldl' (go hData) [] as 
  where 
    go :: HiveData -> [a] -> Word32 -> [a]
    go hData acc w = case hData ^? (hiveCell w . content @a) of 
      Nothing -> acc 
      Just a  -> a:acc 

findCell :: forall a. IsCC a => MFold (a -> Bool) [(Word32,a)]
findCell  = mkMonadicFold go 
  where 
    go :: (a -> Bool) -> ExploreM [(Word32,a)]
    go p = ask >>= \hData -> pure $ M.foldlWithKey f [] (hData ^. (hEnv . parsed))
      where 
        f :: [(Word32,a)] -> Word32 -> HiveCell -> [(Word32,a)]
        f acc i c = case c ^? content @a of
          Nothing -> acc 
          Just a  -> if p a then (i,a):acc else acc 

hasSubkeyElem :: HiveData -> Word32 -> SubkeyList -> Bool 
hasSubkeyElem hData w skl = go (V.toList $ skl ^. subkeyElems) 
  where 
    go :: [SubkeyElem] -> Bool
    go [] = False 
    go (x:xs) = case x of  
      Ri wx -> case hData ^? (regItem @SubkeyList wx) of 
                Nothing -> False 
                Just skl' -> hasSubkeyElem hData w skl' 
      Li wx    -> wx == w || go xs 
      Lf wx _  -> wx == w || go xs 
      Lh wx _  -> wx == w || go xs  

deadZones :: HiveData -> Word32 -> [(Word32,Word32)]
deadZones hData minSize = case occupied  of 
    [] -> []
    (x:xs) -> go [] (snd x) xs 
  where   
   occupied = hData ^. (hEnv . spaceMap . andThen (M.toList . getSpace))
   
   go :: [(Word32,Word32)] -> Word32 -> [(Word32, Word32)] -> [(Word32,Word32)]
   go acc _ [] = acc 
   go acc lastEnd ((nextStart,nextEnd):rest) 
    | (lastEnd+1) == nextStart = go acc nextEnd rest 
    | abs (nextStart - lastEnd) < minSize = go acc nextEnd rest 
    | otherwise            = go ((lastEnd+1,nextStart-1):acc) nextEnd rest       

nonEmptyDeadZoneData :: HiveData -> Word32 -> [(Word32,Word32,BS.ByteString)]
nonEmptyDeadZoneData hData minSize = foldl' go [] $ deadZones hData minSize 
  where 
    go :: [(Word32,Word32,BS.ByteString)]-> (Word32, Word32) -> [(Word32,Word32,BS.ByteString)]
    go acc (s,e) = let bs = hData ^. (hEnv . rawBS . andThen (BS.take (fromIntegral $ e-s) . BS.drop (fromIntegral s)))
                   in  if BS.all (== fromIntegral (ord '\NUL')) bs 
                       then acc 
                       else (s,e,bs):acc
                   

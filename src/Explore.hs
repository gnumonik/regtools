module Explore where

import Data.Word 
import Types 
import Lib 
import ParseM 
import Control.Lens 
import qualified Data.Map as M 
import qualified Data.Vector as V 
import Data.Foldable (Foldable(foldl'))
import qualified Data.ByteString as BS
import Debug.Trace 
import Data.Char (ord)
makeLenses ''HiveData 

newtype NotFound = NotFound Word32 deriving (Show, Eq, Ord)

newtype WrongType = WrongType Word32 deriving (Show, Eq, Ord)

rootCell :: Fold HiveData HiveCell 
rootCell = folding go 
  where 
    go :: HiveData -> Maybe HiveCell 
    go hData = hData ^? (hEnv . parsed . ix (hData ^. (hEnv . offset)))

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

mappin' :: forall a b. (a -> b) -> Fold [a] [b]
mappin' f = andThen (map f) 

stableChildren :: HiveData -> Fold NKRecord [Either NotFound NKRecord]
stableChildren hData = folding go 
  where 
    f ::  [Either NotFound NKRecord] -> SubkeyElem -> [Either NotFound NKRecord]
    f acc ske = case ske of 
      Ri w -> case hData ^? (hiveCell w . content @SubkeyList) of 
                Nothing -> acc 
                Just skl -> acc <> V.foldl' f [] (skl ^. subkeyElems) 
      Li w    -> lookupChild acc w 
      Lf w w2 -> lookupChild acc w 
      Lh w w2 -> lookupChild acc w 

    lookupChild :: [Either NotFound NKRecord] -> Word32 -> [Either NotFound NKRecord]
    lookupChild acc w = case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> Left (NotFound w) : acc
               Just nk -> Right nk : acc  

    go :: NKRecord -> Maybe [Either NotFound NKRecord]
    go nk = if ok 
            then Just $ case hData ^? (hiveCell stblPtr . content @SubkeyList) of 
                          Nothing -> [Left $ NotFound stblPtr]
                          Just skl -> V.foldl' f  [] (skl ^. subkeyElems) 
            else Nothing
      where 
            stblPtr = nk ^. stableSubkeyPtr
            numSK   = nk ^. stableSubkeys
            ok      = numSK /= 0 && stblPtr /= 0 && stblPtr /= (maxBound :: Word32)

volatileChildren :: HiveData -> Fold NKRecord [Either NotFound NKRecord]
volatileChildren hData = folding go 
  where 
    f ::  [Either NotFound NKRecord] -> SubkeyElem -> [Either NotFound NKRecord]
    f acc ske = case ske of 
      Ri w -> case hData ^? (hiveCell w . content @SubkeyList) of 
                Nothing -> acc 
                Just skl -> acc <> V.foldl' f [] (skl ^. subkeyElems) 
      Li w    -> lookupChild acc w 
      Lf w w2 -> lookupChild acc w 
      Lh w w2 -> lookupChild acc w 

    lookupChild :: [Either NotFound NKRecord] -> Word32 -> [Either NotFound NKRecord]
    lookupChild acc w = case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> Left (NotFound w) : acc
               Just nk -> Right nk : acc  

    go :: NKRecord -> Maybe [Either NotFound NKRecord]
    go nk = if ok 
            then Just $ case hData ^? (hiveCell volPtr . content @SubkeyList) of 
                          Nothing -> [Left $ NotFound volPtr]
                          Just skl -> V.foldl' f  [] (skl ^. subkeyElems) 
            else Nothing
      where 
            volPtr = nk ^. unstableSubkeyPtr
            numSK   = nk ^. unstableSubkeys
            ok      = numSK /= 0 && volPtr /= 0 && volPtr /= (maxBound :: Word32)

vks :: HiveData -> Fold NKRecord [VKRecord]
vks hData = folding go 
  where 
    go :: NKRecord -> Maybe [VKRecord]
    go nk = if ok 
            then case hData ^? (hiveCell vlPtr . content @ValueList) of 
                          Nothing -> Nothing
                          Just vList -> Just $ lookupMany @VKRecord hData vList 
            else Nothing 
      where 
        vlPtr = nk ^. valueListPtr 
        numVs = nk ^. numValues 
        ok    = numVs /= 0 && vlPtr /= 0 && vlPtr /= (maxBound :: Word32)

lookupMany :: forall a f. (IsCC a, Foldable f) => HiveData -> f Word32 -> [a]
lookupMany hData as = foldl' go [] as 
  where 
    go :: [a] -> Word32 -> [a]
    go acc w = case hData ^? (hiveCell w . content @a) of 
      Nothing -> acc 
      Just a  -> a:acc 

findCell :: forall a. IsCC a => HiveData -> (a -> Bool) -> [(Word32,a)]
findCell hData p = M.foldlWithKey go [] (hData ^. (hEnv . parsed))
  where 
    go :: [(Word32,a)] -> Word32 -> HiveCell -> [(Word32,a)]
    go acc i c = case c ^? content @a of
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
      Li wx -> wx == w || go xs 
      Lf wx _ -> wx == w || go xs 
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
    --  | lastEnd == nextStart     = go acc nextEnd rest 
    | otherwise            = go ((lastEnd+1,nextStart-1):acc) nextEnd rest       

nonEmptyDeadZoneData :: HiveData -> Word32 -> [(Word32,Word32,BS.ByteString)]
nonEmptyDeadZoneData hData minSize = foldl' go [] $ deadZones hData minSize 
  where 
    go :: [(Word32,Word32,BS.ByteString)]-> (Word32, Word32) -> [(Word32,Word32,BS.ByteString)]
    go acc (s,e) = let bs = hData ^. (hEnv . rawBS . andThen (BS.take (fromIntegral $ e-s) . BS.drop (fromIntegral s)))
                   in  if BS.all (== fromIntegral (ord '\NUL')) bs 
                       then acc 
                       else (s,e,bs):acc
                   

    {--
    start = hData ^. (hEnv . offset)

    f :: Word32 -> Bool 
    f = exclude (0,start) $ hData ^. (hEnv . unparsed)

    raw = BS.take 5000 $ hData ^. (hEnv . rawBS)

    len = fromIntegral $ BS.length raw 

    chunks = goChunks [] 0 

    nextChunk :: Word32 -> (Word32,Word32)
    nextChunk i 
      | i >= len = trace ("nextChunk reached end of BS at " <> show i) (i,i) 
      | f i = trace ("nextChunk calling firstEndPoint at " <> show i) (i,findEndPoint i (i+1)) 
      | otherwise = trace ("searching for start of next chunk @" <> show (i + 1)) nextChunk (i+1) 
  
    findEndPoint :: Word32 -> Word32 -> Word32 
    findEndPoint i i' 
      | i >= len  = trace ("findEndPoint reached end of BS at" <> show i)  i 
      | i' >= len = trace ("findEndPoint reached end of BS at" <> show i') i' 
      | f i'      = trace ("searching for endpoint @" <> show (i + 1))findEndPoint i' (i' +1)
      | otherwise = i  

    goChunks :: [(Word32,Word32)] -> Word32 -> [(Word32,Word32)]
    goChunks acc i 
      | i >= len = trace "goChunks ended" acc 
      | otherwise = let next@(x,y) = nextChunk i 
                    in  trace ("goChunks got a chunk " <> show next) goChunks (next:acc) (y+1)
    --}
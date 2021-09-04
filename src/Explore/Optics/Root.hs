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

-- A bunch of these are written in a somewhat roundabout manner to improve the UX. 
-- (If you're familiar with Control.Lens these probably don't look like most of the other Folds 
-- you've seen, even sand the monadic effects)

regItem :: forall a. IsCC a => Word32 -> Query a 
regItem w = mkMonadicFold go 
  where 
    go :: HiveData -> ExploreM a 
    go hData = case hData ^? (hiveCell w . content @a) of 
      Nothing -> qErr $ QueryError w "RegItem lookup failed. Target doesn't exist or is wrong type."
      Just a  -> pure a 

rootCell :: Query NKRecord 
rootCell = folding go 
  where 
    go :: HiveData -> Maybe NKRecord 
    go hData = hData ^? (hEnv . parsed . ix (hData ^. (hEnv . offset)) . content @NKRecord)

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

findCell :: forall a. IsCC a => MFold (a -> Bool) [(Word32,a)]
findCell  = mkMonadicFold go 
  where 
    go :: (a -> Bool) -> ExploreM [(Word32,a)]
    go p = look >>= \hData -> pure $ M.foldlWithKey f [] (hData ^. (hEnv . parsed))
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

{--
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
      --}
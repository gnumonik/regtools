module Explore where

import Data.Word 
import Types 
import Lib 
import ParseM 
import Control.Lens 
import qualified Data.Map as M 
import qualified Data.Vector as V 

makeLenses ''HiveData 

newtype NotFound = NotFound Word32 deriving (Show, Eq, Ord)

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
      Li w -> case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> Left (NotFound w) : acc
               Just nk -> Right nk : acc  
      Lf w w2 -> case hData ^? (hiveCell w . content @NKRecord) of 
               Nothing -> Left (NotFound w) : acc
               Just nk -> Right nk : acc  
      Lh w w2 -> case hData ^? (hiveCell w . content @NKRecord) of 
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

            ok      = numSK /= 0 && numSK /= (maxBound :: Word32)
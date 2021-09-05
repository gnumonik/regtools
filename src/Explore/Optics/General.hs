module Explore.Optics.General where 

{-- 

Module which contains generic (monadic/non-monadic) optics. (The rest of the optics in this folder are 
organized by their "input". I.e. a MFold NKRecord a would be in `NK.hs`) 

--}


import Control.Lens 
import Types
import Explore.ExploreM
import Control.Monad.IO.Class
import Data.Word (Word32)
import Lib (HiveData)
import Data.Maybe
import Control.Lens.Action
import Text.PrettyPrint.Leijen hiding ((<$>))

takin' :: Int -> Fold [a] [a]
takin' n = folding (Identity . take n)

pPrint :: Pretty a => MFold a ()
pPrint = mkMonadicFold $ \p -> 
         getPrinter >>= \f -> 
           liftIO 
           . f 
           . (\x -> displayS x  "") 
           . renderPretty 1.0 200 
           . pretty
           $ p 

content :: forall a. IsCC a => Fold HiveCell a 
content = folding go 

  where
    go :: HiveCell -> Maybe a 
    go (HiveCell s c) = c ^? cc @a 

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

select :: forall a. (a -> Bool) -> Getter [a] [a]
select p = to go 
  where 
    go :: [a] -> [a]
    go xs = filter p xs 

selectFirst :: forall a. (a -> Bool) -> Fold [a] a 
selectFirst p = folding go 
  where 
    go :: [a] -> Maybe a 
    go as = foldr (\x acc -> if p x then Just x else acc) Nothing as 
  
concatMapped :: forall a b. (MFold a [b]) -> MFold [a] [b]
concatMapped f = mkMonadicFold go 
  where 
    go xs =  concat . catMaybes <$> mapM (^!? f) xs 
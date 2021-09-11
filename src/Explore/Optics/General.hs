module Explore.Optics.General where 

{-- 

Module which contains generic (monadic/non-monadic) optics. 

The rest of the optics in this folder are more or less
organized by their "input". I.e. a MFold NKRecord a would be in `NK.hs`
(For staging reasons that doesn't always hold true, a few things are in 
`Root.hs` that probably shouldn't be)
--}


import Control.Lens
    ( Identity(Identity), (^.), (^?), folding, to, Fold, Getter ) 
import Types
import Explore.ExploreM
import Control.Monad.IO.Class
import Data.Word (Word32)
import Lib (HiveData)
import Data.Maybe
import Control.Lens.Action
import Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Data.Aeson as A 
import Data.Aeson ((.=))
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Search as Search 
import qualified Data.ByteString as BS
import Control.Monad (foldM)
import qualified Data.Text as TS 
import Hash


takin' :: Int -> Fold [a] [a]
takin' n = folding (Identity . take n)

pPrint :: Pretty a => MFold a ()
pPrint = mkMonadicFold $ \p -> 
         getPrinter >>= \f -> 
           liftIO 
           . f 
           . (<> "\n")
           . (\x -> displayS x  "") 
           . renderPretty 1.0 200 
           . pretty
           $ p 

-- | Writes JSON to a file. NOTE: DOES NOT APPEND!
writeJSON :: forall a
           . A.ToJSON a 
          => Printer
          -> FilePath 
          -> Maybe TS.Text -- optional tag for the JSON object 
          -> a 
          -> ExploreM ()
writeJSON f fPath mstr a =  do 
      f <- getPrinter 
      let js = case mstr of 
                  Nothing  -> A.toJSON a 
                  Just txt -> A.object [txt .= a] 
      liftIO $ f (T.unpack . pShow $ js)
      liftIO $ A.encodeFile fPath js  

writeHash :: forall a. DSLHashable a => FilePath -> DSLToHask a -> ExploreM ()
writeHash fPath a =  do
      f <- getPrinter  
      let hashed = mkHash @a a  
      liftIO . f . T.unpack . pShow $ hashed 
      liftIO $ A.encodeFile fPath hashed 

-- | Fold from a cell to its content. Requires a type application.
content :: forall a. IsCC a => Fold HiveCell a 
content = folding go 
  where
    go :: HiveCell -> Maybe a 
    go (HiveCell s c) = c ^? cc @a 

-- | Because I didn't realize know `to` from Control.Lens existed when I wrote this  
andThen :: forall a b. (a -> b) -> Fold a b 
andThen f = folding (\a -> Identity $ f a)

andThenM :: forall a b. (a -> ExploreM b) -> MFold a b 
andThenM = mkMonadicFold  

mappin' :: forall a b. (a -> b) -> Fold [a] [b]
mappin' f = andThen (map f) 

-- | It's like map but for MFolds. 
--   (This is different than the function of the same name in Control.Lens. 
--   Quite possibly Control.Lens has a combinator that will do this but 
--   if it does its buried in a pit of type synonym soup and I can't find it 
--   with hoogle)
mapped :: forall a b. (MFold a b) -> MFold [a] [b]
mapped f = mkMonadicFold go 
  where 
    go :: [a] -> ExploreM [b]
    go xs = catMaybes <$> mapM (^!? f) xs

-- | Like filter. Sorta.
selectM :: forall a. (MFold a Bool) -> MFold [a] [a]
selectM f = mkMonadicFold go 
  where 
    go :: [a] -> ExploreM [a]
    go xs = foldM g [] xs 
      where 
        g :: [a] -> a -> ExploreM [a]
        g acc a = a ^!? f >>= \case 
          Just True -> pure (a : acc) 
          _         -> pure acc
          
           
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

containsSubstring :: BS.ByteString -> BS.ByteString -> Bool 
containsSubstring pattern  = not . null . Search.nonOverlappingIndices pattern  


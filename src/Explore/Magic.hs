module Explore.Magic where

import Data.Singletons 
import Data.Singletons.TH 
import Data.Kind (Type)
import Data.Word
import Types 
import Explore.ExploreM 
import Lib 
import Explore.Optics.Root 
import Data.Constraint 
import Explore.Optics.VK
import Explore.Optics.NK
import Explore.Optics.General 
import Unsafe.Coerce 
import Text.PrettyPrint.Leijen (Pretty(..))
import Control.Lens (folding, to)


withSomeQB :: forall r
            . SomeQB 
           -> (forall (a :: DSLType) (b :: DSLType)
                    . (SingI a, SingI b) 
                   => QueryBuilder a b -> r)
           -> r 
withSomeQB (SomeQB q) f = case qbSing q of 
  (sA,sB) -> withSingI sA $ withSingI sB $ f q 

-- | Generates a PrettyRefl Dict for the second argument type to a 
--   query builder. 
qbDict :: forall a b. QueryBuilder a b -> Dict (PrettyRefl b)
qbDict = \case 
  ROOTCELL          -> Dict 
  SUBKEYS           -> Dict 
  KEYPATH   _       -> Dict 
  PPRINT            -> Dict 
  WRITEJSON _       -> Dict
  MATCHKEYNAME _    -> Dict 
  MATCHVALNAME _    -> Dict 
  MATCHVALDATA _ _  -> Dict
  TRIM _            -> Dict 
  MAP _             -> Dict 
  SELECT _          -> Dict 
  CONCATMAP  _      -> Dict 

-- | Same thing as qbDict but for the first argument. Since this isn't 
--   possible *in general* returns a Maybe Dict vs a Dict 
qbDict1 :: forall a b. QueryBuilder a b -> Maybe (Dict (PrettyRefl a))
qbDict1 = \case 
  ROOTCELL          -> Just Dict 
  SUBKEYS           -> Just Dict 
  KEYPATH   _       -> Just Dict 
  MATCHKEYNAME _    -> Just Dict 
  MATCHVALNAME _    -> Just Dict 
  MATCHVALDATA _ _  -> Just Dict
  TRIM _            -> Just Dict 
  _                 -> Nothing 

-- | Get the singletons of the argument types for a query builder. 
qbSing :: forall a b. QueryBuilder a b -> (Sing a, Sing b)
qbSing = \case 
  ROOTCELL          -> (SROOT,SREGKEY)
  
  SUBKEYS           -> (SREGKEY,SLIST SREGKEY)
  
  KEYPATH _         -> (SROOT,SREGKEY)
  
  PPRINT            -> (SANY,SUNIT)
  
  WRITEJSON _       -> (SANY,SUNIT)
  
  MATCHKEYNAME _    -> (SREGKEY, SBOOL)
  
  MATCHVALNAME _    -> (SREGKEY, SBOOL)
  
  MATCHVALDATA _ _  -> (SREGKEY, SBOOL)
  
  TRIM _            -> (SREGKEY,SREGKEY)
  
  MAP qb            -> case qbSing qb of 
    (a,b) -> (SLIST a,SLIST b)

  SELECT qb         -> case qbSing qb of 
    (a,SBOOL) -> (SLIST a, SLIST a)

  CONCATMAP qb     -> case qbSing qb of 
    (a, SLIST b)  -> (SLIST a, SLIST b)

-- Adds a query builder to a focus if the types match/
extendFocus :: QBConstraint a => (t :~: r) -> Focus t -> QueryBuilder t a -> Focus a 
extendFocus Refl f qb = case qbSing qb of 
  (l,a) -> withSingI a $ FocusS f qb 

-- THIS IS ONLY SAFE IF THE SECOND ARGUMENT CONTAINS A FUNCTION THAT IS POLYMORPHIC OVER EVERY 
-- POSSIBLE OUTPUT TYPE OF ANOTHER QUERY BUILDER. This should more or less be used only for the print 
-- or writeJSON functions. 
unsafeExtendFocus :: forall a t. PrettyRefl a => Focus t -> QueryBuilder 'ANY a -> Focus a 
unsafeExtendFocus f qb = let qb' = unsafeCoerce qb :: QueryBuilder t a 
                             (_,s) = qbSing qb :: (Sing 'ANY, Sing a) 
                         in withSingI s $ FocusS f qb' 

-- | Turns a focus into a BoxedMFold 
collapseFocus :: PrettyRefl a => Focus a -> BoxedMFold HiveData (DSLToHask a) 
collapseFocus = \case 
  fz@(FocusZ qb) ->  renderQB  qb 
  fs@(FocusS rest qb) -> case withSingI (focSing rest) $ withDict (focDict rest) $ collapseFocus rest of 
    box@(MkBoxedMFold f) -> withDict (focDict rest ) $ composeBoxedMFold  box (renderQB qb)

-- | Generates a Pretty/ToJSON dictionary from a focus 
focDict :: forall a. Focus a -> Dict (PrettyRefl a)
focDict = \case 
  FocusZ qb -> Dict 
  FocusS rest qb -> Dict

-- | Peeks the return type of the focus 
focSing :: forall a. Focus a -> Sing a 
focSing = \case 
  fz@(FocusZ _) -> sing @a 
  fs@(FocusS _ _) -> sing @a 

-- | Turns a 'QueryBuilder' into a 'BoxedMFold' of the corresponding Haskell types. 
renderQB :: forall a b. PrettyRefl a => QueryBuilder a b -> BoxedMFold (DSLToHask a) (DSLToHask b)
renderQB = \case 
  ROOTCELL           -> MkBoxedMFold rootCell
  SUBKEYS            -> MkBoxedMFold allSubkeys
  KEYPATH pth        -> MkBoxedMFold $ keyPath pth 
  PPRINT             -> MkBoxedMFold pPrint
  WRITEJSON fp       -> MkBoxedMFold $ writeJSON fp 
  MATCHKEYNAME bs    -> MkBoxedMFold $ to (nameContains bs)
  MATCHVALNAME bs    -> MkBoxedMFold $ to (valNameContains bs)
  MATCHVALDATA b1 b2 -> MkBoxedMFold $ to (valDataContains b1 b2)
  TRIM d             -> MkBoxedMFold $ kvs d 
  MAP qb             -> case renderQB qb of 
    MkBoxedMFold f   -> MkBoxedMFold $ mapped f 
  SELECT qb          -> case renderQB qb of 
    (MkBoxedMFold f) -> MkBoxedMFold $ selectM f 
  CONCATMAP qb       -> case renderQB qb of 
    MkBoxedMFold f   -> MkBoxedMFold $ concatMapped f 
 where 
   mkRegItem :: forall b. Word32 -> Dict (IsCC b) -> BoxedMFold HiveData b 
   mkRegItem w Dict = MkBoxedMFold $ regItem @b w

   mkFindCell :: forall b. Dict (IsCC b) -> (b -> Bool) -> BoxedMFold HiveData [b] 
   mkFindCell Dict p = MkBoxedMFold $ findCell @b p 

composeBoxedMFold :: BoxedMFold a b -> BoxedMFold b c -> BoxedMFold a c 
composeBoxedMFold (MkBoxedMFold f1) (MkBoxedMFold f2) = MkBoxedMFold $ f1 . f2 






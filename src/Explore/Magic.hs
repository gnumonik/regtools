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
import Control.Lens (folding, to, (^?))
import Hash 

-- orphan instances. its either put them here or put a bunch of functions into Types 

instance DSLHashable 'REGKEY where 
  mkHash = OneKey . hashKey 

instance DSLHashable ('LIST 'REGKEY) where 
  mkHash = ManyKeys . map hashKey 

instance DSLHashable ('LIST 'VAL) where 
  mkHash = ManyVals . map hashVal 

tvSing :: forall a. TypedVar a -> Sing a 
tvSing (MkTypedVar txt) = sing @ a 
  
typeDict :: forall a. TypeDict a -> Dict (PrettyRefl a)
typeDict (MkTypeDict x) = Dict  

withSomeQB :: forall r
            . SomeQB 
           -> (forall (a :: DSLType) (b :: DSLType)
                    . (SingI a, SingI b) 
                   => QueryBuilder a b -> r)
           -> r 
withSomeQB (SomeQB q) f = case qbSing q of 
  (sA,sB) -> withSingI sA $ withSingI sB $ f q 

cqbDict :: CompositeQB a b -> (Dict (PrettyRefl a), Dict (PrettyRefl b))
cqbDict = \case 
  QBZ _ -> (Dict,Dict)
  QBS rest qb -> case cqbDict rest of 
    (Dict,Dict) -> case qbDict qb of 
      Dict -> (Dict,Dict)

-- | Generates a PrettyRefl Dict for the second argument type to a 
--   query builder. 
qbDict :: forall a b. QueryBuilder a b -> Dict (PrettyRefl b)
qbDict qb = assertPretty (snd $ qbSing qb )

-- | Same thing as qbDict but for the first argument. Since this isn't 
--   possible *in general* returns a Maybe Dict vs a Dict 
qbDict1 :: forall a b. QueryBuilder a b -> (Dict (PrettyRefl a))
qbDict1 qb = assertPretty (fst $ qbSing qb) 

-- | Get the singletons of the argument types for a query builder. 
qbSing :: forall a b. QueryBuilder a b -> (Sing a, Sing b)
qbSing = \case 
  VALS              -> (SREGKEY,SLIST SVAL)

  ROOTCELL          -> (SROOT,SREGKEY)
  
  SUBKEYS           -> (SREGKEY,SLIST SREGKEY)
  
  KEYPATH _         -> (SREGKEY,SLIST SREGKEY)
  
  MATCHKEYNAME _    -> (SREGKEY, SBOOL)
  
  MATCHVALNAME _    -> (SVAL, SBOOL)
  
  MATCHVALDATA  _   -> (SVAL, SBOOL)
  
  EXPAND _          -> (SREGKEY,SREGKEY)
  
  MAP qb            -> case qbSing qb of 
    (a,b) -> (SLIST a,SLIST b)

  SELECT qb         -> case qbSing qb of 
    (a,SBOOL) -> (SLIST a, SLIST a)

  CONCATMAP qb     -> case qbSing qb of 
    (a, SLIST b)  -> (SLIST a, SLIST b)

  COMPOSED xs     -> go xs 
 where 
   go :: forall a b. CompositeQB a b -> (SDSLType a, SDSLType b)
   go (QBZ qb) = qbSing qb 
   go (QBS rest qb) = case qbSing qb of 
     (s_,e) -> case go rest of 
       (s,e_) -> (s,e)

-- Adds a query builder to a focus if the types match/
extendFocus :: QBConstraint a => (t :~: r) -> Focus t -> QueryBuilder t a -> Focus a 
extendFocus Refl f qb = case qbSing qb of 
  (l,a) -> withSingI a $ FocusS f qb 

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

mapFocus :: (forall a b. QueryBuilder a b -> QueryBuilder a b) -> Focus z -> Focus z 
mapFocus f = \case 
  FocusZ qb -> FocusZ $ f qb
  FocusS rest qb -> FocusS (mapFocus f rest) (f qb)

mapCQB :: (forall a b. QueryBuilder a b -> QueryBuilder a b) -> CompositeQB x y -> CompositeQB x y 
mapCQB f = \case 
  QBZ qb -> QBZ (f qb)
  QBS rest qb -> QBS (mapCQB f rest) qb 

-- | Turns a 'QueryBuilder' into a 'BoxedMFold' of the corresponding Haskell types. 
renderQB :: forall a b. PrettyRefl a => QueryBuilder a b -> BoxedMFold (DSLToHask a) (DSLToHask b)
renderQB = \case  
  VALS               -> MkBoxedMFold fqVals
  ROOTCELL           -> MkBoxedMFold rootCell
  SUBKEYS            -> MkBoxedMFold allSubkeys
  KEYPATH pth        -> MkBoxedMFold $ keyPath pth 
  MATCHKEYNAME bs    -> MkBoxedMFold $ to (nameContains bs)
  MATCHVALNAME bs    -> MkBoxedMFold $ to (valNameContains bs)
  MATCHVALDATA bs    -> MkBoxedMFold $ to (valDataContains bs)
  EXPAND d           -> maybe (MkBoxedMFold allKVs) (\d' -> MkBoxedMFold $ kvs d') d 
  MAP qb             -> case renderQB qb of 
    MkBoxedMFold f   -> MkBoxedMFold $ mapped f 
  SELECT qb          -> case renderQB qb of 
    (MkBoxedMFold f) -> MkBoxedMFold $ selectM f 
  CONCATMAP qb       -> case renderQB qb of 
    MkBoxedMFold f   -> MkBoxedMFold $ concatMapped f 
  COMPOSED cqb       -> go cqb 
 where 
   mkRegItem :: forall b. Word32 -> Dict (IsCC b) -> BoxedMFold HiveData b 
   mkRegItem w Dict = MkBoxedMFold $ regItem @b w

   mkFindCell :: forall b. Dict (IsCC b) -> (b -> Bool) -> BoxedMFold HiveData [b] 
   mkFindCell Dict p = MkBoxedMFold $ findCell @b p 

   go :: forall a b. CompositeQB a b -> BoxedMFold (DSLToHask a) (DSLToHask b)
   go = \case 
    QBZ qb -> withDict (qbDict qb) renderQB qb 
    cqb@(QBS rest qb) -> case go rest of 
      (MkBoxedMFold f1) -> case cqbDict rest of 
        (Dict,Dict) -> case renderQB qb of
          (MkBoxedMFold f2) -> MkBoxedMFold (f1 .f2) 

composeBoxedMFold :: BoxedMFold a b -> BoxedMFold b c -> BoxedMFold a c 
composeBoxedMFold (MkBoxedMFold f1) (MkBoxedMFold f2) = MkBoxedMFold $ f1 . f2 

composeQB :: forall a b c 
           . (PrettyRefl c) 
          => CompositeQB a b 
          -> QueryBuilder b c
          -> CompositeQB a c 
composeQB cqb qb = case qbSing qb of 
  (b,c) -> withSingI b $ withSingI c $ QBS cqb qb  

cqbSing :: forall a b. CompositeQB a b -> (Sing a, Sing b)
cqbSing = \case 
  QBZ qb -> qbSing qb
  QBS rest qb -> case qbSing qb of 
     (qA,qB) -> case cqbSing rest of 
       (cA,cB) -> (cA,qB) 

liftCQB :: forall a b
         . (PrettyRefl a, PrettyRefl b)
        => QueryBuilder a b 
        -> CompositeQB a b 
liftCQB qb = case qbSing qb of 
  (a,b) -> withSingI a $ withSingI b $ QBZ qb 
                          
tdSing :: forall a. TypeDict a -> Sing a 
tdSing (MkTypeDict s) = s 

tdDict :: forall a. TypeDict a -> Dict (PrettyRefl a)
tdDict (MkTypeDict s) = Dict 

expSing :: forall a. DSLExp a -> Sing a 
expSing = \case 
  RootQuery fcs -> focSing fcs 
  
  VarQuery tv qb -> snd $ qbSing qb

  Assign e _ -> expSing e  

  IfThen b a a' -> expSing a'  

  DSLVar tv     -> tvSing tv 

  Append l1 l2  -> expSing l1 

  Concat e      -> case expSing e of 
    (SLIST x) -> x

  IsEmpty e     -> SBOOL  

  Command c     -> SEFFECT

expDict :: forall a. DSLExp a -> Dict (PrettyRefl a)
expDict = \case 
  RootQuery fcs -> focDict fcs 

  VarQuery tv qb -> qbDict qb 

  Assign e _     -> expDict e 

  IfThen b _ a   -> expDict a

  DSLVar tv      -> assertPretty (tvSing tv) 

  Append l1 l2   -> expDict l1 

  Concat e       -> assertPretty (expSing (Concat e))

  IsEmpty e      -> Dict 

  Command c      -> Dict 
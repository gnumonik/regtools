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
qbDict = \case
  VALS              -> Dict 
  FILTERVALUES _    -> Dict
  FILTERSUBKEYS _   -> Dict  
  ROOTCELL          -> Dict 
  SUBKEYS           -> Dict 
  KEYPATH   _       -> Dict 
  PPRINT            -> Dict 
  WRITEJSON _ _     -> Dict
  MATCHKEYNAME _    -> Dict 
  MATCHVALNAME _    -> Dict 
  MATCHVALDATA  _   -> Dict
  EXPAND _          -> Dict 
  MAP _             -> Dict 
  SELECT _          -> Dict 
  CONCATMAP  _      -> Dict 
  COMPOSED _        -> Dict 
  HASHKEY _         -> Dict 
  HASHKEYS _        -> Dict 

-- | Same thing as qbDict but for the first argument. Since this isn't 
--   possible *in general* returns a Maybe Dict vs a Dict 
qbDict1 :: forall a b. QueryBuilder a b -> Maybe (Dict (PrettyRefl a))
qbDict1 = \case
  VALS              -> Just Dict 
  FILTERVALUES _    -> Just Dict 
  FILTERSUBKEYS _   -> Just Dict 
  ROOTCELL          -> Just Dict 
  SUBKEYS           -> Just Dict 
  KEYPATH   _       -> Just Dict 
  MATCHKEYNAME _    -> Just Dict 
  MATCHVALNAME _    -> Just Dict 
  MATCHVALDATA   _  -> Just Dict
  EXPAND _          -> Just Dict 
  COMPOSED cqb      -> case cqbDict cqb of 
    (l,r)           -> Just l 
  _                 -> Nothing 

-- | Get the singletons of the argument types for a query builder. 
qbSing :: forall a b. QueryBuilder a b -> (Sing a, Sing b)
qbSing = \case 
  VALS              -> (SREGKEY,SLIST SVAL)

  FILTERVALUES  _   -> (SREGKEY, SREGKEY)

  FILTERSUBKEYS _   -> (SREGKEY, SREGKEY)

  ROOTCELL          -> (SROOT,SREGKEY)
  
  SUBKEYS           -> (SREGKEY,SLIST SREGKEY)
  
  KEYPATH _         -> (SROOT,SREGKEY)
  
  PPRINT            -> (SANY,SUNIT)
  
  WRITEJSON _ _     -> (SANY,SUNIT)
  
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

  HASHKEY _        -> (SREGKEY,SUNIT)

  HASHKEYS _       -> (SLIST SREGKEY,SUNIT)

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

mapFocus :: (forall a b. QueryBuilder a b -> QueryBuilder a b) -> Focus z -> Focus z 
mapFocus f = \case 
  FocusZ qb -> FocusZ $ f qb
  FocusS rest qb -> FocusS (mapFocus f rest) (f qb)

mapCQB :: (forall a b. QueryBuilder a b -> QueryBuilder a b) -> CompositeQB x y -> CompositeQB x y 
mapCQB f = \case 
  QBZ qb -> QBZ (f qb)
  QBS rest qb -> QBS (mapCQB f rest) qb 

rewritePath :: forall a. FilePath -> DSLExp a -> DSLExp a 
rewritePath fPath = \case 
  RootQuery fcs  -> RootQuery (mapFocus rewrite fcs )
  VarQuery tv qb -> VarQuery tv (rewrite qb)
  Assign fcs a   -> Assign (mapFocus rewrite fcs) a 
  Command c      -> Command c 
 where 
    rewrite :: forall a b. QueryBuilder a b -> QueryBuilder a b 
    rewrite = \case 
        HASHKEY  _    -> HASHKEY fPath  
        HASHKEYS _    -> HASHKEYS fPath
        WRITEJSON _ t -> WRITEJSON fPath t
        COMPOSED cqb  -> COMPOSED (mapCQB rewrite cqb )
        other         -> other   

-- | Turns a 'QueryBuilder' into a 'BoxedMFold' of the corresponding Haskell types. 
renderQB :: forall a b. PrettyRefl a => QueryBuilder a b -> BoxedMFold (DSLToHask a) (DSLToHask b)
renderQB = \case 
  VALS               -> MkBoxedMFold namedVals
  FILTERVALUES qb    -> case renderQB qb of 
    MkBoxedMFold f -> MkBoxedMFold $ filterValues f 
  FILTERSUBKEYS qb   -> case renderQB qb of 
    MkBoxedMFold f -> MkBoxedMFold $ filterSubkeys f   
  ROOTCELL           -> MkBoxedMFold rootCell
  SUBKEYS            -> MkBoxedMFold allSubkeys
  KEYPATH pth        -> MkBoxedMFold $ keyPath pth 
  PPRINT             -> MkBoxedMFold pPrint
  WRITEJSON fp mstr  -> MkBoxedMFold $ writeJSON fp mstr  
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
  HASHKEY fp         -> MkBoxedMFold $ hashOneKey fp 
  HASHKEYS fp        -> MkBoxedMFold $ hashManyKeys fp 
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

unsafeComposeQB :: forall a b c
                 . PrettyRefl c 
                => CompositeQB a b 
                -> QueryBuilder 'ANY c 
                -> CompositeQB a c 
unsafeComposeQB cqb qb = let qb' :: QueryBuilder b c 
                             qb' = unsafeCoerce qb 
                         in composeQB cqb qb' 

unsafeLiftCQB :: forall a b
              . QueryBuilder 'ANY b 
             -> TypeDict a 
             -> CompositeQB a b
unsafeLiftCQB qb td@(MkTypeDict sA)
  = let qb' :: QueryBuilder a b 
        qb' = unsafeCoerce qb 
    in withSingI sA
     $ withSingI (snd $ qbSing qb')
     $ withDict (qbDict qb') 
     $ withDict (typeDict td)
     $ QBZ qb' 
                         

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
                          


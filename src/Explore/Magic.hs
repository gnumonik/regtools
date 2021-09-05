module Explore.Magic where

import Data.Singletons 
import Data.Singletons.TH 
import Data.Kind (Type)
import Data.Word
import Types 
import Explore.ExploreM (Query, MFold, RegistryKey (RegistryKey))
import Lib (HiveData)
import Explore.Optics.Root 
import Data.Constraint 
import Explore.Optics.VK
import Explore.Optics.NK
import Explore.Optics.General 
import Unsafe.Coerce 
import Text.PrettyPrint.Leijen (Pretty(..))
-- Typelevel magic. There are two ways we can parse a sequence of lenses: 
--    1) A massive amount of unreadable TH + ridiculous typeclass sorcery + probably some unsafecoerce 
--    2) Fake dependent types. 
-- We're gonna go with 2 here cuz I do not feel like pouring hover th splice output 
$(singletons [d| 
  data DSLType 
    = NKREC 
    | ROOT
    | VKREC 
    | SKREC 
    | SKLIST 
    | VLIST 
    | REGKEY 
    | VAL
    | LIST DSLType
    | UNIT 
    | ANY -- pls don't break everything its so beautiful
       deriving (Show, Eq)
 |])




type TokType :: CCTok -> DSLType 
type family TokType x where 
  TokType 'SKRec = 'SKREC 
  TokType 'NKRec = 'NKREC 
  TokType 'VKRec = 'VKREC 
  TokType 'SKList = 'SKLIST 
  TokType 'VList  = 'VLIST 
  TokType 'Val    = 'VAL 



sTokType :: Sing (a :: CCTok) -> Sing (TokType' @@ a)
sTokType = \case 
  SSKRec  -> SSKREC 
  SNKRec  -> SNKREC 
  SVKRec  -> SVKREC 
  SSKList -> SSKLIST 
  SVList  -> SVLIST 
  SVal    -> SVAL 

-- yeah we need this. bleh. 
tokToDict :: Sing (c :: CCTok) -> Dict (IsCC (DSLToHask (TokType c))) 
tokToDict = \case 
  SSKRec  -> Dict 
  SNKRec  -> Dict 
  SVKRec  -> Dict 
  SSKList -> Dict 
  SVList  -> Dict 
  SVal    -> Dict 


data TokType' :: CCTok ~> DSLType 
type instance Apply TokType' c = TokType c 

type DSLToHask :: DSLType -> Type 
type family DSLToHask x where 
  DSLToHask 'SKREC    = SKRecord 
  DSLToHask 'NKREC    = NKRecord
  DSLToHask 'VKREC    = VKRecord 
  DSLToHask 'SKLIST   = SubkeyList 
  DSLToHask 'VLIST    = ValueList 
  DSLToHask 'VAL      = Value
  DSLToHask 'ROOT     = HiveData 
  DSLToHask 'REGKEY   = RegistryKey 
  DSLToHask ('LIST a) = [DSLToHask a]
  DSLToHask 'UNIT     = ()

type HaskToDSL :: Type -> DSLType 
type family HaskToDSL x where 
  HaskToDSL SKRecord    = 'SKREC 
  HaskToDSL NKRecord    = 'NKREC 
  HaskToDSL VKRecord    = 'VKREC 
  HaskToDSL SubkeyList  = 'SKLIST 
  HaskToDSL ValueList   = 'VLIST 
  HaskToDSL Value       = 'VAL 
  HaskToDSL HiveData    = 'ROOT 
  HaskToDSL RegistryKey = 'REGKEY
  HaskToDSL [a]         = 'LIST (HaskToDSL a)
  HaskToDSL ()          = 'UNIT 

type Isos :: DSLType -> Type -> Constraint 
type family Isos x y where 
  Isos d t = (DSLToHask d ~ t, HaskToDSL t ~ d)  

data DSLToHask' :: DSLType ~> Type
type instance Apply DSLToHask' c = DSLToHask c 

data SomeQB :: Type where 
  SomeQB :: QueryBuilder a b -> SomeQB 

data QBLeft :: DSLType -> Type where 
  MkQBLeft :: QueryBuilder a b -> QBLeft a 

withQBLeft :: forall r (a :: DSLType)
            . QBLeft a 
           -> (forall (b :: DSLType). QueryBuilder a b -> r )
           -> r 
withQBLeft (MkQBLeft l) f = f l 

withQBRight :: forall r (b :: DSLType)
             . QBRight b 
            -> (forall (a :: DSLType). QueryBuilder a b -> r)
            -> r 
withQBRight (MkQBRight r) f = f r     

data QBRight :: DSLType -> Type where 
  MkQBRight :: QueryBuilder a b -> QBRight b 

withSomeQB :: forall r
            . SomeQB 
           -> (forall (a :: DSLType) (b :: DSLType)
                    . (SingI a, SingI b) 
                   => QueryBuilder a b -> r)
           -> r 
withSomeQB (SomeQB q) f = case qbSing q of 
  (sA,sB) -> withSingI sA $ withSingI sB $ f q 

type PrettyRefl a = Pretty (DSLToHask a)

data QueryBuilder :: DSLType -> DSLType -> Type where 
  REGITEM     :: PrettyRefl (TokType a) => Word32 -> Sing (a :: CCTok) -> QueryBuilder 'ROOT (TokType a)

  ROOTCELL    :: PrettyRefl 'NKREC => QueryBuilder 'ROOT 'NKREC 

  FINDCELL    :: PrettyRefl (TokType a)
              => Sing (a :: CCTok)
              -> (DSLToHask (TokType a) -> Bool)
              -> QueryBuilder 'ROOT ('LIST (TokType a))
  
-- do deadzones later 

  GETVAL      :: PrettyRefl 'VAL => QueryBuilder 'VKREC 'VAL 

  STBLSUBKEYS :: PrettyRefl ('LIST 'NKREC) => QueryBuilder 'NKREC ('LIST 'NKREC)

  VOLSUBKEYS  :: PrettyRefl ('LIST 'NKREC) => QueryBuilder 'NKREC ('LIST 'NKREC)

  ALLSUBKEYS  :: PrettyRefl ('LIST 'NKREC) => QueryBuilder 'NKREC ('LIST 'NKREC)

  VKRECS      :: PrettyRefl ('LIST 'VKREC) => QueryBuilder 'NKREC ('LIST 'VKREC)

  KEYVALUES   :: PrettyRefl 'REGKEY => Word32 -> QueryBuilder 'NKREC 'REGKEY 

  ALLKVS      :: PrettyRefl 'REGKEY => QueryBuilder 'NKREC 'REGKEY 

  KEYPATH     :: PrettyRefl 'NKREC  => [String] -> QueryBuilder 'ROOT 'NKREC

  PPRINT      :: PrettyRefl 'UNIT   => QueryBuilder 'ANY 'UNIT 

qbDict :: forall a b. QueryBuilder a b -> Dict (Pretty (DSLToHask b))
qbDict = \case 
  REGITEM  _ _ -> Dict 
  ROOTCELL  -> Dict 
  FINDCELL _ _ -> Dict 
  GETVAL -> Dict 
  STBLSUBKEYS -> Dict 
  VOLSUBKEYS -> Dict 
  ALLSUBKEYS -> Dict 
  VKRECS    -> Dict 
  KEYVALUES _ -> Dict 
  ALLKVS    -> Dict 
  KEYPATH   _ -> Dict 
  PPRINT    -> Dict 


qbSing :: forall a b. QueryBuilder a b -> (Sing a, Sing b)
qbSing = \case 
  REGITEM w b  -> (SROOT,sTokType b)
  ROOTCELL     -> (SROOT,SNKREC)
  FINDCELL b _ -> (SROOT,SLIST $ sTokType b)
  GETVAL       -> (SVKREC,SVAL) 
  STBLSUBKEYS  -> (SNKREC,SLIST SNKREC)
  VOLSUBKEYS   -> (SNKREC,SLIST SNKREC)
  ALLSUBKEYS   -> (SNKREC,SLIST SNKREC)
  VKRECS       -> (SNKREC,SLIST SVKREC)
  KEYVALUES _  -> (SNKREC,SREGKEY)
  ALLKVS       -> (SNKREC,SREGKEY)
  KEYPATH _    -> (SROOT,SNKREC)
  PPRINT       -> (SANY,SUNIT)

type QBConstraint a = (SingI a, Pretty (DSLToHask a))

data Focus :: DSLType -> Type where 
  FocusZ  :: QBConstraint a 
          =>  QueryBuilder 'ROOT a
          ->  Focus a 
  FocusS :: QBConstraint a => Focus t -> QueryBuilder t a -> Focus a 

extendFocus :: QBConstraint a => (t :~: r) -> Focus t -> QueryBuilder t a -> Focus a 
extendFocus Refl f qb = case qbSing qb of 
  (l,a) -> withSingI a $ FocusS f qb 

unsafeExtendFocus :: forall a t. PrettyRefl a => Focus t -> QueryBuilder 'ANY a -> Focus a 
unsafeExtendFocus f qb = let qb' = unsafeCoerce qb :: QueryBuilder t a 
                             (_,s) = qbSing qb :: (Sing 'ANY, Sing a) 
                         in withSingI s $ FocusS f qb' 

-- probably gonna have to shove some singletons into boxedMfold or w/e 
collapseFocus :: Pretty (DSLToHask a) => Focus a -> BoxedMFold HiveData (DSLToHask a) 
collapseFocus = \case 
  fz@(FocusZ qb) ->  renderQB qb 
  fs@(FocusS rest qb) -> case withSingI (focSing rest) $ withDict (focDict rest) $ collapseFocus rest of 
    box@(MkBoxedMFold f) -> withDict (focDict rest ) $ composeBoxedMFold  box (renderQB qb)
 
focDict :: forall a. Focus a -> Dict (Pretty (DSLToHask a))
focDict = \case 
  FocusZ qb -> Dict 
  FocusS rest qb -> Dict 


focSing :: forall a. Focus a -> Sing a 
focSing = \case 
  fz@(FocusZ _) -> sing @a 
  fs@(FocusS _ _) -> sing @a 

compQB :: forall b c d. QBConstraint c => Focus b -> QueryBuilder b c -> Focus c 
compQB fcs qb = case qbSing qb of 
  (_,c) -> withSingI c $ FocusS fcs qb 

data BoxedQuery :: Type -> Type where 
  MkBoxQuery :: Query a -> BoxedQuery a 

data BoxedMFold :: Type -> Type -> Type where 
  MkBoxedMFold ::  MFold a b -> BoxedMFold a b

renderQB :: forall a b. Pretty (DSLToHask a)=> QueryBuilder a b -> BoxedMFold (DSLToHask a) (DSLToHask b)
renderQB = \case 
  REGITEM w b  -> mkRegItem w (tokToDict b) 
  ROOTCELL     -> MkBoxedMFold rootCell
  FINDCELL b p -> mkFindCell (tokToDict b) p  
  GETVAL       -> MkBoxedMFold val 
  STBLSUBKEYS  -> MkBoxedMFold stblSubkeys 
  VOLSUBKEYS   -> MkBoxedMFold volSubkeys 
  ALLSUBKEYS   -> MkBoxedMFold allSubkeys
  VKRECS       -> MkBoxedMFold vks 
  KEYVALUES d  -> MkBoxedMFold $ kvs (fromIntegral d) 
  ALLKVS       -> MkBoxedMFold $ allKVs 
  KEYPATH pth  -> MkBoxedMFold $ keyPath pth 
  PPRINT       -> MkBoxedMFold pPrint 

 where 
   mkRegItem :: forall b. Word32 -> Dict (IsCC b) -> BoxedMFold HiveData b 
   mkRegItem w Dict = MkBoxedMFold $ regItem @b w

   mkFindCell :: forall b. Dict (IsCC b) -> (b -> Bool) -> BoxedMFold HiveData [b] 
   mkFindCell Dict p = MkBoxedMFold $ findCell @b p 


composeBoxedMFold :: BoxedMFold a b -> BoxedMFold b c -> BoxedMFold a c 
composeBoxedMFold (MkBoxedMFold f1) (MkBoxedMFold f2) = MkBoxedMFold $ f1 . f2 






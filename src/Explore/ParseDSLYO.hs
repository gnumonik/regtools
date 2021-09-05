module Explore.ParseDSLYO where 

import Prelude hiding (lex)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer 
import Explore.Magic 
import qualified Data.Text as T
import Data.Void
import Control.Monad (void)
import Text.Megaparsec.Char
import Data.Functor (($>))
import Types 
import Data.Kind 
import Data.Singletons 
import Data.Word
import Explore.Optics.Utils 
import Data.Singletons.Decide
import Explore.LexDSL 
import qualified Data.Map as M 
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Control.Monad.State.Strict as ST 
import Data.Constraint

type Context = M.Map T.Text (SomeSing DSLType)

insertVar :: forall a. TypedVar (a :: DSLType) -> Context -> Either String Context 
insertVar tv@(MkTypedVar txt) cxt = case M.lookup txt cxt of
  Nothing -> Right $ M.insert txt (SomeSing $ sing @a) cxt 
  Just (SomeSing t')  -> case decideEquality t' (sing @a) of 
    Nothing -> Left $ "Type mismatch! The variable " 
                    <> (T.unpack txt) 
                    <> " was previously assigned a value of type "
                    <> (show . fromSing $ t' )    
                    <> " but is being used as a variable of type "
                    <> (show . fromSing $ sing @a)
                    <> " in the present context."
    Just Refl -> Right cxt 

mkVar :: forall a. Sing (a :: DSLType) -> T.Text -> DSLParser (TypedVar a) 
mkVar sA txt 
  = let tv = withSingI sA $ MkTypedVar @a txt
    in insertVar tv  <$!>  lift ST.get >>= \case 
      Left err -> fail err 
      Right cxt -> lift (ST.modify . const $ cxt) >> pure tv 


data TypedVar :: DSLType -> Type where 
  MkTypedVar :: SingI (a :: DSLType) => T.Text -> TypedVar a 

data DSLExp :: DSLType -> Type where

  QueryExp  :: Focus a -> DSLExp a

  Assign    :: Focus a -> TypedVar a -> DSLExp a

  -- commands/statements, but those are easy to add later 

data Some :: (k -> Type) -> Type where 
  Some :: forall k (a :: k) (c :: k -> Type)
        . c a -> Some c  

type DSLParser a = ParsecT Void [Tok] (ST.State Context) a  


tk :: Tok -> DSLParser ()
tk t = void $ satisfy (== t) 

w32 :: DSLParser Word32 
w32 = do 
  IntLike n <- satisfy (\case {IntLike _ -> True ; _ -> False})
  pure . fromIntegral $ n 

cctok :: DSLParser CCTok 
cctok = do 
  CTok c <- satisfy (\case {CTok _ -> True ; _ -> False})
  pure c 

dslExp :: DSLParser (Some DSLExp)
dslExp = choice [assign, qExp]

assign :: DSLParser (Some DSLExp)
assign = do 
  Name n <- satisfy (\case {Name _ -> True ; _ -> False})
  qExp >>= \case 
    Some (QueryExp f) -> case focSing f of 
      sF -> do 
        tv <-  mkVar sF n 
        pure . Some $ Assign f (withSingI sF $ MkTypedVar n)
    _ -> fail "Assignment failure. This really really shouldn't happen."

-- this is only possible w/ singletons, ty dr. eisenberg 
qExp :: DSLParser (Some DSLExp)
qExp = do 
  qbs <- {-- between (tk LParen) (tk RParen) --} (someQB `sepBy` tk Pipe)
  case composeQExp qbs of 
    Left err -> fail err 
    Right (Some f)  -> pure . Some . QueryExp $ f 
 where 
  composeQExp :: [SomeQB] -> Either String (Some Focus)
  composeQExp [] = Left "Error: Empty Query Expression!"
  composeQExp (SomeQB x:xs) = case qbSing x of 
    (SROOT,f) -> withSingI f $ withDict (qbDict x) $ go (FocusZ x) xs 
    _         -> Left $ "Error: The first sub-query does not target the Hive Root" 
    where 
      go :: forall b. SingI b => Focus b ->  [SomeQB] -> Either String (Some Focus)
      go acc [] = Right $ Some acc
      go acc (SomeQB b:bs) = case qbSing b of 
        (SANY,r) -> withSingI r $ withDict (qbDict b) $ go (unsafeExtendFocus acc b) bs 
        (l,r) -> case decideEquality l (sing @b) of 
          Nothing -> Left $ "Error: Type Mismatch In Query (make this more informative)"
          Just Refl -> withSingI r $ withDict (qbDict b) $ go (extendFocus Refl acc b) bs  

-- if something breaks its probably because i have/don't have 'try' somewhere here 
someQB :: DSLParser SomeQB
someQB = choice [
        someRegItem 
      , someRootCell 
      , someKeyPath 
      , someGetVal 
      , someStblSubkeys 
      , someVolSubkeys 
      , someAllSubkeys 
      , someVKRecs 
      , someKeyValues 
      , someAllKVs
      , somePPrint
  --  , someFindCell
  ]
 where

    someGetVal :: DSLParser SomeQB 
    someGetVal = do 
      tk (QBTok GetVal)
      pure . SomeQB $  GETVAL

    someStblSubkeys :: DSLParser SomeQB 
    someStblSubkeys = do 
      tk (QBTok StblSubkeys)
      pure . SomeQB $ STBLSUBKEYS 

    someVolSubkeys :: DSLParser SomeQB 
    someVolSubkeys = do 
      tk (QBTok VolSubkeys)
      pure . SomeQB $ VOLSUBKEYS 

    someAllSubkeys :: DSLParser SomeQB 
    someAllSubkeys = do 
      tk (QBTok AllSubkeys)
      pure . SomeQB $ ALLSUBKEYS 

    someVKRecs :: DSLParser SomeQB 
    someVKRecs = do 
      tk (QBTok VKRecs)
      pure . SomeQB $ VKRECS 

    someKeyValues :: DSLParser SomeQB 
    someKeyValues = do 
      tk (QBTok KeyValues)
      w <- w32 
      pure . SomeQB $ KEYVALUES w 

    someAllKVs :: DSLParser SomeQB
    someAllKVs = do 
      tk (QBTok AllKVs)
      pure . SomeQB $ ALLKVS   

    somePPrint :: DSLParser SomeQB 
    somePPrint = do 
      tk (QBTok PPrint)
      pure . SomeQB $ PPRINT 

    someRegItem :: DSLParser SomeQB 
    someRegItem = do 
      tk (QBTok RegItem) 
      w <- w32 
      c <- cctok 
      case toSing c of 
        SomeSing SSKRec  -> pure . SomeQB $ REGITEM w SSKRec 
        SomeSing SNKRec  -> pure . SomeQB $ REGITEM w SNKRec 
        SomeSing SVKRec  -> pure . SomeQB $ REGITEM w SVKRec 
        SomeSing SSKList -> pure . SomeQB $ REGITEM w SSKList 
        SomeSing SVList  -> pure . SomeQB $ REGITEM w SVList 
        SomeSing SVal    -> pure . SomeQB $ REGITEM w SVal 

    someRootCell :: DSLParser SomeQB 
    someRootCell = do 
      tk (QBTok RootCell)
      pure . SomeQB $ ROOTCELL 

    someKeyPath :: DSLParser SomeQB 
    someKeyPath = do 
      tk (QBTok KeyPath)
      (LitString kPath) <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let kPath' =  toKeyPath . T.unpack $ kPath 
      pure . SomeQB $ KEYPATH kPath' 

    someFindCell :: DSLParser SomeQB 
    someFindCell = undefined -- need to work out predicates  


    



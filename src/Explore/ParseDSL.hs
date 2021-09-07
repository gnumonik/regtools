module Explore.ParseDSL where 

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
import qualified Data.ByteString.Char8 as BC

-- | This is more or less our Type Checker 
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

-- | Concrete implementation of the type checker in the parser monad 
mkVar :: forall a. Sing (a :: DSLType) -> T.Text -> DSLParser (TypedVar a) 
mkVar sA txt 
  = let tv = withSingI sA $ MkTypedVar @a txt
    in insertVar tv  <$!>  lift ST.get >>= \case 
      Left err -> fail err 
      Right cxt -> lift (ST.modify . const $ cxt) >> pure tv 

-- | Match a token, return ()
tk :: Tok -> DSLParser ()
tk t = void $ satisfy (== t) 

w32 :: DSLParser Word32 
w32 = do 
  IntLike n <- satisfy (\case {IntLike _ -> True ; _ -> False})
  pure . fromIntegral $ n 

dslExp :: DSLParser (Some DSLExp)
dslExp = choice [cmd,assign, qExp]

assign :: DSLParser (Some DSLExp)
assign = do 
  Name n <- satisfy (\case {Name _ -> True ; _ -> False})
  tk LArrow
  qExp >>= \case 
    Some (QueryExp f) -> case focSing f of 
      sF -> do 
        tv <-  mkVar sF n 
        pure . Some $ Assign f (withSingI sF $ MkTypedVar n)
    _ -> fail "Assignment failure. This really really shouldn't happen."

cmd :: DSLParser (Some DSLExp)
cmd = choice [cmdExit
             -- ,cmdHelp
             -- ,cmdDeadSpace
             ]
 where 
   cmdExit = do 
     tk (CmdTok ExitTok)
     pure . Some . Command $ EXIT 

-- | this is only possible w/ singletons, ty dr. eisenberg 
qExp :: DSLParser (Some DSLExp)
qExp = do 
  qbs <- someQB `sepBy1` tk Pipe
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
        someRootCell 
      , someKeyPath 
      , someSubkeys 
      , somePPrint
      , someWriteJSON
      , someMatchName
      , someMatchValName 
      , someMatchValData 
      , someMap 
      , someSelect 
      , someConcatMap 
      , someTrim
  ]
 where
    someTrim :: DSLParser SomeQB 
    someTrim = do 
      tk (QBTok Trim)
      w <- w32 
      pure . SomeQB $ TRIM w 

    someMatchName :: DSLParser SomeQB 
    someMatchName = do 
      tk (QBTok MatchName)
      LitString n <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let n' = BC.pack (T.unpack n) 
      pure . SomeQB $ MATCHKEYNAME n' 

    someMatchValName :: DSLParser SomeQB 
    someMatchValName = do 
      tk (QBTok MatchValName) 
      LitString n <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let n' = BC.pack (T.unpack n)
      pure . SomeQB $ MATCHVALNAME n' 

    someMatchValData :: DSLParser SomeQB
    someMatchValData = do 
      tk (QBTok MatchValData)
      LitString n1 <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      LitString n2 <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let (n1',n2') = (BC.pack . T.unpack $ n1, BC.pack . T.unpack $ n2)
      pure . SomeQB $ MATCHVALDATA n1' n2' 

    someMap :: DSLParser SomeQB 
    someMap = do 
      tk (QBTok Map)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case (qbDict1 qb, qbDict qb) of 
        (Just d,Dict) -> withDict d $ pure . SomeQB $ MAP qb
        _             -> fail "Type mismatch in `map` function"

    someSelect :: DSLParser SomeQB 
    someSelect = do 
      tk (QBTok Select)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case qbDict1 qb of 
        Just d -> case qbSing qb of 
          (_,SBOOL) -> withDict d $ pure . SomeQB $ SELECT qb 
          _ -> fail "Type mismatch in `select` function"
        Nothing -> fail "Type mismatch in `select` function"

    someConcatMap :: DSLParser SomeQB 
    someConcatMap = do 
      tk (QBTok ConcatMap)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case qbDict1 qb of 
        Just d -> case qbSing qb of 
          (_,SLIST x) ->  pure . SomeQB $ withDict d $ withDict (qbDict qb) $ CONCATMAP qb 
          _ -> fail "Type mismatch in `concatMap` function"
        _ -> fail "Type mismatch in `concatMap` function"

    someSubkeys :: DSLParser SomeQB 
    someSubkeys = do 
      tk (QBTok SubKeys)
      pure . SomeQB $ SUBKEYS 

    somePPrint :: DSLParser SomeQB 
    somePPrint = do 
      tk (QBTok PPrint)
      pure . SomeQB $ PPRINT 

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


    someWriteJSON :: DSLParser SomeQB 
    someWriteJSON = do 
      tk (QBTok WriteJSON)
      LitString fPath <- satisfy (\case {LitString _ -> True ; _ -> False})
      pure . SomeQB $ WRITEJSON (T.unpack fPath) 


    



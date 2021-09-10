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
import Control.Lens (over, _1,  _2, view, (^.), Ixed (ix), (^?), set)
import qualified Data.Text.IO as TIO

{----------------------------------------------
 ----------------------------------------------
              Utilities / Type checking
-----------------------------------------------
-----------------------------------------------}

-- | This is more or less our Type Checker 
insertVar :: forall a. PrettyRefl a => TypedVar (a :: DSLType) -> Context -> Either String Context 
insertVar tv@(MkTypedVar txt) cxt = case M.lookup txt (cxt ^. tyCxt) of
  Nothing -> Right $ over tyCxt (M.insert txt (Some $ MkTypeDict $ sing @a)) cxt 
  Just (Some (MkTypeDict t'))  -> case decideEquality t' (sing @a) of 
    Nothing -> Left $ "Type mismatch! The variable " 
                    <>T.unpack txt
                    <> " was previously assigned a value of type "
                    <> (show . fromSing $ t' )    
                    <> " but is being used as a variable of type "
                    <> (show . fromSing $ sing @a)
                    <> " in the present context."
    Just Refl -> Right cxt 

-- | Type checker for assignments  
mkVar :: forall a. PrettyRefl a => Sing (a :: DSLType) -> T.Text -> DSLParser (TypedVar a) 
mkVar sA txt 
  = let tv = withSingI sA $ MkTypedVar @a txt
    in insertVar tv  <$!>  lift ST.get >>= \case 
      Left err -> fail err 
      Right cxt -> lift ( (ST.modify . const) cxt) >> pure tv 

-- | Type checker for argument variables 
checkVar :: forall (a :: DSLType) 
          . T.Text  -> Sing a -> DSLParser (Either String (TypedVar a)) 
checkVar  txt sB = lift (ST.gets $ view tyCxt) >>= \cxt -> 
  case M.lookup txt cxt of
    Nothing -> pure . Left $ "Error: " 
                          <> "The variable " 
                          <> T.unpack txt
                          <> "has not been defined!" 
    Just (Some (MkTypeDict sX)) -> case decideEquality sB sX of 
        Just Refl -> withSingI sB $ pure . Right . MkTypedVar $ txt 
        Nothing   -> pure . Left $ "Error: Type mismatch! "
                                 <> "The variable "
                                 <> T.unpack txt
                                 <> " is being used as type "
                                 <> (show . fromSing $ sB)
                                 <> " but was previously assigned to a value of type "
                                 <> (show . fromSing $ sX) 

lookupType :: T.Text -> DSLParser (Maybe (Some TypeDict))
lookupType txt = lift (ST.gets $ view tyCxt) >>= \cxt -> pure $ M.lookup txt cxt 

lookupPlugin :: T.Text -> DSLParser (Maybe FilePath)
lookupPlugin txt = lift (ST.gets (^? (plCxt . ix txt))) 

insertPlugin :: T.Text -> FilePath -> DSLParser ()
insertPlugin txt pl = lift (ST.gets (^? (plCxt . ix txt))) >>= \case 
        Nothing -> lift . ST.modify' $ set (plCxt . ix txt) pl
        Just _  -> fail $ "Error: Plugin alias " <> T.unpack txt <> " clashes with a previously imported plugin alias"

{----------------------------------------------
 ----------------------------------------------
               Primitive Parsers 
-----------------------------------------------
-----------------------------------------------}

-- | Match a token, return ()
tk :: Tok -> DSLParser ()
tk t = void $ satisfy (== t) 

w32 :: DSLParser Word32 
w32 = do 
  IntLike n <- satisfy (\case {IntLike _ -> True ; _ -> False})
  pure . fromIntegral $ n 

{----------------------------------------------
 ----------------------------------------------
               DSL Syntax Parser 
-----------------------------------------------
-----------------------------------------------}

dslExp :: DSLParser (Some DSLExp)
dslExp = label "an expression" $ choice [ 
                  try $ label "a command" cmd
                , try $ label "an assignment expression" assign
                , try $ label "a query builder" qExp
                , try $ label "an if-then-else expression" ifThen
                , try $ label "an append expression" _append
                , try $ label "a concat expression" _concat
                , try $ label "an isEmpty expression" _isempty
                , try $ label "a typed variable" tyVar
                , between (tk LParen) (tk RParen) dslExp]

{----------------------------------------------
 ----------------------------------------------
               Command/Statement Parser 
-----------------------------------------------
-----------------------------------------------}

cmd :: DSLParser (Some DSLExp)
cmd = label "A command" $ 
      Some . Command <$> 
      choice [cmdExit
             ,cmdPrint 
             ,cmdWriteJSON 
             ,cmdHash
             ,cmdShowType]
 where 
  cmdShowType = do 
    tk (CmdTok ShowTypeTok) 
    Some e <- dslExp 
    pure $ SHOWTYPE e 

  cmdExit = do 
    tk (CmdTok ExitTok)
    pure  EXIT 

  cmdPrint :: DSLParser DSLCommand 
  cmdPrint = do 
    tk (CmdTok PrintTok)
    Some e <- dslExp
    withDict (expDict e) $ pure (PPRINT e)

  cmdWriteJSON :: DSLParser DSLCommand 
  cmdWriteJSON = do 
    tk (CmdTok WriteJSONTok)
    Some e <- dslExp 
    LitString fPath <- satisfy (\case {LitString _ -> True ; _ -> False}) 
    mTag <- option Nothing (Just <$> lString)
    withDict (expDict e) $ pure $ WRITEJSON e (T.unpack fPath) mTag   

  cmdHash = do 
    tk (CmdTok WriteHashTok)
    Some e <- dslExp 
    LitString fPath <- satisfy (\case {LitString _ -> True ; _ -> False}) 
    (case expSing e of 
        SREGKEY       -> pure $  HASH Dict e (T.unpack fPath) 

        SLIST SREGKEY -> pure $ HASH Dict e (T.unpack fPath) 

        SLIST SVAL    -> pure $ HASH Dict e (T.unpack fPath) 

        _ -> fail $ 
              "Error in `writeHash` command: " 
              <> "Only Registry Keys, Lists of Registry Keys,"
              <> " and Lists of Registry Values can be hashed.") :: DSLParser DSLCommand 


{----------------------------------------------
 ----------------------------------------------
              Expression parser 
-----------------------------------------------
-----------------------------------------------}

tyVar :: DSLParser (Some DSLExp)
tyVar = do 
  Name n <- satisfy (\case {Name _ -> True ; _ -> False})
  lookupType n >>= \case 
    Nothing -> fail $ "The variable `" <> T.unpack n <> "` has not been assigned!"
    Just (Some td@(MkTypeDict s)) -> pure $ go n td  
 where 
   go :: forall a. T.Text -> TypeDict a -> Some DSLExp 
   go txt (MkTypeDict s) = Some . DSLVar $ withSingI s $ MkTypedVar @a txt 

_concat :: DSLParser (Some DSLExp)
_concat = do 
  tk ConcatTok 
  Some ex <- dslExp 
  case expSing ex of 
    (SLIST (SLIST x)) -> pure . Some $ Concat ex 
    _ -> fail "Cannot `concat` something that isn't a list of lists"

_append :: DSLParser (Some DSLExp) 
_append = do 
  tk AppendTok 
  Some ex1 <- dslExp 
  Some ex2 <- dslExp 
  case (expSing ex1, expSing ex2) of
    (sA@(SLIST a), sB@(SLIST b)) -> 
      case decideEquality sA sB of
        Just Refl -> pure . Some $ Append ex1 ex2 
        Nothing -> fail "Arguments to append are not the same type!"
    _ -> fail "Arguments to append are not both lists!" 

_isempty :: DSLParser (Some DSLExp)
_isempty = do 
  tk IsEmptyTok 
  Some ex1 <- dslExp 
  case expSing ex1 of 
    SLIST a -> pure . Some $ IsEmpty ex1
    _       -> fail "argument to append must be a list!" 

ifThen :: DSLParser (Some DSLExp)
ifThen = do 
  tk If 
  Some e1 <- dslExp 
  case expSing e1 of 
     SBOOL -> do 
       tk Then 
       Some e2 <- dslExp 
       tk Else 
       Some e3 <- dslExp 
       case decideEquality (expSing e2) (expSing e3) of 
         Just Refl -> pure . Some $ IfThen e1 e2 e3 
         Nothing -> fail "Type mismatch in If-Then-Else Expression: Then/Else branches don't match."
     _ -> fail "Type mismatch in If-Then-Else expression: First expression doesn't evaluate to Bool!"

assign :: DSLParser (Some DSLExp)
assign = do 
  Name n <- satisfy (\case {Name _ -> True ; _ -> False})
  tk LArrow
  qExp >>= \case 
    Some exp -> case expSing exp of 
      sF -> do 
        tv <-  withDict (expDict exp) mkVar sF n 
        pure . Some $ Assign exp (withSingI sF $ MkTypedVar n)

-- | this is only possible w/ singletons, ty dr. eisenberg 
qExp :: DSLParser (Some DSLExp)
qExp = choice [try varQuery,rootQuery] 
 where 
  rootQuery :: DSLParser (Some DSLExp)
  rootQuery = do 
    tk Query
    tk (QBTok Root) 
    qbs <- between (tk LParen) (tk RParen) (someQB `sepBy1` tk Pipe)
    case composeQExpRoot qbs of 
      Left err -> fail err 
      Right (Some f)  -> pure . Some . RootQuery $ f 

  composeQExpRoot :: [SomeQB] -> Either String (Some Focus)
  composeQExpRoot [] = Left "Error: Empty Query Expression! (Or, more likely: Parser bug! Please report this)"
  composeQExpRoot (SomeQB x:xs) = case qbSing x of 
    (SROOT,f)   -> withSingI f $ withDict (qbDict x) $ go (FocusZ x) xs 

    (SREGKEY,f) -> let fz = FocusZ ROOTCELL 
                       fs = withSingI f $ withDict (qbDict x) $ FocusS fz x 
                   in withSingI f $ withDict (qbDict x) $ go fs xs 

    _           -> Left $ "Error: The first sub-query does not target the Hive Root" 
    where 
      go :: forall b. SingI b => Focus b ->  [SomeQB] -> Either String (Some Focus)
      go acc [] = Right $ Some acc
      go acc (SomeQB b:bs) = case qbSing b of 
        (l,r) -> case decideEquality l (sing @b) of 
          Nothing -> Left $ "Error: Type Mismatch In Query (make this more informative)"
          Just Refl -> withSingI r $ withDict (qbDict b) $ go (extendFocus Refl acc b) bs  

  varQuery = do 
    tk Query 
    Name nm <- satisfy (\case {Name _ -> True ; _ -> False})
    qbs <- between (tk LParen) (tk RParen) (someQB `sepBy1` tk Pipe) 
    case qbs of 
      [] -> fail "Error: Empty Query Expression! (Or, more likely: Parser bug! Please report this)"

      (SomeQB x:xs) -> case assertPretty (fst $ qbSing x) of
          dL  -> case qbDict x of 
            dR ->  case withDict dL $ withDict dR $ go ( liftCQB x) xs of 
              Left err -> fail err 
              Right (SomeQB z) -> case qbSing z of 
                (sL,sR) -> checkVar nm sL >>= \case 
                  Left err -> fail err 
                  Right tv -> pure $ Some (VarQuery tv z)
   where 
     go :: forall a b. (PrettyRefl a, PrettyRefl b) => CompositeQB a b -> [SomeQB] -> Either String SomeQB 
     go cqb []            = Right $ SomeQB (COMPOSED cqb) 
     go cqb (SomeQB x:xs) = case cqbSing cqb of 
       (s,e_) -> case qbSing x of 
         (s_,e) -> case decideEquality e_ s_ of 
           Just Refl -> case withDict (qbDict x) composeQB cqb x of 
             blah -> case cqbDict blah of 
               (Dict,Dict) -> go blah xs
           Nothing -> let t1Str = show . fromSing $ e_ 
                          t2Str = show . fromSing $ s_
                      in  Left $ "Error: Couldn't match " <> t1Str <> " with " <> t2Str <> " in query expression"

{----------------------------------------------
 ----------------------------------------------
               Query Builder parsers 
-----------------------------------------------
-----------------------------------------------}

someQB :: DSLParser SomeQB
someQB  = choice [
        someVals 
      , someKeyPath 
      , someSubkeys 
      , someMatchName
      , someMatchValName 
      , someMatchValData 
      , someMap 
      , someSelect 
      , someConcatMap 
      , someExpand]
 where
    filePath :: DSLParser FilePath 
    filePath = do 
        LitString fPath <- satisfy (\case {LitString _ -> True ; _ -> False})
        pure . T.unpack $ fPath 

    someVals = do 
      tk (QBTok Vals)
      pure . SomeQB $ VALS 

    someExpand = do 
      tk (QBTok Expand)
      w <- option Nothing (Just <$> w32) 
      pure . SomeQB $ EXPAND w 

    someMatchName = do 
      tk (QBTok MatchName)
      LitString n <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let n' = BC.pack (T.unpack n) 
      pure . SomeQB $ MATCHKEYNAME n' 

    someMatchValName = do 
      tk (QBTok MatchValName) 
      LitString n <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let n' = BC.pack (T.unpack n)
      pure . SomeQB $ MATCHVALNAME n' 

    someMatchValData = do 
      tk (QBTok MatchValData)
      LitString n1 <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let n1' = BC.pack . T.unpack $ n1
      pure . SomeQB $ MATCHVALDATA n1'  

    someMap = do 
      tk (QBTok Map)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case (qbDict1 qb, qbDict qb) of 
        (Dict,Dict) -> pure . SomeQB $ MAP qb

-- for some reason we need the type annotations on the next two
    someSelect :: DSLParser SomeQB 
    someSelect = do 
      tk (QBTok Select)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case qbSing qb of 
          (_,SBOOL) -> withDict (qbDict1 qb) $ pure . SomeQB $ SELECT qb 
          _ -> fail "Type mismatch in `select` function"

    someConcatMap :: DSLParser SomeQB 
    someConcatMap = do 
      tk (QBTok ConcatMap)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case qbSing qb of 
          (_,SLIST x) ->  pure . SomeQB $ withDict (qbDict1 qb) $ withDict (qbDict qb) $ CONCATMAP qb 
          _ -> fail "Type mismatch in `concatMap` function"

    someSubkeys = do 
      tk (QBTok SubKeys)
      pure . SomeQB $ SUBKEYS 

    someKeyPath :: DSLParser SomeQB 
    someKeyPath = do 
      tk (QBTok KeyPath)
      (LitString kPath) <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let kPath' =  toKeyPath . T.unpack $ kPath 
      pure . SomeQB $ KEYPATH kPath' 

lString :: (MonadParsec e s m, MonadFail m, Token s ~ Tok) => m T.Text
lString = do 
  LitString s <- satisfy (\case {LitString _ -> True ; _ -> False})
  pure s 



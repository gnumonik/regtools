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

-- | This is more or less our Type Checker 
insertVar :: forall a. PrettyRefl a => TypedVar (a :: DSLType) -> Context -> Either String Context 
insertVar tv@(MkTypedVar txt) cxt = case M.lookup txt (cxt ^. tyCxt) of
  Nothing -> Right $ over tyCxt (M.insert txt (Some $ MkTypeDict $ sing @a)) cxt 
  Just (Some (MkTypeDict t'))  -> case decideEquality t' (sing @a) of 
    Nothing -> Left $ "Type mismatch! The variable " 
                    <> (T.unpack txt) 
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
    in insertVar tv  <$!>  lift (ST.gets fst) >>= \case 
      Left err -> fail err 
      Right cxt -> lift ( (ST.modify . over _1 . const) cxt) >> pure tv 

-- | Type checker for argument variables 

checkVar :: forall (a :: DSLType) 
          . T.Text  -> Sing a -> DSLParser (Either String (TypedVar a)) 
checkVar  txt sB = lift (ST.gets $ view (_1 . tyCxt)) >>= \cxt -> 
  case M.lookup txt cxt of
    Nothing -> pure . Left $ "Error: " 
                          <> "The variable " 
                          <> (T.unpack txt)
                          <> "has not been defined!" 
    Just (Some (MkTypeDict sX)) -> case decideEquality sB sX of 
        Just Refl -> withSingI sB $ pure . Right . MkTypedVar $ txt 
        Nothing   -> pure . Left $ "Error: Type mismatch! "
                                 <> "The variable "
                                 <> (T.unpack txt)
                                 <> " is being used as type "
                                 <> (show . fromSing $ sB)
                                 <> " but was previously assigned to a value of type "
                                 <> (show . fromSing $ sX) 

lookupType :: T.Text -> DSLParser (Maybe (Some TypeDict))
lookupType txt = lift (ST.gets $ view (_1 . tyCxt)) >>= \cxt -> pure $ M.lookup txt cxt 

lookupPlugin :: T.Text -> DSLParser (Maybe FilePath)
lookupPlugin txt = lift (ST.gets (^? (_1 . plCxt . ix txt))) 

insertPlugin :: T.Text -> FilePath -> DSLParser ()
insertPlugin txt pl = lift (ST.gets (^? (_1 . plCxt . ix txt))) >>= \case 
        Nothing -> lift . ST.modify' $ set (_1 . plCxt . ix txt) pl
        Just _  -> fail $ "Error: Plugin alias " <> T.unpack txt <> " clashes with a previously imported plugin alias"


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
    Some (RootQuery f) -> case focSing f of 
      sF -> do 
        tv <-  withDict (focDict f) $ mkVar sF n 
        pure . Some $ Assign f (withSingI sF $ MkTypedVar n)
    _ -> fail "Assignment failure. This really really shouldn't happen."

cmd :: DSLParser (Some DSLExp)
cmd = Some . Command <$> 
      choice [cmdExit
             -- ,cmdHelp
             -- ,cmdDeadSpace
             ]
 where 
  cmdExit = do 
    tk (CmdTok ExitTok)
    pure  EXIT 

  cmdLoad = do 
    tk (CmdTok LoadTok)
    LitString pPath <- satisfy (\case {LitString _ -> True ; _ -> False}) 
    Name nm <- satisfy (\case {Name _ -> True ; _ -> False})
    insertPlugin nm (T.unpack pPath)
    pure $ LOAD (T.unpack pPath) nm

  cmdRun = do 
    tk (CmdTok RunTok)
    Name nm <- satisfy (\case {Name _ -> True ; _ -> False})
    lookupPlugin nm >>= \case 
      Nothing -> fail $ "No plugin with the alias " <> T.unpack nm <> "has been loaded"
      Just inPath -> do 
        LitString outPath <- satisfy (\case {LitString _ -> True ; _ -> False})
        pure $ RUN inPath (T.unpack outPath)

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

    (SANY,f)    -> let fz = FocusZ ROOTCELL 
                       fs = withDict (qbDict x) $ unsafeExtendFocus fz x 
                   in withSingI f $ withDict (qbDict x) $ go fs xs  

    _           -> Left $ "Error: The first sub-query does not target the Hive Root" 
    where 
      go :: forall b. SingI b => Focus b ->  [SomeQB] -> Either String (Some Focus)
      go acc [] = Right $ Some acc
      go acc (SomeQB b:bs) = case qbSing b of 
        (SANY,r) -> withSingI r $ withDict (qbDict b) $ go (unsafeExtendFocus acc b) bs 
        (l,r) -> case decideEquality l (sing @b) of 
          Nothing -> Left $ "Error: Type Mismatch In Query (make this more informative)"
          Just Refl -> withSingI r $ withDict (qbDict b) $ go (extendFocus Refl acc b) bs  

  varQuery = do 
    tk Query 
    Name nm <- satisfy (\case {Name _ -> True ; _ -> False})
    qbs <- between (tk LParen) (tk RParen) (someQB `sepBy1` tk Pipe) 
    case qbs of 
      [] -> fail "Error: Empty Query Expression! (Or, more likely: Parser bug! Please report this)"

      (SomeQB x:xs) -> case qbSing x of 
        (SANY,_) -> lookupType nm >>= \case 
          Nothing -> fail "Error: Cannot deduce type of query expression. (Did you use a variable before assigning it?)"
          Just (Some td@(MkTypeDict sX)) ->  
            let x' = unsafeLiftCQB x td 
            in case cqbDict x' of 
                (dL,dR) ->  case  withDict dL $ withDict dR $ go x' xs of 
                  Left err -> fail err 
                  Right (SomeQB z) -> case qbSing z of 
                    (sL,sR) -> checkVar nm sL >>= \case 
                      Left err -> fail err 
                      Right tv -> pure $ Some (VarQuery tv z)

        _        -> case qbDict1 x of
          Nothing -> fail "Invalid query. (Probably you tried to use `map` or `select` as the first expression in sequence)" 
          Just dL  -> case qbDict x of 
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
         (SANY,e) -> case withDict (qbDict x) $ unsafeComposeQB cqb x of
           blah   -> case cqbDict blah of 
             (Dict,Dict) -> go blah xs  
         (s_,e) -> case decideEquality e_ s_ of 
           Just Refl -> case withDict (qbDict x) composeQB cqb x of 
             blah -> case cqbDict blah of 
               (Dict,Dict) -> go blah xs
           Nothing -> let t1Str = show . fromSing $ e_ 
                          t2Str = show . fromSing $ s_
                      in  Left $ "Error: Couldn't match " <> t1Str <> " with " <> t2Str <> " in query expression"



-- if something breaks its probably because i have/don't have 'try' somewhere here 
someQB :: DSLParser SomeQB
someQB  = choice [
    --    someRootCell ,
        someVals 
      , someFilterValues 
      , someFilterSubkeys 
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
      , someExpand
      , someHashKey 
      , someHashKeys]
 where
    filePath :: DSLParser FilePath 
    filePath = ST.gets snd >>= \case 
      REPLMode -> do 
        LitString fPath <- satisfy (\case {LitString _ -> True ; _ -> False})
        pure . T.unpack $ fPath 
      PluginMode fPath -> do 
        tk FPathVar 
        pure fPath  

    someVals :: DSLParser SomeQB 
    someVals = do 
      tk (QBTok Vals)
      pure . SomeQB $ VALS 

    someFilterValues :: DSLParser SomeQB 
    someFilterValues = do 
      tk (QBTok FilterValues)
      SomeQB qb <- between (tk LParen) (tk RParen) someQB 
      case qbSing qb of 
        (SVAL,SBOOL) ->  pure . SomeQB $ FILTERVALUES qb
        _             -> fail "Type mismatch in `map` function"

    someFilterSubkeys :: DSLParser SomeQB 
    someFilterSubkeys = do 
        tk (QBTok FilterSubkeys)
        SomeQB qb <- between (tk LParen) (tk RParen) someQB 
        case qbSing qb of 
          (SREGKEY,SBOOL) -> pure . SomeQB $ FILTERSUBKEYS qb
          _               -> fail "Type mismatch in `map` function"        

    someHashKey :: DSLParser SomeQB 
    someHashKey = do 
      tk (QBTok HashKey)
      fPath <- filePath 
      pure . SomeQB $ HASHKEY fPath 

    someHashKeys :: DSLParser SomeQB 
    someHashKeys = do 
      tk (QBTok HashKeys)
      fPath <- filePath 
      pure . SomeQB $ HASHKEYS fPath

    someExpand :: DSLParser SomeQB 
    someExpand = do 
      tk (QBTok Expand)
      w <- option Nothing (Just <$> w32) 
      pure . SomeQB $ EXPAND w 

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
      let n1' = BC.pack . T.unpack $ n1
      pure . SomeQB $ MATCHVALDATA n1'  

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
{--
    someRootCell :: DSLParser SomeQB 
    someRootCell = do 
      tk (QBTok Root)
      pure . SomeQB $ ROOTCELL 
--}
    someKeyPath :: DSLParser SomeQB 
    someKeyPath = do 
      tk (QBTok KeyPath)
      (LitString kPath) <- satisfy (\case {LitString _ -> True ; _ -> False}) 
      let kPath' =  toKeyPath . T.unpack $ kPath 
      pure . SomeQB $ KEYPATH kPath' 

    someWriteJSON :: DSLParser SomeQB 
    someWriteJSON = do 
      tk (QBTok WriteJSON)
      fPath <- filePath
      jsonTag <- option Nothing (Just <$> lString) 
      pure . SomeQB $ WRITEJSON fPath jsonTag  

lString = do 
  LitString s <- satisfy (\case {LitString _ -> True ; _ -> False})
  pure s 



module Explore.LexDSL where


import Prelude hiding (lex)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer 
import Explore.Magic 
import qualified Data.Text as T
import Data.Void
import Control.Monad (void)
import Text.Megaparsec.Char
import Data.Functor (($>))
import Types (CCTok(..))
import Data.Kind (Type)
import Data.Singletons (SingI (sing), SomeSing (SomeSing), SingKind (toSing), withSingI)

-- our go-to indexedcomonadstore. I think we're actually gonna need it :-( 
import Control.Lens.Internal.Context 
import Data.Word
import Explore.Optics.Utils 
import Data.Singletons.Decide



type Lexer = Parsec Void T.Text Tok 

sc :: (MonadParsec e s f, Token s ~ Char) => f ()
sc = void $ many spaceChar 

lex :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lex = lexeme sc 

data Tok 
  = Query 
  | LArrow 
  | Pipe 
  | LParen 
  | RParen 
  | QBTok QBToken 
  | IntLike Int 
  | CTok CCTok 
  | LitString T.Text 
  | Name T.Text deriving (Show, Eq, Ord)

data QBToken 
  = RegItem 
  | RootCell 
  | FindCell 
  | GetVal 
  | StblSubkeys 
  | VolSubkeys 
  | AllSubkeys 
  | VKRecs 
  | KeyValues 
  | AllKVs 
  | PPrint 
  | KeyPath deriving (Show, Eq, Ord)


dsltoks :: Lexer 
dsltoks = choice [
    try query 
  , try intLike 
  , try qbTok 
  , try cTok 
  , pipe
  , lArrow 
  , lParen 
  , rParen 
  , try litString
  , name ]

testLex :: T.Text -> IO ()
testLex = parseTest (some dsltoks) 

name :: Lexer 
name = lex $ do 
  first <- lowerChar 
  rest  <- many alphaNumChar 
  pure . Name . T.pack $ first:rest

litString :: (MonadParsec e s m, Token s ~ Char) => m Tok
litString =  do 
      s <- between (lexChar_ '"') (lexChar_ '"') (some $ alphaNumChar <|> char ' ') 
      pure . LitString . T.pack $ s 

lexStr_ :: (MonadParsec e s f, Token s ~ Char) => Tokens s -> f ()
lexStr_ str = void . lex $ string str  

lexChar_ :: (MonadParsec e s f, Token s ~ Char) => Char -> f ()
lexChar_ c = void . lex $ char c 

query :: Lexer 
query = lexStr_ "query" $>  Query 

lArrow :: Lexer 
lArrow = lexStr_ "<-" $> LArrow 

pipe :: Lexer 
pipe = lexChar_ '|' $> Pipe 

lParen :: Lexer 
lParen = lexChar_ '(' $> LParen 

rParen :: Lexer 
rParen = lexChar_ ')' $> RParen 

intLike :: Lexer 
intLike = IntLike . fromIntegral <$> decimal 

qbTok :: Lexer 
qbTok 
  = choice [
    f "regItem"  RegItem
  , f "rootCell" RootCell
  , f "findCell" FindCell 
  , f "getVal"   GetVal  
  , f "stblSubkeys" StblSubkeys 
  , f "volSubkeys" VolSubkeys  
  , f "allSubkeys" AllSubkeys  
  , f "vkRecs" VKRecs  
  , f "keyValues" KeyValues  
  , f "allKVs" AllKVs 
  , f "keyPath" KeyPath
  , f "print" PPrint 
  ]
 where 
    f :: T.Text -> QBToken -> Lexer 
    f str t = try $ lexStr_ str $> QBTok t 

cTok :: Lexer 
cTok 
  = choice [
    f "sk" SKRec
  , f "nk" NKRec 
  , f "vk" VKRec 
  , f "skl" SKList 
  , f "vl"  VList 
  , f "val" Val 
  ]
 where 
   f :: T.Text -> CCTok -> Lexer 
   f str t = lexStr_ str $> CTok t 

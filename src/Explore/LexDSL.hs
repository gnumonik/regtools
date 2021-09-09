module Explore.LexDSL where


import Prelude hiding (lex)
import Text.Megaparsec
    ( (<|>),
      between,
      choice,
      MonadParsec(try),
      parseTest,
      many,
      some,
      Stream(Token, Tokens), satisfy, notFollowedBy, option, Parsec )
import Text.Megaparsec.Char.Lexer ( decimal, lexeme ) 
import qualified Data.Text as T
import Control.Monad (void)
import Text.Megaparsec.Char
    ( alphaNumChar, char, lowerChar, spaceChar, string )
import Data.Functor (($>))
import Types
import Data.Kind (Type)
import Data.Singletons (SingI (sing), SomeSing (SomeSing), SingKind (toSing), withSingI)
import Data.Void (Void)

sc :: (MonadParsec e s f, Token s ~ Char) => f ()
sc = void $ many spaceChar 

lex = lexeme sc 

dsltoks :: Lexer 
dsltoks = choice [
    try query 
  , try intLike 
  , try qbTok 
  , try cmdTok
  , pipe
  , lArrow 
  , lParen 
  , rParen 
  , try litString
  , name ]

pluginToks :: Lexer 
pluginToks = choice [
    try query 
  , try intLike 
  , try qbTok 
  , try cmdTok
  , pipe
  , lArrow 
  , lParen 
  , rParen
  , lCurly 
  , rCurly 
  , plugin 
  , as 
  , fPathVar
  , try litString
  , name ]

testLex :: T.Text -> IO ()
testLex = parseTest (some dsltoks) 

cmdTok :: Lexer 
cmdTok = choice [
    f "help" HelpTok 
  , f "exit" ExitTok
  , f "load" LoadTok
  , f "run"  RunTok 
  ]
 where 
    f :: T.Text -> CmdToken -> Lexer 
    f str t = lex . try $ lexStr_ str $> CmdTok t 
name :: Lexer 
name = lex $ do 
  first <- lowerChar 
  rest  <- many alphaNumChar 
  pure . Name . T.pack $ first:rest

litString :: (MonadParsec e s m, Token s ~ Char) => m Tok
litString =  do 
      s <- between (lexChar_ '"') (lexChar_ '"') (some $ satisfy (/= '"')) 
      pure . LitString . T.pack $ s 

lexStr_ :: T.Text -> Parsec Void T.Text ()
lexStr_ str = void . lex $ go 
  where 
    go :: Lexer_ 
    go = do 
      s <- string str 
      nope <- option Nothing (Just <$> alphaNumChar)
      case nope of 
        Nothing -> pure ()
        Just _  -> fail "boom"

lexChar_ :: (MonadParsec e s f, Token s ~ Char) => Char -> f ()
lexChar_ c = void . lex $ char c 

query :: Lexer 
query = lexStr_ "query" $>  Query 

lArrow :: Lexer 
lArrow = lexStr_ "<-" $> LArrow 

pipe :: Lexer 
pipe = choice [try pipe2, pipe1]
  where 
    pipe2 = lexStr_ ">>>" $> Pipe 

    pipe1 = lexChar_ '|' $> Pipe 

lParen :: Lexer 
lParen = lexChar_ '(' $> LParen 

rParen :: Lexer 
rParen = lexChar_ ')' $> RParen 

intLike :: Lexer 
intLike = lex $ IntLike . fromIntegral <$> decimal 

lCurly = lexChar_ '{' $> LCurly 

rCurly = lexChar_ '}' $> RCurly 

plugin = lexStr_ "PLUGIN" $> Plugin 

fPathVar = lexStr_ "$FILEPATH$" $> FPathVar 



as = lexStr_ "AS" $> As 

qbTok :: Lexer 
qbTok 
  = choice [
    f "root" Root
  , f "key" KeyPath
  , f "print" PPrint 
  , f "writeJSON" WriteJSON
  , f "keyNameHas" MatchName 
  , f "valNameHas" MatchValName
  , f "valDataHas" MatchValData 
  , f "map" Map 
  , f "select" Select 
  , f "concatMap" ConcatMap
  , f "subkeys" SubKeys
  , f "expand" Expand
  , f "hashKey" HashKey 
  , f "hashKeys" HashKeys
  , f "filterValues" FilterValues 
  , f "filterSubkeys" FilterSubkeys 
  , f "values" Vals 
  ]
 where 
    f :: T.Text -> QBToken -> Lexer 
    f str t = lex . try $ lexStr_ str $> QBTok t 


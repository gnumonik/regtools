module Explore.Optics.Utils where

import Data.Word (Word32)
import Data.List 
import Text.Megaparsec 
import Text.Megaparsec.Char 
import qualified Data.Text as T 
import Data.Void 

type Parser = Parsec Void T.Text 

parseKeyPath :: Parser [T.Text]
parseKeyPath = map T.pack <$> some (satisfy (/= '\\')) `sepBy1` char '\\'



isAsciiFlag :: Word32 -> Bool 
isAsciiFlag x = x == 0x0020 

toKeyPath :: String -> [String]
toKeyPath = go ([],"") 
  where 
    go :: ([String],String) -> [Char] -> [String]
    go (resAcc,strAcc)  []    = reverse $ reverse strAcc : resAcc 
    go (resAcc,strAcc) (c:cs) = 
      if c == '\\' 
        then  let str = reverse strAcc 
              in go (str:resAcc,[]) cs 
        else go (resAcc,c:strAcc) cs 
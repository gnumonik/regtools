module Explore.Optics.Utils where

import Data.Word (Word32)
import Data.List 

isAsciiFlag :: Word32 -> Bool 
isAsciiFlag x = x == 0x0020 

type KeyPath = [String]
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
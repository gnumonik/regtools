module Hash where

import Types 
import Crypto.Hash.MD5 
import qualified Data.ByteString as BS 
import Control.Lens 
import qualified Data.ByteString.Char8 as C8 
import qualified Data.Text as T 
import Data.List 
import Text.Hex 
import Time (prettyTime)
import Data.Serialize

hashKey :: RegistryKey -> KeyHash 
hashKey rKey = KeyHash nm hashTime hashVals where 
  path =  (<> "\\") . intercalate "\\" . map C8.unpack  $ (rKey ^. keyParents)
  nm   = case rKey ^. keyParents of 
    [] -> rootPath 
    _  -> T.pack $ path <> (C8.unpack $ rKey ^. keyName )

  hashTime = encodeHex $ hash (C8.pack . prettyTime $ rKey ^. keyTime)

  valString = BS.concat $ map (uncurry (<>) . over _2 valToBS) (map getNamedVal . getNamedVals $ rKey ^. keyValues)

  hashVals = encodeHex $ hash valString 

valToBS :: Value -> BS.ByteString 
valToBS = \case 
  REG_NONE bs                       -> bs 
  REG_SZ bs                         -> bs 
  REG_EXPAND_SZ bs                  -> bs 
  REG_BINARY bs                     -> bs 
  REG_DWORD w                       -> runPut (putWord32le w) 
  REG_DWORD_LITTLE_ENDIAN w         -> runPut (putWord32le w) 
  REG_DWORD_BIG_ENDIAN w            -> runPut (putWord32be w) 
  REG_LINK bs                       -> bs 
  REG_MULTI_SZ bs                   -> bs 
  REG_RESOURCE_LIST bs              -> bs 
  REG_FULL_RESOURCE_DESCRIPTOR bs   -> bs 
  REG_RESOURCE_REQUIREMENTS_LIST bs -> bs 
  REG_QWORD w                       -> runPut (putWord64le w)



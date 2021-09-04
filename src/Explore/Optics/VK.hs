module Explore.Optics.VK where

import Explore.ExploreM
import Types
import Lib
import qualified Data.Vector as V
import Data.Word (Word32)
import Control.Lens ( (^?), (^.) )
import Explore.Optics.Root
import Explore.Optics.General
import Control.Monad.Look
import Control.Lens.Action
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Serialize
import Debug.Trace



val :: MFold VKRecord Value
val  = mkMonadicFold go 
  where 
    go :: VKRecord -> ExploreM Value
    go vk = if checkVKPointer (vk ^. dataLength)
            then look >>= \hData -> 
                  hData ^!? regItem @Value (vk ^. dataPtr) >>= \case 

                    Nothing -> qErr 
                             $ QueryError (vk ^. dataPtr) (T.pack "Value doesn't exist")

                    Just myVal -> pure myVal 

            else case runGet (valueData 4 (vk ^. valueType)) (runPut . putWord32le $ vk ^. dataPtr) of 
                  Left err -> qErr 
                            $ QueryError 0 (T.pack $ "Error reading truncated value from VK data pointer field: " <> err)
                  Right v  -> pure v 

namedVal :: MFold VKRecord [(BS.ByteString,Value)]
namedVal = mkMonadicFold go 
  where 
    go :: VKRecord -> ExploreM [(BS.ByteString,Value)]
    go vk = let valNm = vk ^. valName
            in if checkVKPointer (vk ^. dataLength)
            then look >>= \hData -> 
              hData ^!? regItem @Value (vk ^. dataPtr) >>= \case 
                Nothing -> pure []
                Just vx -> pure [(valNm,vx)] 

            else case runGet (valueData 4 (vk ^. valueType)) (runPut . putWord32le $ vk ^. dataPtr) of 
                  Left err -> trace err $ pure []
                  Right v  -> pure [(valNm,v)]
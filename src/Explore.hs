{-# LANGUAGE TupleSections #-}
module Explore where

import Data.Word 
import Types hiding (stableSubkeys)
import Lib ( HiveData, checkVKPointer, valueData ) 
import ParseM
    ( offset, parsed, rawBS, spaceMap, OccupiedSpace(getSpace) ) 
import Control.Lens
    ( Identity(Identity),
      (^?),
      folding,
      (^.),
      makeLenses,
      Ixed(ix),
      Leftmost,
      Fold, filtered ) 
import qualified Data.Map as M 
import qualified Data.Vector as V 
import Data.Foldable (Foldable(foldl'))
import qualified Data.ByteString as BS
import Data.Kind 
import Debug.Trace 
import Data.Char (ord)
import Control.Monad.State 
import Control.Monad.Reader 
import Data.Serialize
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Control.Lens.Action 
import qualified Data.ByteString.Char8 as BC 
import Control.Lens.Getter
import qualified Data.Sequence as Seq 
import qualified Data.Text as T 
import Control.Monad.Errors
import Control.Monad.Errors.Class
import Control.Monad.Look 

makeLenses ''HiveData 










 












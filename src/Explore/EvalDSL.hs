module Explore.EvalDSL where

import Types
import Data.Word (Word32)
import Control.Lens 
import Lib 
import Explore.ExploreM 
import Explore.Optics.General
import ParseM
import qualified Data.Map as M
import Control.Monad.Look
import Data.List (foldl')
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Vector as V
import Control.Lens.Action
import Explore.ParseDSLYO
import Control.Monad.Errors 
import Control.Monad.Errors.Class 
import qualified Data.Text as T 
import qualified  Control.Monad.Trans.State.Strict as ST  
import Explore.Magic
import Data.Kind
import Unsafe.Coerce 
import Data.Singletons
import Control.Monad.State.Class
import qualified Control.Monad.Trans.Reader as R 
import Control.Monad.Reader.Class 
import Control.Monad.Trans.Class 
import Control.Monad.IO.Class 
import Data.Constraint

type ValMap = M.Map T.Text DSLValue 

data DSLValue :: Type where 
  MkDSLValue :: (t ~ DSLToHask d) => Sing d -> t -> DSLValue 

type EvalM a = ST.StateT ValMap IO  a



evalDSL :: forall a. (String -> IO ()) -> HiveData -> DSLExp a -> EvalM (Maybe QueryErrors)
evalDSL printer hData = \case 
  QueryExp fcs -> case withDict (focDict fcs) $ collapseFocus fcs of 
    MkBoxedMFold fld ->  do 
      liftIO (query printer hData fld)  
      pure Nothing 

  Assign fcs (MkTypedVar txt) -> case withDict (focDict fcs) $ collapseFocus fcs of 
    MkBoxedMFold fld ->  do 
      x <- liftIO (query printer hData fld)
      case x of 
        Left err -> pure . Just $ err
        Right (a :: DSLToHask a)  -> do 
          let sF = focSing fcs 
          modify (M.insert txt (MkDSLValue sF a))
          pure Nothing

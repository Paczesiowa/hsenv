{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , TypeSynonymInstances
  , FlexibleInstances
  , Arrows
  #-}
module Util.Args.GetOpt where

import Util.Args.ArgArrow
import Util.Args.ArgDescr
import Util.Args.RawArgs
import Data.Maybe (fromMaybe)
import Control.Arrow

class GetOpt a b | a -> b where
    getOpt :: a -> ArgArrow () b

data Switch = Switch { switchName :: String
                     , switchHelp :: String
                     }

instance GetOpt Switch Bool where
    getOpt sd = proc () -> do
      addKnownArg [SwitchDescr (switchName sd) (switchHelp sd)] -< ()
      args <- askArgs -< ()
      returnA -< switchName sd `elem` switches args

data DynOpt = DynOpt { dynOptName        :: String
                     , dynOptTemplate    :: String
                     , dynOptDescription :: String
                     , dynOptHelp        :: String
                     }

instance GetOpt DynOpt (Maybe String) where
    getOpt dod = proc () -> do
      addKnownArg [ValArg (dynOptName dod)
                          (dynOptTemplate dod)
                          (DynValue $ dynOptDescription dod)
                          (dynOptHelp dod)] -< ()
      args <- askArgs -< ()
      returnA -< lookup (dynOptName dod) $ valArgs args

data StaticOpt = StaticOpt { staticOptName     :: String
                           , staticOptTemplate :: String
                           , staticOptDefault  :: String
                           , staticOptHelp     :: String
                           }

instance GetOpt StaticOpt String where
    getOpt sod = proc () -> do
      addKnownArg [ValArg (staticOptName sod)
                          (staticOptTemplate sod)
                          (DynValue $ staticOptDefault sod)
                          (staticOptHelp sod)] -< ()
      args <- askArgs -< ()
      returnA -< fromMaybe (staticOptDefault sod)
                          $ lookup (staticOptName sod)
                          $ valArgs args

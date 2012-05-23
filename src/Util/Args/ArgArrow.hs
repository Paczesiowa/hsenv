{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Args.ArgArrow where

import Util.Args.StaticArrowT
import Util.Args.RawArgs
import Util.Args.ArgDescr
import Data.Monoid (mempty)
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.Reader as Reader (liftIO)
import Control.Arrow
import Control.Category (Category)

newtype ArgArrow a b = ArgArrow (StaticArrowT KnownArgs (Kleisli (ReaderT Args IO)) a b)
    deriving (Category, Arrow, ArrowChoice)

runArgArrow :: ArgArrow () a -> Args -> IO a
runArgArrow (ArgArrow (StaticArrowT _ m)) = runReaderT $ runKleisli m ()

liftIO :: (a -> IO b) -> ArgArrow a b
liftIO m = ArgArrow $ StaticArrowT mempty $ Kleisli (Reader.liftIO . m)

addKnownArg :: KnownArgs -> ArgArrow () ()
addKnownArg = ArgArrow . addStatic

askArgs :: ArgArrow () Args
askArgs = ArgArrow $ StaticArrowT mempty $ Kleisli $ const ask

getKnownArgs :: ArgArrow a b -> KnownArgs
getKnownArgs (ArgArrow arrow) = getStatic arrow

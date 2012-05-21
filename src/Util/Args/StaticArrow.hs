module Util.Args.StaticArrow where

import Data.Monoid
import Control.Arrow
import qualified Control.Category as C

data StaticArrowT m arr a b = StaticArrowT m (arr a b)

instance (C.Category arr, Monoid m) => C.Category (StaticArrowT m arr) where
  id = StaticArrowT mempty C.id
  StaticArrowT m2 arr2 . StaticArrowT m1 arr1 =
      StaticArrowT (m2 `mappend` m1) $ arr2 C.. arr1

instance (Arrow arr, Monoid m) => Arrow (StaticArrowT m arr) where
  arr f = StaticArrowT mempty $ arr f
  first (StaticArrowT m arr) = StaticArrowT m $ first arr

instance (ArrowChoice arr, Monoid m) => ArrowChoice (StaticArrowT m arr) where
    left (StaticArrowT m arr) = StaticArrowT m $ left arr

addStatic :: (Monoid m, Arrow arr) => m -> StaticArrowT m arr a a
addStatic m = StaticArrowT m C.id

getStatic :: (Monoid m, Arrow arr) => StaticArrowT m arr a b -> m
getStatic (StaticArrowT m _) = m

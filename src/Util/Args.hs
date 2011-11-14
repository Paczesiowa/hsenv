{-# LANGUAGE Arrows #-}
module Util.Args where

-- library

import Control.Arrow
import qualified Control.Category as C
import Data.Maybe (fromJust)

type State = [(String, String)]

data ArgArrow a b = ArgArrow [String] (State -> a -> IO b)

runArgArrow :: State -> ArgArrow () a -> IO a
runArgArrow s (ArgArrow args m) = m s ()

usage :: ArgArrow a b -> String
usage (ArgArrow args _) = "Known arguments: " ++ unwords args

instance C.Category ArgArrow where
  id = ArgArrow [] $ \_ x -> return x
  ArgArrow args1 m1 . ArgArrow args2 m2 =
    ArgArrow (args1 ++ args2) $ \s x -> m2 s x >>= m1 s

instance Arrow ArgArrow where
  arr f = ArgArrow [] $ \_ x -> return $ f x
  first (ArgArrow args m) = ArgArrow args $ \s (x, y) -> do
    z <- m s x
    return (z, y)

getOption :: String -> ArgArrow () String
getOption arg = ArgArrow [arg] $ \s _ -> return $ fromJust $ lookup arg s

liftIO :: (a -> IO b) -> ArgArrow a b
liftIO m = ArgArrow [] $ \_ x -> m x

liftIO2 :: IO a -> ArgArrow () a
liftIO2 m = ArgArrow [] $ \_ _ -> m

-- end of library

data Options = Options Bool Int String deriving Show

foo :: ArgArrow () Options
foo = proc () -> do
  bool   <- getOption "bool" >>^ read   -< ()
  number <- getOption "number" >>^ read -< ()
  string <- getOption "string" -< ()
  returnA -< Options bool number string

main = do
  let args = [ ("bool", "True")
             , ("number", "1337")
             , ("string", "foo")
             ]
  putStrLn $ usage foo
  runArgArrow args foo >>= print

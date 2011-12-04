module Util.Args where

import Data.Monoid
import Control.Arrow
import qualified Control.Category as C
import Data.Maybe (fromMaybe)

{-# ANN module "HLint: ignore Use String" #-}
data Args = Args { switches :: [String]
                 , valArgs  :: [(String, String)]
                 }
    deriving Show

instance Monoid Args where
    mempty = Args [] []
    Args xs1 ys1 `mappend` Args xs2 ys2 = Args (xs1 ++ xs2) (ys1 ++ ys2)

breakOn :: Eq a => a -> [a] -> Maybe ([a], [a])
breakOn sep = aux []
  where aux _ [] = Nothing
        aux prevs (x:xs) | x == sep   = Just (reverse prevs, xs)
                         | otherwise = aux (x:prevs) xs

parseArgument :: String -> Args
parseArgument ('-':'-':arg) =
  case breakOn '=' arg of
    Nothing         -> mempty{switches = [arg]}
    Just (key, val) -> mempty{valArgs = [(key, val)]}
parseArgument ['-', _c] = mempty
parseArgument ('-':_k:_val) = mempty
parseArgument _ = mempty

parseArguments :: [String] -> Args
parseArguments args =
    case breakOn "--" args of
      Nothing -> mconcat $ map parseArgument args
      Just (args', _positionals) -> mconcat $ map parseArgument args'

data ArgDescr = Switch { argName :: String
                       , helpMsg :: String
                       }
              | ValArg { argName      :: String
                       , valTemplate  :: String
                       , defaultValue :: Maybe String
                       , helpMsg      :: String
                       }
     deriving (Show, Eq)

type KnownArgs = [ArgDescr]

data ArgArrow a b = ArgArrow KnownArgs (Args -> a -> IO b)

runArgArrow :: Args -> ArgArrow () a -> IO a
runArgArrow args (ArgArrow _ m) = m args ()

-- usage :: ArgArrow a b -> String
-- usage (ArgArrow args _) = "Known arguments: " ++ unwords args

-- TODO: check if info/descriptions match
mergeKnownArgs :: KnownArgs -> KnownArgs -> KnownArgs
mergeKnownArgs = (++)

instance C.Category ArgArrow where
  id = ArgArrow [] $ \_ x -> return x
  ArgArrow knArgs2 m2 . ArgArrow knArgs1 m1 =
    ArgArrow (mergeKnownArgs knArgs2 knArgs1) $ \s x -> m1 s x >>= m2 s

instance Arrow ArgArrow where
  arr f = ArgArrow [] $ \_ x -> return $ f x
  first (ArgArrow knArgs m) = ArgArrow knArgs $ \s (x, y) -> do
    z <- m s x
    return (z, y)

getSwitch :: String -> String -> ArgArrow () Bool
getSwitch name info = ArgArrow knArgs m
    where m args _ = return $ name `elem` switches args
          knArgs   = [Switch name info]

getOptionWithDefault :: String -> String -> String -> String -> ArgArrow () String
getOptionWithDefault name template default' info = ArgArrow knArgs m
    where knArgs = [ValArg name template (Just default') info]
          m args _ = return $ fromMaybe default' $ lookup name $ valArgs args

getOptionWithDefault2 :: String -> String -> String -> String -> ArgArrow () (Maybe String)
getOptionWithDefault2 name template defaultStr info = ArgArrow knArgs m
    where knArgs = [ValArg name template (Just defaultStr) info]
          m args _ = return $ lookup name $ valArgs args

liftIO :: IO a -> ArgArrow () a
liftIO m = ArgArrow [] $ \_ _ -> m

module Util.Args where

import Data.Monoid
import Control.Arrow
import qualified Control.Category as C

{-# ANN module "HLint: ignore Use String" #-}
data Args = Args { shortSwitches  :: [Char]
                 , longSwitches   :: [String]
                 , shortValArgs   :: [(Char, String)]
                 , longValArgs    :: [(String, String)]
                 , positionalArgs :: [String]
                 }
    deriving Show

instance Monoid Args where
    mempty = Args [] [] [] [] []
    Args ss1 ls1 sva1 lva1 pa1 `mappend` Args ss2 ls2 sva2 lva2 pa2 =
      Args (ss1 ++ ss2) (ls1 ++ ls2) (sva1 ++ sva2) (lva1 ++ lva2) (pa1 ++ pa2)

breakOn :: Eq a => a -> [a] -> Maybe ([a], [a])
breakOn sep = aux []
  where aux _ [] = Nothing
        aux prevs (x:xs) | x == sep   = Just (reverse prevs, xs)
                         | otherwise = aux (x:prevs) xs

parseArgument :: String -> Args
parseArgument ('-':'-':arg) =
  case breakOn '=' arg of
    Nothing         -> mempty{longSwitches = [arg]}
    Just (key, val) -> mempty{longValArgs = [(key, val)]}
parseArgument ['-', c] = mempty{shortSwitches = [c]}
parseArgument ('-':k:val) = mempty{shortValArgs = [(k, val)]}
parseArgument arg = mempty{positionalArgs = [arg]}

parseArguments :: [String] -> Args
parseArguments args =
    case breakOn "--" args of
      Nothing -> mconcat $ map parseArgument args
      Just (args', positionals) ->
        mconcat (map parseArgument args') `mappend` mempty{positionalArgs = positionals}

data KnownArg = LongAndShort String Char
              | Long String
              | Short Char
     deriving Show

data Multiplicity = Single
                  | Multiple
     deriving Show

data KnownArgs = KnownArgs { knownValueArgs :: [(KnownArg, Multiplicity)]
                           , knownSwitches  :: [(KnownArg, Multiplicity)]
                           , positionalsOk  :: Bool
                           }
     deriving Show

data ArgArrow a b = ArgArrow KnownArgs (Args -> a -> IO b)

runArgArrow :: Args -> ArgArrow () a -> IO a
runArgArrow args (ArgArrow _ m) = m args ()

-- usage :: ArgArrow a b -> String
-- usage (ArgArrow args _) = "Known arguments: " ++ unwords args

-- TODO: check if multiplicities match
mergeKnownArgs :: [(KnownArg, Multiplicity)] -> [(KnownArg, Multiplicity)] -> [(KnownArg, Multiplicity)]
mergeKnownArgs = (++)

instance C.Category ArgArrow where
  id = ArgArrow KnownArgs{ knownValueArgs = []
                         , knownSwitches  = []
                         , positionalsOk  = False
                         } $ \_ x -> return x
  ArgArrow knArgs2 m2 . ArgArrow knArgs1 m1 =
    ArgArrow KnownArgs{ knownValueArgs =
                           mergeKnownArgs (knownValueArgs knArgs1) (knownValueArgs knArgs2)
                      , knownSwitches  =
                           mergeKnownArgs (knownSwitches knArgs1) (knownSwitches knArgs2)
                      , positionalsOk  = positionalsOk knArgs1 || positionalsOk knArgs2
                      } $ \s x -> m1 s x >>= m2 s

instance Arrow ArgArrow where
  arr f = ArgArrow KnownArgs{ knownValueArgs = []
                            , knownSwitches  = []
                            , positionalsOk  = False
                            } $ \_ x -> return $ f x
  first (ArgArrow knArgs m) = ArgArrow knArgs $ \s (x, y) -> do
    z <- m s x
    return (z, y)

-- getOption :: String -> ArgArrow () String
-- getOption arg = ArgArrow [arg] $ \s _ -> return $ fromJust $ lookup arg s

-- liftIO :: (a -> IO b) -> ArgArrow a b
-- liftIO m = ArgArrow [] $ \_ x -> m x

-- liftIO2 :: IO a -> ArgArrow () a
-- liftIO2 m = ArgArrow [] $ \_ _ -> m

-- -- end of library

-- data Options = Options Bool Int String deriving Show

-- foo :: ArgArrow () Options
-- foo = proc () -> do
--   bool   <- getOption "bool" >>^ read   -< ()
--   number <- getOption "number" >>^ read -< ()
--   string <- getOption "string" -< ()
--   returnA -< Options bool number string

-- main = do
--   let args = [ ("bool", "True")
--              , ("number", "1337")
--              , ("string", "foo")
--              ]
--   putStrLn $ usage foo
--   runArgArrow args foo >>= print

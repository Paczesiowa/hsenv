{-# Language MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  #-}

module Util.Args where

import Data.Monoid
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, hPutStrLn)
import Control.Arrow
import qualified Control.Category as C
import Data.Maybe (fromMaybe)
import System.Environment (getProgName)
import Util.WordWrap (wordWrap)
import Data.Function (on)
import Data.List(sortBy)
import System.Environment (getArgs)

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

data DefaultValue = ConstValue String
                  | DynValue String
    deriving (Show, Eq)

data ArgDescr = SwitchDescr { argName :: String
                            , helpMsg :: String
                            }
              | ValArg { argName      :: String
                       , valTemplate  :: String
                       , defaultValue :: DefaultValue
                       , helpMsg      :: String
                       }
     deriving (Show, Eq)

type KnownArgs = [ArgDescr]

data ArgArrow a b = ArgArrow KnownArgs (Args -> a -> IO b)

data ArgParseResult a = Usage
                      | Help
                      | Error String
                      | OK a
    deriving (Show, Eq)

runArgArrow :: Args -> ArgArrow () a -> IO a
runArgArrow args (ArgArrow _ m) = m args ()

padTo :: String -> Int -> String
padTo s n = take n $ s ++ repeat ' '

showFlagDescr :: ArgDescr -> [String]
showFlagDescr argDescr = zipWith makeLine lefts msgLines
    where lefts    = [argLine] ++ repeat ""
          argLine  = case argDescr of
                       SwitchDescr name _ -> "--" ++ name
                       ValArg name tmpl _ _ -> concat ["--", name, "=", tmpl]
          msgLines = wordWrap 60 $ case argDescr of
                                     SwitchDescr _ hlp -> hlp
                                     ValArg _ _ default' help ->
                                         concat [help, "\n", defaultsLine default']
          defaultsLine (ConstValue s) = concat ["(defaults to '", s, "')"]
          defaultsLine (DynValue s)   = concat ["(defaults to ", s, ")"]
          makeLine infoLine descrLine = (infoLine `padTo` 20) ++ descrLine

helperArgArrow :: ArgArrow a b -> ArgArrow a (ArgParseResult b)
helperArgArrow (ArgArrow knargs m) = ArgArrow knargs' m'
    where knargs' = SwitchDescr "help" "Show this help message" : knargs
          m' args x | "help" `elem` switches args = return Help
                    | "usage" `elem` switches args = return Usage
                    | otherwise = OK `fmap` m args x

parseArgs :: ArgArrow () a -> String -> String -> IO a
parseArgs arrgArr version outro = do
  args   <- getArgs
  result <- runArgArrow (parseArguments args) arrgArr'
  case result of
    OK a -> return a
    Error s -> hPutStrLn stderr s >> exitFailure
    _ -> usage arrgArr' version outro >>= putStr >> exitSuccess
  where arrgArr' = helperArgArrow arrgArr


usage :: ArgArrow a b -> String -> String -> IO String
usage (ArgArrow args _) version outro = do
  self <- getProgName
  let intro = "usage: " ++ self ++ " [FLAGS]"
  return $ unlines $ [intro, "", "Flags:"] ++ flagsDescr ++ [""] ++  outro'
      where flagsDescr = concatMap showFlagDescr $ argDescrSort args
            argDescrSort = sortBy (compare `on` argName)
            outro' = wordWrap 80 outro


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

instance ArrowChoice ArgArrow where
    left (ArgArrow knArgs m) =
        ArgArrow knArgs $ \s x ->
            case x of
              Left y  -> Left `fmap` m s y
              Right y -> return $ Right y

liftIO :: (a -> IO b) -> ArgArrow a b
liftIO m = ArgArrow [] $ \_ x -> m x

class GetOpt a b | a -> b where
    getOpt :: a -> ArgArrow () b

data Switch = Switch { switchName :: String
                     , switchHelp :: String
                     }

instance GetOpt Switch Bool where
    getOpt sd = ArgArrow knArgs m
        where m args _ = return $ switchName sd `elem` switches args
              knArgs   = [SwitchDescr (switchName sd) (switchHelp sd)]

data DynOpt = DynOpt { dynOptName        :: String
                     , dynOptTemplate    :: String
                     , dynOptDescription :: String
                     , dynOptHelp        :: String
                     }

instance GetOpt DynOpt (Maybe String) where
    getOpt dod = ArgArrow knArgs m
        where knArgs = [ValArg (dynOptName dod)
                               (dynOptTemplate dod)
                               (DynValue $ dynOptDescription dod)
                               (dynOptHelp dod)]
              m args _ = return $ lookup (dynOptName dod) $ valArgs args

data StaticOpt = StaticOpt { staticOptName     :: String
                           , staticOptTemplate :: String
                           , staticOptDefault  :: String
                           , staticOptHelp     :: String
                           }

instance GetOpt StaticOpt String where
    getOpt sod = ArgArrow knArgs m
        where knArgs = [ValArg (staticOptName sod)
                               (staticOptTemplate sod)
                               (DynValue $ staticOptDefault sod)
                               (staticOptHelp sod)]
              m args _ = return $ fromMaybe (staticOptDefault sod)
                                $ lookup (staticOptName sod)
                                $ valArgs args

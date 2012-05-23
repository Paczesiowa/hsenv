{-# LANGUAGE Arrows #-}
module Util.Args.Args (parseArgs) where

import Util.Args.ArgDescr (ArgDescr(..), DefaultValue(..), KnownArgs)
import Util.Args.ArgArrow (ArgArrow, runArgArrow, askArgs, getKnownArgs, addKnownArg)
import Util.Args.RawArgs (Args(..), parseArguments)
import Util.WordWrap (wordWrap)
import Util.String (padTo)
import Control.Arrow ((>>>), returnA, arr)
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)
import Data.List(sortBy)
import Data.Maybe (catMaybes)
import Util.Args.Usage (usage)

data ArgParseResult a = Usage
                      | Help
                      | Error String
                      | Version
                      | OK a
    deriving (Show, Eq)

helperArgArrow :: ArgArrow a b -> ArgArrow a (ArgParseResult b)
helperArgArrow arrow = proc x -> do
  addKnownArg knargs -< ()
  args <- askArgs -< ()
  if "help" `elem` switches args then
    returnA -< Help
   else if "usage" `elem` switches args then
    returnA -< Usage
   else if "version" `elem` switches args then
    returnA -< Version
   else
    arrow >>> arr OK -< x
    where knargs = [versionOpt, helpOpt]
          helpOpt = SwitchDescr "help" "Show this help message" (Just 'h')
          versionOpt = SwitchDescr "version" "Show version string" Nothing

failWith :: String -> IO a
failWith s = hPutStrLn stderr s >> exitFailure

validateArguments :: Args -> KnownArgs -> IO ()
validateArguments args knArgs
    | not $ null $ positionals args = failWith "Positional arguments are not allowed"
    | otherwise =
        either failWith return $
               checkUnknownArguments args knShortSwitches knSwitches knKeys
    where knShortSwitches = catMaybes $ flip map knArgs $ \x -> case x of
                               SwitchDescr _ _ c -> c
                               _ -> Nothing
          knSwitches = catMaybes $ flip map knArgs $ \x -> case x of
                               SwitchDescr x _ _ -> Just x
                               _ -> Nothing
          knKeys = catMaybes $ flip map knArgs $ \x -> case x of
                               ValArg x _ _ _ -> Just x
                               _ -> Nothing

parseArgs :: ArgArrow () a -> String -> String -> IO a
parseArgs arrgArr version outro = do
  args <- getArgs
  case parseArguments args of
    Left s -> failWith s
    Right parsedArgs -> do
      validateArguments parsedArgs $ getKnownArgs arrgArr'
      result <- runArgArrow arrgArr' parsedArgs
      case result of
        OK a -> return a
        Error s -> failWith s
        Version -> putStrLn version >> exitSuccess
        _ -> usage arrgArr' outro >>= putStr >> exitSuccess
  where arrgArr' = helperArgArrow arrgArr

checkUnknownArguments :: Args -> [Char] -> [String] -> [String] -> Either String ()
checkUnknownArguments args knownShortSwitches knownSwitches knownKeys = do
  mapM_ (validate "short switch" "-" knownShortSwitches (:"")) $ shortSwitches args
  mapM_ (validate "switch" "--" knownSwitches id) $ switches args
  mapM_ (validate "option" "--" knownKeys id) $ map fst $ valArgs args
    where validate xName prefix knownXs showX x =
              if x `elem` knownXs then
                  Right ()
              else
                  Left $ "Unknown " ++ xName ++ " '" ++ prefix ++ showX x ++ "'"

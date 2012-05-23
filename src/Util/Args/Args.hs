{-# LANGUAGE Arrows #-}
module Util.Args.Args where

import Util.Args.ArgDescr
import Util.Args.ArgArrow
import Util.Args.RawArgs
import Util.StaticArrowT
import Util.WordWrap
import Util.String
import Control.Arrow
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)
import Data.Function (on)
import Data.List(sortBy)
import Data.Maybe

showFlagDescr :: ArgDescr -> [String]
showFlagDescr argDescr = zipWith makeLine lefts msgLines
    where lefts    = argLine : repeat ""
          argLine  = case argDescr of
                       SwitchDescr name _ Nothing -> "--" ++ name
                       SwitchDescr name _ (Just c) ->
                           concat ["-", [c], " ", "--", name]
                       ValArg name tmpl _ _ -> concat ["--", name, "=", tmpl]
          msgLines = wordWrap 60 $ case argDescr of
                                     SwitchDescr _ hlp _ -> hlp
                                     ValArg _ _ default' help ->
                                         concat [help, "\n", defaultsLine default']
          defaultsLine (ConstValue s) = concat ["(defaults to '", s, "')"]
          defaultsLine (DynValue s)   = concat ["(defaults to ", s, ")"]
          makeLine infoLine descrLine = (infoLine `padTo` 20) ++ descrLine

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


usage :: ArgArrow a b -> String -> IO String
usage arrow outro = do
  self <- getProgName
  let intro = "usage: " ++ self ++ " [FLAGS]"
  return $ unlines $ [intro, "", "Flags:"] ++ flagsDescr ++ [""] ++  outro'
      where flagsDescr = concatMap showFlagDescr $ argDescrSort $ getKnownArgs arrow
            argDescrSort = sortBy (compare `on` argName)
            outro' = wordWrap 80 outro

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

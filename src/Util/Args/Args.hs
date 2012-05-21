{-# LANGUAGE Arrows #-}
module Util.Args.Args where

import Util.Args.ArgDescr
import Util.Args.ArgArrow
import Util.Args.RawArgs
import Util.Args.StaticArrow
import Util.WordWrap
import Util.String
import Control.Arrow
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getProgName)
import Data.Function (on)
import Data.List(sortBy)

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
          helpOpt = SwitchDescr "help" "Show this help message"
          versionOpt = SwitchDescr "version" "Show version string"

parseArgs :: ArgArrow () a -> String -> String -> IO a
parseArgs arrgArr version outro = do
  args   <- getArgs
  result <- runArgArrow arrgArr' (parseArguments args)
  case result of
    OK a -> return a
    Error s -> hPutStrLn stderr s >> exitFailure
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

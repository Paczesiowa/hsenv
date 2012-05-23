module Util.Args.RawArgs where

import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Instances ()
import Util.List (breakOn)

{-# ANN Args "HLint: ignore Use String" #-}
data Args = Args { shortSwitches :: [Char]
                 , switches      :: [String]
                 , valArgs       :: [(String, String)]
                 , positionals   :: [String]
                 }
    deriving Show

instance Monoid Args where
    mempty = Args [] [] [] []
    Args xs1 ys1 zs1 us1 `mappend` Args xs2 ys2 zs2 us2 =
        Args (xs1 ++ xs2) (ys1 ++ ys2) (zs1 ++ zs2) (us1 ++ us2)

parseArgument :: String -> Either String Args
parseArgument param@('-':'-':arg) =
    Right $ case breakOn '=' arg of
      Nothing         -> mempty{switches = [arg]}
      Just (key, val) -> mempty{valArgs = [(key, val)]}
parseArgument ['-', c] = Right mempty{shortSwitches = [c]}
parseArgument param@('-':_) = Left $ "Invalid option: '" ++ param ++ "'"
parseArgument arg = Right mempty{positionals = [arg]}

parseArguments :: [String] -> Either String Args
parseArguments args =
    case breakOn "--" args of
      Nothing -> mconcat `liftM` mapM parseArgument args
      Just (args', positionals) -> do
                             parsedArgs <- mapM parseArgument args'
                             return $ mconcat parsedArgs `mappend` mempty{positionals = positionals}

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

module Util.Args.ArgDescr where

data DefaultValue = ConstValue String
                  | DynValue String
    deriving (Show, Eq)

data ArgDescr = SwitchDescr { argName  :: String
                            , helpMsg  :: String
                            , shortOpt :: Maybe Char
                            }
              | ValArg { argName      :: String
                       , valTemplate  :: String
                       , defaultValue :: DefaultValue
                       , helpMsg      :: String
                       }
     deriving (Show, Eq)

type KnownArgs = [ArgDescr]

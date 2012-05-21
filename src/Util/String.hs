module Util.String (padTo) where

padTo :: String -> Int -> String
padTo s n = take n $ s ++ repeat ' '

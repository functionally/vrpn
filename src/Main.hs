module Main (
  main
) where


import Network.VRPN (PositionCallback, positionLoop)
import System.Environment (getArgs)


dump :: PositionCallback
dump x y z = print (x, y, z)


main :: IO ()
main =
  do
    [device] <- getArgs
    positionLoop device dump

{-|
Module      :  Main
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Simple VRPN client.
-}


module Main (
  main
) where


import Network.VRPN (Device(..), closeDevice, mainLoops, openDevice)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)


-- | Action for a simple VRPN client.
main :: IO ()
main =
  do
    names <- getArgs
    let
      out s = putStrLn s >> hFlush stdout
      devices :: [Device Int Int Int Double]
      devices =
        concat
          [
            [
              Tracker name
                (Just $ \t s p o -> out $ show t ++ ", sensor " ++ show s ++ ", position " ++ show p ++ ", orientation " ++ show o)
                (Just $ \t s v o d -> out $ show t ++ ", sensor " ++ show s ++ ", velocity " ++ show v ++ ", orientation " ++ show o ++ ", delta " ++ show d)
                (Just $ \t s a o d -> out $ show t ++ ", sensor " ++ show s ++ ", acceleration " ++ show a ++ ", orientation " ++ show o ++ ", delta " ++ show d)
            , Button name
                (Just $ \t b p -> out $ show t ++ ", button " ++ show b ++ ", pressed " ++ show p)
            , Analog name
                (Just $ \t v -> out $ show t ++ ", analog " ++ show v)
            , Dial name
                (Just $ \t d c -> out $ show t ++ ", dial " ++ show d ++ ", change " ++ show c)
            ]
          |
            name <- names
          ]
    remotes <- mapM openDevice devices
    mainLoops (return False) (10 :: Double) remotes
    mapM_ closeDevice remotes

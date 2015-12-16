{-# LANGUAGE ForeignFunctionInterface #-}


module Main (
  main
) where


import Foreign.C.String (CString, withCString) 
import Foreign.C.Types (CDouble(..))
import Foreign.Ptr (FunPtr)
import System.Environment (getArgs)


type PositionCallback = CDouble -> CDouble -> CDouble -> IO ()

 
foreign import ccall "wrapper"
  wrap :: PositionCallback -> IO (FunPtr PositionCallback)


foreign import ccall "mainLoop" mainLoop :: FunPtr PositionCallback -> CString -> IO ()


dump :: CDouble -> CDouble -> CDouble -> IO () 
dump x y z = print (x, y, z)


main :: IO ()
main =
  do
    [device] <- getArgs
    dump' <- wrap dump
    withCString device
      $ mainLoop dump'

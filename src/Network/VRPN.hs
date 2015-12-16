{-# LANGUAGE ForeignFunctionInterface #-}


module Network.VRPN (
  PositionCallback
, positionLoop
) where


import Foreign.C.String (CString, withCString) 
import Foreign.C.Types (CDouble(..))
import Foreign.Ptr (FunPtr, freeHaskellFunPtr)


type PositionCallback = CDouble -> CDouble -> CDouble -> IO ()

 
foreign import ccall "wrapper"
  wrap :: PositionCallback -> IO (FunPtr PositionCallback)


foreign import ccall "mainLoop"
  mainLoop :: FunPtr PositionCallback -> CString -> IO ()


positionLoop :: String -> PositionCallback -> IO ()
positionLoop device callback =
  do
    callback' <- wrap callback
    withCString device
      $ mainLoop callback'
    freeHaskellFunPtr callback'

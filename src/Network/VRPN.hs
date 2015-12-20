{-|
Module      :  Network.VRPN
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Bindings to VRPN, \<<https://github.com/vrpn/vrpn/wiki>\>, and is loosely modeled on the code in \<<https://github.com/vrpn/vrpn/blob/master/client_src/vrpn_print_devices.C>\>.  This has been tested using VRPN 07.30 on Linux.  It requires the VRPN C++ header files.

Here is a simple example that illustrates the use of this module:

@
data ButtonType = LeftButton | RightButton
  deriving (Enum, Eq, Show)

main :: IO ()
main =
  do
    putStrLn "Press the left button to exit."
    done <- newEmptyMVar
    let
      -- A remote button that signals completion when the left button is released.
      button :: Device Int ButtonType Double
      button =
        Button "spacenav0@localhost"
          $ Just
          $ \time button state ->
            do
              print (time, button, state)
              if button == LeftButton && not state
                then void $ tryPutMVar done ()
                else return ()
      -- An analog device.
      analog :: Device Int Int Int Double
      analog = Analog "spacenav0@localhost"
        $ Just
        $ curry print
    -- Open the remote devices.
    devices <- sequence [openDevice button, openDevice analog]
    -- Loop until a signal to complete is received.
    mainLoops (not <$> isEmptyMVar done) 10 devices
    -- Close the remote devices.
    mapM_ closeDevice devices
@
-}


{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}


module Network.VRPN (
-- * Devices
  Device(..)
, RemoteDevice
-- * Callbacks
, PositionCallback
, VelocityCallback
, AccelerationCallback
, ButtonCallback
, AnalogCallback
, DialCallback
, ExitCallback
-- * Operations on devices
, openDevice
, closeDevice
, withDevices
, mainLoop
, mainLoops
-- * Time
, TimeVal(..)
, sleep
) where


import Control.Monad (unless, when)
import Foreign.C.String (CString, withCString) 
import Foreign.C.Types (CDouble(..), CInt(..), CLong(..))
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr)


-- | A VRPN device.
data Device s b d a =
    -- | A tracker.
    Tracker
    {
      device               :: String                           -- ^ The device name.
    , positionCallback     :: Maybe (PositionCallback s a)     -- ^ The position callback.
    , velocityCallback     :: Maybe (VelocityCallback s a)     -- ^ The velocity callback.
    , accelerationCallback :: Maybe (AccelerationCallback s a) -- ^ The acceleration callback.
    }
    -- | A button.
  | Button
    {
      device               :: String                           -- ^ The device name.
    , buttonCallback       :: Maybe (ButtonCallback b)         -- ^ The button callback.
    }
    -- | An analog device.
  | Analog
    {
      device               :: String                           -- ^ The device name.
    , analogCallback       :: Maybe (AnalogCallback a)         -- ^ The analog callback.
    }
    -- | A dial.
  | Dial
    {
      device               :: String                           -- ^ The device name.
    , dialCallback         :: Maybe (DialCallback d a)         -- ^ The dial callback.
    }


-- | Timestamps in seconds and fractions of a section.
data TimeVal =
  TimeVal
  {
    timeSeconds      :: Int -- ^ The seconds.
  , timeMicroSeconds :: Int -- ^ The microseconds.
  }
    deriving (Eq, Ord, Read, Show)


-- | Callback for position information.
type PositionCallback s a =  TimeVal      -- ^ The timestamp.
                          -> s            -- ^ Which sensor is reporting.
                          -> (a, a, a)    -- ^ The position vector.
                          -> (a, a, a, a) -- ^ The orientation quaternion.
                          -> IO ()        -- ^ The action performed by the callback.


-- | Callback for position information.
type PositionCallback' =  CLong   -- ^ Seconds of the timestamp.
                       -> CLong   -- ^ Microseconds of the timestamp.
                       -> CInt    -- ^ Which sensor is reporting.
                       -> CDouble -- ^ 1st component of the position vector.
                       -> CDouble -- ^ 2nd component of the position vector.
                       -> CDouble -- ^ 3rd component of the position vector.
                       -> CDouble -- ^ 1st component of the orientation quaternion.
                       -> CDouble -- ^ 2nd component of the orientation quaternion.
                       -> CDouble -- ^ 3rd component of the orientation quaternion.
                       -> CDouble -- ^ 4th component of the orientation quaternion.
                       -> IO ()   -- ^ The action performed by the callback.

-- | Wrap a position callback.
foreign import ccall "wrapper"
  wrapPositionCallback :: PositionCallback' -> IO (FunPtr PositionCallback')


-- | Make a position callback suitable for FFI.
makePositionCallback :: (Enum s, RealFloat a)
                     => PositionCallback s a -- ^ The callback.
                     -> PositionCallback'    -- ^ An equivalent FFI callback.
makePositionCallback callback seconds microseconds sensor px py pz ox oy oz ow =
  callback
    (TimeVal (fromEnum seconds) (fromEnum microseconds))
    (toEnum $ fromEnum sensor)
    (realToFrac px, realToFrac py, realToFrac pz)
    (realToFrac ox, realToFrac oy, realToFrac oz, realToFrac ow)


-- | Callback for velocity information.
type VelocityCallback s a =  TimeVal      -- ^ The timestamp.
                          -> s            -- ^ Which sensor is reporting.
                          -> (a, a, a)    -- ^ The velocity vector.
                          -> (a, a, a, a) -- ^ The future orientation quaternion.
                          -> a            -- ^ Delta time for the future orientation quaternion, in seconds.
                          -> IO ()        -- ^ The action performed by the callback.


-- | Callback for velocity information.
type VelocityCallback' =  CLong   -- ^ Seconds of the timestamp.
                       -> CLong   -- ^ Microseconds of the timestamp.
                       -> CInt    -- ^ Which sensor is reporting.
                       -> CDouble -- ^ 1st component of the velocity vector.
                       -> CDouble -- ^ 2nd component of the velocity vector.
                       -> CDouble -- ^ 3rd component of the velocity vector.
                       -> CDouble -- ^ 1st component of the future orientation quaternion.
                       -> CDouble -- ^ 2nd component of the future orientation quaternion.
                       -> CDouble -- ^ 3rd component of the future orientation quaternion.
                       -> CDouble -- ^ 4th component of the future orientation quaternion.
                       -> CDouble -- ^ Delta time for the future orientation quaternion, in seconds.
                       -> IO ()   -- ^ The action performed by the callback.


-- | Wrap a velocity callback.
foreign import ccall "wrapper"
  wrapVelocityCallback :: VelocityCallback' -> IO (FunPtr VelocityCallback')


-- | Make a velocity callback suitable for FFI.
makeVelocityCallback :: (Enum s, RealFloat a)
                     => VelocityCallback s a -- ^ The callback.
                     -> VelocityCallback'    -- ^ An equivalent FFI callback.
makeVelocityCallback callback seconds microseconds sensor vx vy vz ox oy oz ow dt =
  callback
    (TimeVal (fromEnum seconds) (fromEnum microseconds))
    (toEnum $ fromEnum sensor)
    (realToFrac vx, realToFrac vy, realToFrac vz)
    (realToFrac ox, realToFrac oy, realToFrac oz, realToFrac ow)
    (realToFrac dt)


-- | Callback for acceleration information.
type AccelerationCallback s a =  TimeVal      -- ^ The timestamp.
                              -> s            -- ^ Which sensor is reporting.
                              -> (a, a, a)    -- ^ The acceleration vector.
                              -> (a, a, a, a) -- ^ The acceleration orientation quaternion.
                              -> a            -- ^ Delta time for the acceleration quaternion, in seconds.
                              -> IO ()        -- ^ The action performed by the callback.


-- | Callback for acceleration information.
type AccelerationCallback' =  CLong   -- ^ Seconds of the timestamp.
                           -> CLong   -- ^ Microseconds of the timestamp.
                           -> CInt    -- ^ Which sensor is reporting.
                           -> CDouble -- ^ 1st component of the acceleration vector.
                           -> CDouble -- ^ 2nd component of the acceleration vector.
                           -> CDouble -- ^ 3rd component of the acceleration vector.
                           -> CDouble -- ^ 1st component of the acceleration quaternion.
                           -> CDouble -- ^ 2nd component of the acceleration quaternion.
                           -> CDouble -- ^ 3rd component of the acceleration quaternion.
                           -> CDouble -- ^ 4th component of the acceleration quaternion.
                           -> CDouble -- ^ Delta time for the acceleration quaternion, in seconds.
                           -> IO ()   -- ^ The action performed by the callback.


-- | Wrap an acceleration callback.
foreign import ccall "wrapper"
  wrapAccelerationCallback :: AccelerationCallback' -> IO (FunPtr AccelerationCallback')


-- | Make an acceleration callback suitable for FFI.
makeAccelerationCallback :: (Enum s, RealFloat a)
                         => AccelerationCallback s a -- ^ The callback.
                         -> AccelerationCallback'    -- ^ An equivalent FFI callback.
makeAccelerationCallback callback seconds microseconds sensor ax ay az ox oy oz ow dt =
  callback
    (TimeVal (fromEnum seconds) (fromEnum microseconds))
    (toEnum $ fromEnum sensor)
    (realToFrac ax, realToFrac ay, realToFrac az)
    (realToFrac ox, realToFrac oy, realToFrac oz, realToFrac ow)
    (realToFrac dt)


-- | Callback for button information.
type ButtonCallback b =  TimeVal -- ^ The timestamp.
                      -> b       -- ^ Which button was pressed, counting from 0.
                      -> Bool    -- ^ Whether the button is pressed.
                      -> IO ()   -- ^ The action performed by the callback.


-- | Callback for button information.
type ButtonCallback' =  CLong   -- ^ Seconds of the timestamp.
                     -> CLong   -- ^ Microseconds of the timestamp.
                     -> CInt    -- ^ Which button was pressed, counting from 0.
                     -> CInt    -- ^ The button state (0 = off, 1 = on).
                     -> IO ()   -- ^ The action performed by the callback.


-- | Wrap a button callback.
foreign import ccall "wrapper"
  wrapButtonCallback :: ButtonCallback' -> IO (FunPtr ButtonCallback')


-- | Make a button callback suitable for FFI.
makeButtonCallback :: Enum b
                   => ButtonCallback b -- ^ The callback.
                   -> ButtonCallback'  -- ^ An equivalent FFI callback.
makeButtonCallback callback seconds microseconds button state =
  callback
    (TimeVal (fromEnum seconds) (fromEnum microseconds))
    (toEnum $ fromEnum button)
    (state /= 0)


-- | Callback for analog information.
type AnalogCallback a =  TimeVal -- ^ The timestamp.
                      -> [a]     -- ^ The analog values.
                      -> IO ()   -- ^ The action performed by the callback.


-- | Callback for analog information.
type AnalogCallback' =  CLong      -- ^ Seconds of the timestamp.
                     -> CLong      -- ^ Microseconds of the timestamp.
                     -> CInt        -- ^ The number of values.
                     -> Ptr CDouble -- ^ The analog values.
                     -> IO ()       -- ^ The action performed by the callback.


-- | Wrap an analog callback.
foreign import ccall "wrapper"
  wrapAnalogCallback :: AnalogCallback' -> IO (FunPtr AnalogCallback')


-- | Make an analog callback suitable for FFI.
makeAnalogCallback :: RealFloat a
                   => AnalogCallback a -- ^ The callback.
                   -> AnalogCallback'  -- ^ An equivalent FFI callback.
makeAnalogCallback callback seconds microseconds n ptr =
  do
    values <- peekArray (fromEnum n) ptr
    callback
      (TimeVal (fromEnum seconds) (fromEnum microseconds))
      (map realToFrac values)


-- | Callback for dial information.
type DialCallback d a =  TimeVal -- ^ The timestamp.
                      -> d       -- ^ Which dial changed.
                      -> a       -- ^ The fraction of a revolution it changed.
                      -> IO ()   -- ^ The action performed by the callback.


-- | Callback for dial information.
type DialCallback' =  CLong   -- ^ Seconds of the timestamp.
                   -> CLong   -- ^ Microseconds of the timestamp.
                   -> CInt    -- ^ Which dial changed.
                   -> CDouble -- ^ The fraction of a revolution it changed.
                   -> IO ()   -- ^ The action performed by the callback.


-- | Wrap a dial callback.
foreign import ccall "wrapper"
  wrapDialCallback :: DialCallback' -> IO (FunPtr DialCallback')


-- | Make an analog callback suitable for FFI.
makeDialCallback :: (Enum d, RealFloat a)
                 => DialCallback d a -- ^ The callback.
                 -> DialCallback'    -- ^ An equivalent FFI callback.
makeDialCallback callback seconds microseconds dial value =
  callback
    (TimeVal (fromEnum seconds) (fromEnum microseconds))
    (toEnum $ fromEnum dial)
    (realToFrac value)


-- | Callback for exiting the main loop.
type ExitCallback = IO Bool -- ^ An action indicate whether to exit the main loop.


-- | A remote object.
data Remote


-- | Construct a remote tracker.
foreign import ccall "makeTracker"
  makeTracker :: CString -> FunPtr PositionCallback' -> FunPtr VelocityCallback' -> FunPtr AccelerationCallback' -> IO (Ptr Remote)


-- | Run the main loop of a remote tracker.
foreign import ccall "mainloopTracker"
  mainloopTracker :: Ptr Remote -> IO ()


-- | Destroy a remote tracker.
foreign import ccall "deleteTracker"
  deleteTracker :: Ptr Remote -> IO ()


-- | Construct a remote button.
foreign import ccall "makeButton"
  makeButton :: CString -> FunPtr ButtonCallback' -> IO (Ptr Remote)


-- | Run the main loop of a remote button.
foreign import ccall "mainloopButton"
  mainloopButton :: Ptr Remote -> IO ()


-- | Destory a remote button.
foreign import ccall "deleteButton"
  deleteButton :: Ptr Remote -> IO ()


-- | Construct a remote analog.
foreign import ccall "makeAnalog"
  makeAnalog :: CString -> FunPtr AnalogCallback' -> IO (Ptr Remote)


-- | Run the main loop of a remote analog.
foreign import ccall "mainloopAnalog"
  mainloopAnalog :: Ptr Remote -> IO ()


-- | Destroy a remote analog.
foreign import ccall "deleteAnalog"
  deleteAnalog :: Ptr Remote -> IO ()


-- | Construction a remote dial.
foreign import ccall "makeDial"
  makeDial :: CString -> FunPtr DialCallback' -> IO (Ptr Remote)


-- | Run the main loop of a remote dial.
foreign import ccall "mainloopDial"
  mainloopDial :: Ptr Remote -> IO ()


-- | Destory a remote dial.
foreign import ccall "deleteDial"
  deleteDial :: Ptr Remote -> IO ()


-- | Sleep for the specified milliseconds.
foreign import ccall "vrpnSleep"
  vrpnSleep :: CDouble -> IO ()


-- | Sleep for the specified amount of time.
sleep :: RealFloat a => a     -- ^ The number of milliseconds.
                     -> IO () -- ^ An action to sleep the specified amount of time
sleep = vrpnSleep . realToFrac


-- | A remote VRPN device.
newtype RemoteDevice = RemoteDevice (ForeignPtr Remote, ForeignPtr Remote -> IO ())


-- | Open a remote VRPN device.
openDevice :: (Enum s, Enum b, Enum d, RealFloat a)
           => Device s b d a                -- ^ The device.
           -> IO RemoteDevice               -- ^ An action for opening the device.
openDevice Tracker{..} =
  do
    positionCallback'     <- maybe (return nullFunPtr) (wrapPositionCallback     . makePositionCallback    ) positionCallback
    velocityCallback'     <- maybe (return nullFunPtr) (wrapVelocityCallback     . makeVelocityCallback    ) velocityCallback
    accelerationCallback' <- maybe (return nullFunPtr) (wrapAccelerationCallback . makeAccelerationCallback) accelerationCallback
    ptr <-
      withCString device $ \device' ->
        makeTracker device' positionCallback' velocityCallback' accelerationCallback'
    ptr' <-
      newForeignPtr ptr $ do
        deleteTracker ptr
        freeHaskellFunPtr positionCallback'
        freeHaskellFunPtr velocityCallback'
        freeHaskellFunPtr accelerationCallback'
    return $ RemoteDevice (ptr', flip withForeignPtr mainloopTracker)
openDevice Button{..} =
  do
    buttonCallback' <- maybe (return nullFunPtr) (wrapButtonCallback . makeButtonCallback) buttonCallback
    ptr <-
      withCString device $ \device' ->
        makeButton device' buttonCallback'
    ptr' <-
      newForeignPtr ptr $ do
        deleteButton ptr
        freeHaskellFunPtr buttonCallback'
    return $ RemoteDevice (ptr', flip withForeignPtr mainloopButton)
openDevice Analog{..} =
  do
    analogCallback' <- maybe (return nullFunPtr) (wrapAnalogCallback . makeAnalogCallback) analogCallback
    ptr <-
      withCString device $ \device' ->
        makeAnalog device' analogCallback'
    ptr' <-
      newForeignPtr ptr $ do
        deleteAnalog ptr
        freeHaskellFunPtr analogCallback'
    return $ RemoteDevice (ptr', flip withForeignPtr mainloopAnalog)
openDevice Dial{..} =
  do
    dialCallback' <- maybe (return nullFunPtr) (wrapDialCallback . makeDialCallback) dialCallback
    ptr <-
      withCString device $ \device' ->
        makeDial device' dialCallback'
    ptr' <-
      newForeignPtr ptr $ do
        deleteDial ptr
        freeHaskellFunPtr dialCallback'
    return $ RemoteDevice (ptr', flip withForeignPtr mainloopDial)


-- | Close a remote device.
closeDevice :: RemoteDevice -- ^ The device.
            -> IO ()        -- ^ An action for closing the device.
closeDevice (RemoteDevice (device, _)) = finalizeForeignPtr device


-- | Operate on devices.
withDevices :: (Enum s, Enum b, Enum d, RealFloat a)
            => [Device s b d a]          -- ^ The devices.
            -> ([RemoteDevice] -> IO ()) -- ^ The operation.
            -> IO ()                     -- ^ The action for operating on the devices.
withDevices devices operation =
  do
    remotes <- mapM openDevice devices
    operation remotes
    mapM_ closeDevice remotes


-- | Run the main loop of a device *once*.
mainLoop :: RemoteDevice -- ^ The device.
         -> IO ()        -- ^ An action for running the main loop of the device *once*.
mainLoop (RemoteDevice (device, mainloopDevice)) = mainloopDevice device


-- | Run the main loops of devices *repeatedly*.
mainLoops :: RealFloat a
          => ExitCallback   -- ^ Callback for exiting the loop.
          -> a              -- ^ The number of milliseconds to idle after each device's main loop is run once.
          -> [RemoteDevice] -- ^ The devices.
          -> IO ()          -- ^ An action for running the main loops *repeatedly*.
mainLoops exitCallback milliseconds devices =
  do
    mapM_ mainLoop devices
    when (milliseconds > 0)
      $ sleep milliseconds
    exit <- exitCallback
    unless exit
      $ mainLoops exitCallback milliseconds devices

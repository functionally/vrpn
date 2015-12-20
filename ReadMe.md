Bindings for VRPN
=================

This package contains bindings to VRPN, <<https://github.com/vrpn/vrpn/wiki>> and is loosely modeled on the code in <<https://github.com/vrpn/vrpn/blob/master/client_src/vrpn_print_devices.C>>.  This has been tested using VRPN 07.30 on Linux.  It requires the VRPN C++ header files.

Please report issues at <<https://bwbush.atlassian.net/projects/HVRPN/issues/>>.


Skeletal example illustrating the use of VRPN bindings
------------------------------------------------------

```haskell
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
```

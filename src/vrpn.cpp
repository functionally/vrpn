#include <stdio.h>
#include <stdlib.h>
#include <vrpn_Analog.h>
#include <vrpn_Tracker.h>


typedef void (PositionCallback)(double, double, double);


void VRPN_CALLBACK handle_tracker_pos_quat (void *callback, const vrpn_TRACKERCB tracker)
{
  ((PositionCallback*) callback)(tracker.pos[0], tracker.pos[1], tracker.pos[2]);
}


void VRPN_CALLBACK handle_analog (void *callback, const vrpn_ANALOGCB analog)
{
  ((PositionCallback*) callback)(analog.channel[0], analog.channel[1], analog.channel[2]);
}


extern "C" void mainLoop(PositionCallback callback, char* device)
{
  vrpn_Tracker_Remote tracker(device);
  tracker.register_change_handler((void*) callback, handle_tracker_pos_quat);

//vrpn_Analog_Remote analog(device);
//analog.register_change_handler((void*) callback, handle_analog);

  while (1) {
    tracker.mainloop();
//  analog.mainloop();
    vrpn_SleepMsecs(1);
  }
}

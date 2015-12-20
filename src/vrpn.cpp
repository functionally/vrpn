#include <stdio.h>
#include <stdlib.h>
#include <vrpn_Analog.h>
#include <vrpn_Dial.h>
#include <vrpn_Button.h>
#include <vrpn_Tracker.h>


typedef void (PositionCallback)(long, long, int, double, double, double, double, double, double, double);


void VRPN_CALLBACK handle_tracker_pos_quat(void *callback, const vrpn_TRACKERCB tracker)
{
  ((PositionCallback*) callback)(
    tracker.msg_time.tv_sec
  , tracker.msg_time.tv_usec
  , tracker.sensor
  , tracker.pos[0]
  , tracker.pos[1]
  , tracker.pos[2]
  , tracker.quat[0]
  , tracker.quat[1]
  , tracker.quat[2]
  , tracker.quat[3]
  );
}


typedef void (VelocityCallback)(long, long, int, double, double, double, double, double, double, double, double);


void VRPN_CALLBACK handle_tracker_vel(void *callback, const vrpn_TRACKERVELCB tracker)
{
  ((VelocityCallback*) callback)(
    tracker.msg_time.tv_sec
  , tracker.msg_time.tv_usec
  , tracker.sensor
  , tracker.vel[0]
  , tracker.vel[1]
  , tracker.vel[2]
  , tracker.vel_quat[0]
  , tracker.vel_quat[1]
  , tracker.vel_quat[2]
  , tracker.vel_quat[3]
  , tracker.vel_quat_dt
  );
}


typedef void (AccelerationCallback)(long, long, int, double, double, double, double, double, double, double, double);


void VRPN_CALLBACK handle_tracker_acc(void *callback, const vrpn_TRACKERACCCB tracker)
{
  ((AccelerationCallback*) callback)(
    tracker.msg_time.tv_sec
  , tracker.msg_time.tv_usec
  , tracker.sensor
  , tracker.acc[0]
  , tracker.acc[1]
  , tracker.acc[2]
  , tracker.acc_quat[0]
  , tracker.acc_quat[1]
  , tracker.acc_quat[2]
  , tracker.acc_quat[3]
  , tracker.acc_quat_dt
  );
}


typedef void (ButtonCallback)(long, long, int, int);


void VRPN_CALLBACK handle_button(void *callback, const vrpn_BUTTONCB button)
{
  ((ButtonCallback*) callback)(
    button.msg_time.tv_sec
  , button.msg_time.tv_usec
  , button.button
  , button.state
  );
}


typedef void (AnalogCallback)(long, long, int, const double*);


void VRPN_CALLBACK handle_analog (void *callback, const vrpn_ANALOGCB analog)
{
  ((AnalogCallback*) callback)(
    analog.msg_time.tv_sec
  , analog.msg_time.tv_usec
  , analog.num_channel
  , analog.channel
  );
}


typedef void (DialCallback)(long, long, int, double);


void VRPN_CALLBACK handle_dial(void *callback, const vrpn_DIALCB dial)
{
  ((DialCallback*) callback)(
    dial.msg_time.tv_sec
  , dial.msg_time.tv_usec
  , dial.dial
  , dial.change
  );
}


extern "C" vrpn_Tracker_Remote* makeTracker(char* device, PositionCallback positionCallback, VelocityCallback velocityCallback, AccelerationCallback accelerationCallback)
{
  vrpn_Tracker_Remote* tracker = new vrpn_Tracker_Remote(device);
  if (tracker && positionCallback)
    tracker->register_change_handler((void*) positionCallback, handle_tracker_pos_quat);
  if (tracker && velocityCallback)
    tracker->register_change_handler((void*) velocityCallback, handle_tracker_vel);
  if (tracker && accelerationCallback)
    tracker->register_change_handler((void*) accelerationCallback, handle_tracker_acc);
  return tracker;
}


extern "C" void mainloopTracker(vrpn_Tracker_Remote* tracker)
{
  if (tracker)
    tracker->mainloop();
}


extern "C" void deleteTracker(vrpn_Tracker_Remote* tracker)
{
  delete tracker;
}


extern "C" vrpn_Button_Remote* makeButton(char* device, ButtonCallback buttonCallback)
{
  vrpn_Button_Remote* button = new vrpn_Button_Remote(device);
  if (button && buttonCallback)
    button->register_change_handler((void*) buttonCallback, handle_button);
  return button;
}


extern "C" void mainloopButton(vrpn_Button_Remote* button)
{
  if (button)
    button->mainloop();
}


extern "C" void deleteButton(vrpn_Button_Remote* button)
{
  delete button;
}


extern "C" vrpn_Analog_Remote* makeAnalog(char* device, AnalogCallback analogCallback)
{
  vrpn_Analog_Remote* analog = new vrpn_Analog_Remote(device);
  if (analog && analogCallback)
    analog->register_change_handler((void*) analogCallback, handle_analog);
  return analog;
} 


extern "C" void mainloopAnalog(vrpn_Analog_Remote* analog)
{
  if (analog)
    analog->mainloop();
}


extern "C" void deleteAnalog(vrpn_Analog_Remote* analog)
{
  delete analog;
}


extern "C" vrpn_Dial_Remote* makeDial(char* device, DialCallback dialCallback)
{
  vrpn_Dial_Remote* dial = new vrpn_Dial_Remote(device);
  if (dial && dialCallback)
    dial->register_change_handler((void*) dialCallback, handle_dial);
  return dial;
}


extern "C" void mainloopDial(vrpn_Dial_Remote* dial)
{
  if (dial)
    dial->mainloop();
}


extern "C" void deleteDial(vrpn_Dial_Remote* dial)
{
  delete dial;
}


extern "C" void vrpnSleep(long t)
{
  vrpn_SleepMsecs(t);
}

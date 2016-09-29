//######################################################
//### ARDUINO CODE - Driver For Eyes and Lights 
//### Experimental Code.  
//######################################################

#include <Servo.h> 
#include <Keyboard.h>

Servo eye1;  // create servo object to control a servo 
 
int minPulse     =  600;  // minimum servo position  
int maxPulse     =  2400; // maximum servo position  
int turnRate     =  100;  // servo turn rate increment (larger value, faster rate)  
int refreshTime  =  20;   // time (ms) between pulses (50Hz)  
 
/** The Arduino will calculate these values for you **/  
int centerServo;         // center servo position  
int pulseWidth;          // servo pulse width  
int moveServo;           // raw user input  
long lastPulse   = 0;    // recorded time (ms) of the last pulse  
 
 
void setup() {  
 centerServo = maxPulse - ((maxPulse - minPulse)/2);  
 pulseWidth = centerServo;   // Give the servo a starting point (or it floats)  
 Serial.begin(9600);  
 Serial.println("      Arduino Serial Servo Control");  
 Serial.println("Press < or > to move, spacebar to center");  
 Serial.println();  

// SERVO
eye1.attach(8);  // attaches the servo on pin 8 to the servo object 
pinMode(8, OUTPUT);  // on/off for SERVO

// LEDs
pinMode(2, OUTPUT);  // on/off for LED - 1 on
pinMode(3, OUTPUT);  // on/off for LED - 1 on
pinMode(4, OUTPUT);  // on/off for LED - 1 on
pinMode(5, OUTPUT);  // on/off for LED - 1 on
pinMode(6, OUTPUT);  // on/off for LED - 1 on

digitalWrite(2, LOW); // active high is not closed
digitalWrite(3, LOW); // active high is not closed
digitalWrite(4, LOW); // active high is not closed
digitalWrite(5, LOW); // active high is not closed
digitalWrite(6, LOW); // active high is not closed

}  

void loop() {  
 // wait for serial input  
 if (Serial.available() > 0) {  
   // read the incoming byte:  
   moveServo = Serial.read();  
   
   if (moveServo == 49) // '1' is for left look
   { eye1.write(160); } 

   if (moveServo == 50) // '2' is look ahead
   { eye1.write(90); } 
   
   if (moveServo == 51) // '3' is for right look
   { eye1.write(20); } 

   if (moveServo == 114)  {  digitalWrite(2, HIGH); }  // r RED
   if (moveServo == 103)  {  digitalWrite(3, HIGH); }  // g GREEN
   if (moveServo == 98)  {  digitalWrite(4, HIGH); }  // b BLUE
   if (moveServo == 119)  {  digitalWrite(5, HIGH); }  // w WHITE
   if (moveServo == 121)  {  digitalWrite(6, HIGH); }  // y YELLOW 

   if (moveServo == 32) // SPACE CLEARS EVERYTHING
   { 
    digitalWrite(2, LOW); // all leds off
    digitalWrite(3, LOW);
    digitalWrite(4, LOW);
    digitalWrite(5, LOW);
    digitalWrite(6, LOW);
    eye1.write(90);  // center eyes
   }  

 }  
 
}  

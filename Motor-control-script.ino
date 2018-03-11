/* Description:
 *  Author: Luis Bezares Calderon, based on the switch code by David A. Mellis (21 November 2006)
 *  switch
 * Purpose: Controlling the vibration of a shaftless motor according using an on delay value and indirectly causing a filament to vibrate. 
 * Circuit configuration: A motor circuit of the type describe in tis link: http://learningaboutelectronics.com/Articles/Vibration-motor-circuit.php
 * Other requirements: A thin filament glued to the motor.
 * This script was used to stimulate trunk tethered larva as described in Bezares-Calderon et al,2018.
 * 
 */

int inPin = 2;         // the number of the input pin
int outPin = 13;       // the number of the output pin
const int motorPin = 5; 
int LOOP = 0;
int myDelayVals[1]{200};   //The most important variable to specify the magnitude of the stimulation. More than one stimulation delays can be added to the array. 
//
int state = LOW;      // the current state of the output pin
int reading;           // the current reading from the input pin
int previous = LOW;    // the previous reading from the input pin
int Size;
// DM: the following variables are long's because the time, measured in miliseconds,
// will quickly become a bigger number than can be stored in an int.
long time = 0;         // DM:the last time the output pin was toggled
long debounce = 200;   // DM:the debounce time, increase if the output flickers

void setup()
{
  pinMode(inPin, INPUT);
  pinMode(outPin, OUTPUT);
 Serial.begin(9600);
  pinMode(motorPin, OUTPUT);
}

void loop()
{
  reading = digitalRead(inPin);
  // DM:if the input just went from LOW and HIGH and we've waited long enough
  // to ignore any noise on the circuit, toggle the output pin and remember
  // the time
  if (reading == HIGH && previous == LOW && millis() - time > debounce) {
    if (state == HIGH){
      state = LOW;
      LOOP=0;
  } else
      state = HIGH;

    time = millis();    
  }

  digitalWrite(outPin, state);
  if(state == HIGH && LOOP == 0){
      Size=(sizeof(myDelayVals)/sizeof(int)); // this variable store the number of delay values to be used.
      delay(4000);  //An initial delay before starting the stimulation
     for(int i=0; i<Size; i++ ){
       analogWrite(motorPin,255);  //0-255 is the dynamic range of the motor. The highest value is used (255).
       delay(myDelayVals[i]);  //Leave the motor on at the defined value for the time specified in the myDelayVals vector.
       Serial.print("size= ");  
       Serial.print(Size);
       Serial.print("\t vals= ");
       Serial.print(myDelayVals[i]);
       Serial.print("\t counter= ");
       Serial.println(i); 
       analogWrite(motorPin,0); //Switch off the motor
       delay(6000); //Rest time before the next stimulation value.
     }
     delay(1000);
     LOOP=1;   // Flag variable needed to stop the program.
   }
  

  previous = reading;
}

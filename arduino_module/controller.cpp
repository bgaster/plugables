/*
 * Author: Benedict R. Gaster
 * Desc:
 */

#include <Arduino.h>

#include <PString.h>

/// Variables will change:
int ledState = HIGH;         // the current state of the output pin
int buttonState;             // the current reading from the input pin
int lastButtonState = LOW;   // the previous reading from the input pin

// the following variables are long's because the time, measured in miliseconds,
// will quickly become a bigger number than can be stored in an int.
unsigned long lastDebounceTime = 0;  // the last time the output pin was toggled
unsigned long debounceDelay = 50;    // the debounce time; increase if the output flickers

#define BUFFER_SIZE 30
char buffer[BUFFER_SIZE];
PString message(buffer, BUFFER_SIZE);

char buffer_serial[BUFFER_SIZE];
PString serial_message(buffer_serial, BUFFER_SIZE);

void setup() {                
	Serial.begin(57600);

	while (!Serial) {
	    ; // wait for serial port to connect. Needed for Leonardo only
	}

	// initialize the digital pin as an output.
	// Pin 13 has an LED connected on most Arduino boards:
	pinMode(6, OUTPUT);     
	pinMode(7, INPUT);

	Serial.write(65);

	int inByte = 0;
	while (inByte  != 66) {
	    
	    //while ( !(Serial.available() > 0) ) {
	    //}
	
	    inByte = Serial.read();
	    //Serial.write(65);
	}

	digitalWrite(6, ledState);
}


void loop() {
    bool sendMsg = true;

    int reading = digitalRead(7);

    int pot1 = 0;
    int pot2 = 0;
    

    // If the switch changed, due to noise or pressing:
    if (reading != lastButtonState) {
	// reset the debouncing timer
	lastDebounceTime = millis();
    }

    if ((millis() - lastDebounceTime) > debounceDelay) {
	// whatever the reading is at, it's been there for longer
	// than the debounce delay, so take it as the actual current state:

	// if the button state has changed:
	if (reading != buttonState) {
	    buttonState = reading;

	    // only toggle the LED if the new button state is HIGH
	    if (buttonState == HIGH) {
		ledState = !ledState;
	    }
	}
    }

    // set the LED:
    digitalWrite(6, ledState);
    
    // save the reading.  Next time through the loop,
    // it'll be the lastButtonState:
    lastButtonState = reading;

    pot1 = analogRead(0);
//    pot2 = analogRead(1);

    serial_message = "POT 1: ";
    serial_message.format("%d", pot1);

//    serial_message = "POT 2: ";
//    serial_message.format("%d", pot2);


//    if (sendMsg) {

    
    Serial.print(serial_message);
    
    Serial.write(10);

//    sendMsg = false;
//    } 
}

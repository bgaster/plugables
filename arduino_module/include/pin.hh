/*
 * Name: pin.hh
 * Author: Benedict R. Gaster
 * Desc:
 *
 * The code is designed to be used with statically known controllers,
 * in particular, most (if not all) parameters are set via template
 * parameters. The reason for this is that the code is designed for
 * very small micro-controllers and thus we want to avoid any
 * unecesarry allocations and avoid indrection when possible.
 *
 * This makes the code a pain to write by hand, but this is not the
 * attention, it is designed to be used by automatically generated
 * code, from the blocks instrument designer.
 *
 * This is not how one might right the same code when dynamic
 * behaviour is necessary or space/performance is less of a conern.
 *
 * FIXME: Added pound define to check we are compiling for Nano.
 *        no real reason that we limit ourselves to Nano, just at 
 *        the moment the design is based around that, shold not be
 *        a big change.
 */
#pragma once

#include <Arduino.h>

#include <stdint.h>

#include <static_if.hh>

// type for Arduino pins
typedef uint8_t pin_id;

/**
 * @description Mapping for digital pins on Nano
 * For some reason Arduino.h does not define these.
 * 
 * Note: For Nano Arduino defines A0 = 13, ..., so 
 * but this is increased on more pinned devices, e.g. mega.
 * In this case we just need to add more digial pins, but
 * of course, we would then need to add check for who we are building 
 * for.
 */
enum PIN {
    D0 = 0,
    D1 = 1,
    D2 = 2,
    D3 = 3,
    D4 = 4,
    D5 = 5,
    D6 = 6,
    D7 = 7,
    D8 = 8,
    D9 = 9,
    D10 = 10,
    D11 = 11,
    D12 = 12,
    D13 = 13,
    D14 = 14, // ??

    // FIXME: if we ever have a large number of pins :-)
    PIN_UNDEFINED = 127
};

//------------------------------------------------------------------------------

namespace blocks {

//FIXME: make read templated on return type
//       this will allow return uint8_t or uint16_t
//       i suppose we could have two read functions?

template<pin_id Id, bool WithPullUp>
struct modify_policy_digital_read
{
    void setup() const
    {
	// call Arduino library to set pin for input
	// handle pullup and non-pullup cases
	static_if<WithPullUp>([&](auto f) {
		pinMode(Id, INPUT_PULLUP); 
	}).else_([&](auto f){
		pinMode(Id, INPUT); 
	});
    }
    
    uint16_t read(void) const
    {
	// perform digital read
	return digitalRead(Id);
    }    
};

template<pin_id Id>
struct modify_policy_digital_write
{
    void setup() const
    {
	// call Arduino library to set pin for output
	pinMode(Id, OUTPUT); 
    }
    
    void write(uint8_t value) const
    {
	// peform digital write
	digitalWrite(Id, value);
    }    
};

template<pin_id Id>
struct modify_policy_analog_read
{
    void setup() const
    {
	// nothing to do for analog pin on Arduino
    }
    
    uint16_t read(void) const
    {	
	// perform analog read
	return analogRead(Id);
    }    
};

//template<pin_id Id>
struct modify_policy_undefined {
    // used as default for invalid, i.e. type error, cases
};    

//------------------------------------------------------------------------------

template <pin_id Id, typename pin_policy>
struct pin : public pin_policy
{
    constexpr pin() 
    {
    }
    
    //typedef pin_policy pin_policy_;
};

template <pin_id Id>
using analog_pin_ro = pin<Id, modify_policy_analog_read<Id>>;

template <pin_id Id, bool WithPullUp>
using digital_pin_ro = pin<Id, modify_policy_digital_read<Id, WithPullUp>>;

template <pin_id Id>
using digital_pin_wo = pin<Id, modify_policy_digital_write<Id>>;

//------------------------------------------------------------------------------

// define visible typedefs

typedef analog_pin_ro<A0> a0_ro;
typedef analog_pin_ro<A1> a1_ro;
typedef analog_pin_ro<A2> a2_ro;
typedef analog_pin_ro<A3> a3_ro;
typedef analog_pin_ro<A4> a4_ro;
typedef analog_pin_ro<A5> a5_ro;
typedef analog_pin_ro<A6> a6_ro;
typedef analog_pin_ro<A7> a7_ro;

typedef digital_pin_ro<D0, false> d0_ro;
typedef digital_pin_ro<D1, false> d1_ro;
typedef digital_pin_ro<D2, false> d2_ro;
typedef digital_pin_ro<D3, false> d3_ro;
typedef digital_pin_ro<D4, false> d4_ro;
typedef digital_pin_ro<D5, false> d5_ro;
typedef digital_pin_ro<D6, false> d6_ro;
typedef digital_pin_ro<D7, false> d7_ro;
typedef digital_pin_ro<D8, false> d8_ro;

typedef digital_pin_ro<D0, true> d0_ropu;
typedef digital_pin_ro<D1, true> d1_ropu;
typedef digital_pin_ro<D2, true> d2_ropu;
typedef digital_pin_ro<D3, true> d3_ropu;
typedef digital_pin_ro<D4, true> d4_ropu;
typedef digital_pin_ro<D5, true> d5_ropu;
typedef digital_pin_ro<D6, true> d6_ropu;
typedef digital_pin_ro<D7, true> d7_ropu;
typedef digital_pin_ro<D8, true> d8_ropu;

typedef digital_pin_wo<D0> d0_wo;
typedef digital_pin_wo<D1> d1_wo;
typedef digital_pin_wo<D2> d2_wo;
typedef digital_pin_wo<D3> d3_wo;
typedef digital_pin_wo<D4> d4_wo;
typedef digital_pin_wo<D5> d5_wo;
typedef digital_pin_wo<D6> d6_wo;
typedef digital_pin_wo<D7> d7_wo;
typedef digital_pin_wo<D8> d8_wo;


}; // namespace blocks


    
    

    

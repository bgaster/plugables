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
 */
#pragma once

#include <stdint.h>
#include <helper.hh>

// type for Arduino pins
typedef int8_t pin_id;

// This is so we can debug code without Arduino library
#if defined(__FAKE_IT__)
enum PIN {
    A0 = 0,
    A1 = 1,
    A2 = 2,
    A3 = 3,
    A4 = 4,
    A5 = 5,
    A6 = 6,
    A7 = 7,

    D0 = 8,
    D1 = 9,
    D2 = 10,
    D3 = 11,
    D4 = 12,
    D5 = 13,
    D6 = 14,
    D7 = 15,

    PIN_UNDEFINED
};
#endif // __FAKE_IT__

//------------------------------------------------------------------------------

namespace blocks {

template<pin_id Id>
struct modify_policy_digital_read
{
    void setup() const
    {
	// add pin setup code here
    }
    
    uint8_t read(void)
    {
	// return digital_read()
	return 0;
    }    
};

template<pin_id Id>
struct modify_policy_digital_write
{
    void setup() const
    {
	// add pin setup code here	
    }
    
    void write(uint8_t value)
    {
	// digital_write()
    }    
};

template<pin_id Id>
struct modify_policy_analog_read
{
    void setup() const
    {
	// add pin setup code here
    }
    
    uint8_t read(void)
    {
	// return analog_read()
	return 0;
    }    
};

template<pin_id Id>
struct modify_policy_undefined {
    // used as default for invalid, i.e. type error, cases
};    

//------------------------------------------------------------------------------

template <pin_id Id, template<pin_id Id_> typename pin_policy>
struct pin : public pin_policy<Id>
{
    constexpr pin() 
    {
    }
    
    //typedef pin_policy pin_policy_;
};

template <pin_id Id>
using analog_pin_ro = pin<Id, modify_policy_analog_read>;

template <pin_id Id>
using digital_pin_ro = pin<Id, modify_policy_digital_read>;

template <pin_id Id>
using digital_pin_wo = pin<Id, modify_policy_digital_write>;

//------------------------------------------------------------------------------

// define visible typedefs

typedef analog_pin_ro<A0> a0_ro;
typedef analog_pin_ro<A1> a1_ro;
typedef analog_pin_ro<A2> a2_ro;
typedef analog_pin_ro<A3> a3_ro;
typedef analog_pin_ro<A4> a4_ro;
typedef analog_pin_ro<A5> a5_ro;
typedef analog_pin_ro<A6> a6_ro;
typedef analog_pin_ro<A7> a7_r0;

typedef digital_pin_ro<D0> d0_ro;
typedef digital_pin_ro<D1> d1_ro;
typedef digital_pin_ro<D2> d2_ro;
typedef digital_pin_ro<D3> d3_ro;
typedef digital_pin_ro<D4> d4_ro;
typedef digital_pin_ro<D5> d5_ro;
typedef digital_pin_ro<D6> d6_ro;
typedef digital_pin_ro<D7> d7_ro;

typedef digital_pin_wo<D0> d0_wo;
typedef digital_pin_wo<D1> d1_wo;
typedef digital_pin_wo<D2> d2_wo;
typedef digital_pin_wo<D3> d3_wo;
typedef digital_pin_wo<D4> d4_wo;
typedef digital_pin_wo<D5> d5_wo;
typedef digital_pin_wo<D6> d6_wo;
typedef digital_pin_wo<D7> d7_w0;

}; // namespace blocks


    
    

    

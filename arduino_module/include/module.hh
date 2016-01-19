/*
 * @name module.hh
 * @author Benedict R. Gaster
 * @description
 *
 */
#pragma once

//#include <Arduino.h>
#include <stdint.h>

#include <tuple>
#include <utility> // C++14 integer sequeneces needed for tuple indexing

#include <controller.hh>
#include <helper.hh>

// currently there is a bug in GCC which means varadic templates are
// not being expanded propably in some cases:
// see bug:
//     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=47226
//
// The code contains a work-around to handle this for now, hopefully one
// day we can remove it!


//------------------------------------------------------------------------------

namespace blocks {

typedef uint8_t module_id;

/**
 * @class
 * @description
 *
 * WARNING: Creating multiple instances of this class is going to do 
 *          bad things, dont do it!
 *          Copy constructor and assignment operator deleted
 */
template<module_id ModuleID, typename... Controllers>
class module
{
public:
    typedef uint16_t control_packet;

private:
    static constexpr std::tuple<Controllers...> controllers_{};

    static constexpr uint8_t number_controllers{sizeof...(Controllers)};
    
    static uint8_t msg_buffer_[number_controllers * sizeof(uint16_t)];
    static constexpr module_id mod_id_{ModuleID};	

    template<typename Controller>
    void setup_controller(const Controller& controller) const
    {
	controller.setup();
    }
    
    template<size_t... I>
    void setup_controllers(std::index_sequence<I...>) const
    {
	// it would be nice to call setup inline, but GCC is broken in
	// this regard!
	setup_controller(std::get<I>(controllers_)...);
    }
public:
    // no copying or assingment of protocol
    module(module const&) = delete; 
    module& operator=(module const&) = delete; 

    /*
     * @constructor protocol
     * @description default constructor
     */
    constexpr module()
    {
	
    }
    
    void setup() const
    {
	// Serial.begin(57600);

	// while (!Serial) {
	//     ; // wait for serial port to connect. Needed for Leonardo only
	// }

	// // protocol requires we write 65 ('A') and then
	// // wait to handshake with 66 ('B') on the return
	// Serial.write(65);
	// int inByte = 0;
	// while (inByte  != 66) {	    
	//     inByte = Serial.read();
	// }

	// setup the controllers
	setup_controllers<sizeof...(Controllers)-1>(
	    std::index_sequence<sizeof...(Controllers)-1>());
	
	
    }

    // some here functions for creating elements of the protocol
    
    /**
     * @method control_packet
     * @description 
     */
    static inline control_packet make_control_packet(uint8_t id, uint8_t value)
    {
	return (value << 8 | id);
    }

};

//------------------------------------------------------------------------------
// allocate any statics and static constexprs

template<module_id ModuleId, typename... Controllers>
uint8_t module<ModuleId, Controllers...>::msg_buffer_[] = {0};

template<module_id ModuleId, typename... Controllers>
constexpr std::tuple<Controllers...>
module<ModuleId, Controllers...>::controllers_;

template<module_id ModuleId, typename... Controllers>
constexpr module_id module<ModuleId, Controllers...>::mod_id_;

}; // namespace blocks


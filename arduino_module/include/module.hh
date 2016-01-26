/*
 * @name module.hh
 * @author Benedict R. Gaster
 * @description
 *
 */
#pragma once

#include <Arduino.h>

#include <stdint.h>

#include <tuple>
#include <utility> // C++14 integer sequeneces needed for tuple indexing

#include <protocol.hh>
#include <controller.hh>

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
template<uint32_t baud, module_id ModuleID, typename... Controllers>
class module
{
private:
    static std::tuple<Controllers...> controllers_;

    static constexpr uint8_t number_controllers{sizeof...(Controllers)};

    static protocol<baud, ModuleID, number_controllers> protocol_;
    
    //static uint8_t msg_buffer_[number_controllers * sizeof(control_packet)];
    static constexpr module_id mod_id_{ModuleID};	

    // some helper functions to expand and work on controllers
    // from the tuple container
    template<typename Controller>
    void setup_controller(const Controller& controller) const
    {
	controller.setup();
    }
    
    template<size_t... I>
    void setup_controllers(std::index_sequence<I...>) const
    {
	int ignore[] { (setup_controller(std::get<I>(controllers_)), 0)... };
	(void) ignore; // silence compiler warnings about the unused local variable
    }

    template<typename Controller>
    void run_controller(Controller& controller)
    {
	control_packet packet = controller.run();
	if (packet != invalid_control_packet()) {
	    protocol_.push_control_packet(packet);
	}
    }

    template<size_t... I>
    void run_controllers(std::index_sequence<I...>)
    {
	int ignore[] { (run_controller(std::get<I>(controllers_)), 0)... };
	(void) ignore; // silence compiler warnings about the unused local variable
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

    /*
     * @method setup
     * @description 
     *
     * called once during microcontroller setup, we need
     * to setup Serial usage and all controllers
     * 
     * TODO: we could move the serial suff out to its own place
     * to allow composibility with other uses, however, today
     * we are the main interface and so do it here for now.
     * 
     */
    void setup() const
    {
	// setup protocol interface
	protocol_.setup();
	
	// setup the controllers
	setup_controllers(
	    std::make_index_sequence<sizeof...(Controllers)-1>());	
    }

    /**
     * @method execute_iteration
     * @description
     *
     *    executed for each iteration of the main loop. process each
     *    controller:
     *
     *            - determining if a control packet should be sent 
     *              for a change in its state. 
     *            - build and send, if necessary, module control packet  
     */
    void run() {
	//run each controller to produce a control packets
	run_controllers(
	    std::make_index_sequence<sizeof...(Controllers)>());
	
	// send any generated control packets
	protocol_.send();
    }    
};

//------------------------------------------------------------------------------
// allocate any statics and static constexprs

template<uint32_t baud, module_id ModuleId, typename... Controllers>
protocol<
    baud,
    ModuleId,
    module<baud, ModuleId, Controllers...>::number_controllers>
module<baud, ModuleId, Controllers...>::protocol_;

template<uint32_t baud, module_id ModuleId, typename... Controllers>
std::tuple<Controllers...>
module<baud, ModuleId, Controllers...>::controllers_;

template<uint32_t baud, module_id ModuleId, typename... Controllers>
constexpr module_id module<baud, ModuleId, Controllers...>::mod_id_;

}; // namespace blocks


/*
 * Name: controller.hh
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
#include <pin.hh>

#if defined(__DEBUG__)
#define __assert__(msg) printf("%s", (msg))
#else
#define __assert__(msg)
#endif

namespace blocks {

// we support up to 256 different types of controllers, but
// currently provide nowhere near this number
typedef uint8_t controller_id;

/*
 * @enum class controller_type
 * @description
 */
enum class controller_type {
    LINEAR_POT,
    LINEAR_SLIDER,
    TOGGLE_PAIR,
    SWITCH_TRIPPLE,
    LED_SWITCH,
    UNDEFINED	
};

namespace detail {

/*
 * @template class controller_policy
 * @description
 */
template<
    controller_id Id,
    controller_type T,
    typename Pin,
    control_command Cmd>
class controller_policy {
	
private:
    // all memeber data is defined static constexpr, replying on the fact
    // that the compiler will optimize this initailization. moreover,
    // for this to work Id must be unique for each controller of a given
    // module! if, not the compiler will catch it and produce a static error
    static constexpr controller_id id_ = Id;
    static constexpr controller_type type_ = T;
    static constexpr Pin pin_ = Pin();
    
    static constexpr control_command ctl_cmd_ = Cmd;
public:
    // only default constructor exposed to application
    controller_policy()
    { }

    // as this intended to be allocated only once, at compile time,
    // then delete copy/assignment constructors
    controller_policy(controller_policy const&) = delete; 
    controller_policy& operator=(controller_policy const&) = delete; 
    
    auto get_type() const 
    {
	return type_;
    }

    auto get_controller_id() const
    {
	return id_;
    }

    
    auto get_pin() const
    {
	return pin_;
    }
    

    auto get_command() const
    {
	return ctl_cmd_;
    }    
};

} // namespace detail

template<
    controller_id Id,
    controller_type Type,
    typename Pin1,
    control_command Cmd,
    typename Pin2 = pin<PIN_UNDEFINED, modify_policy_undefined>(),
    control_command Cmd2 = CONTROL_COMMAND_UNDEFINED,
    typename Pin3 = pin<PIN_UNDEFINED, modify_policy_undefined>(),
    control_command Cmd3 = CONTROL_COMMAND_UNDEFINED,
    // some "fake" arguments to make sure this
    // only fires when there is no valid instance
    int X = 0, size_t T = sizeof(char)> 
class controller {    
    static_assert(
	sizeof(Type) < sizeof(T) && X != 0,
	"Unsupported controller");
};    

template <
    controller_id Id,
    typename Pin,
    control_command Cmd>
class controller <
    Id,
    controller_type::LINEAR_POT,
    Pin,
    Cmd> : public  detail::controller_policy<
                         Id,
                         controller_type::LINEAR_POT,
                         Pin,
                         Cmd> {
    // note: as we are 100% static should not be need to initialize
    // anything here!
};

template <
    controller_id Id,
    typename Pin1,
    control_command Cmd1,
    typename Pin2,
    control_command Cmd2>
class controller <
    Id,
    controller_type::SWITCH_TRIPPLE,
    Pin1, Cmd1,
    Pin2, Cmd2> : public  detail::controller_policy<
                         Id,
                         controller_type::SWITCH_TRIPPLE,
                         Pin1,
                         Cmd1> {
    // note: as we are 100% static should not be need to initialize
    // anything here!

    static constexpr Pin2 pin2_ = Pin2();
    static constexpr control_command ctl_cmd2_ = Cmd2;
};

template <
    controller_id Id,
    typename Pin1,
    control_command Cmd1,
    typename Pin2,
    control_command Cmd2,
    typename Pin3>
class controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1, Cmd1,
    Pin2, Cmd2,
    Pin3,
    CONTROL_COMMAND_UNDEFINED> : public  detail::controller_policy<
                         Id,
                         controller_type::TOGGLE_PAIR,
                         Pin1,
                         Cmd1> {
public:
    typedef detail::controller_policy<
                         Id,
                         controller_type::TOGGLE_PAIR,
                         Pin1,
                         Cmd1> controller_policy_;
    
    
    // note: as we are 100% static should not be need to initialize
    // anything here!

    static constexpr Pin2 pin2_ = Pin2();
    static constexpr control_command ctl_cmd2_ = Cmd2;
    static constexpr Pin2 pin3_ = Pin3();

public:
    
    void setup() const {
	controller_policy_::get_pin().setup();
    }
    
};


} // namespace blocks

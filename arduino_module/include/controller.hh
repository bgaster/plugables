/*
 * @name controller.hh
 * @author Benedict R. Gaster
 * @description
 *
 * The code is designed to be used with statically known controllers,
 * in particular, most (if not all) parameters are set via template
 * parameters. The reason for this is that the code is designed for
 * very small micro-controllers and thus we want to avoid any
 * unecesarry allocations and avoid indrection when possible.
 *
 * This makes the code a pain to write by hand, but this is not the
 * intention, it is designed to be used by automatically generated
 * code, from the blocks instrument designer.
 *
 * This is not how one might right the same code when dynamic
 * behaviour is necessary or space/performance is less of a conern.
 *
 *
 * Note: 0 is not a valid Controller ID. This is checked and a compile
 *       time error is reported if the invariant is invalid.
 *
 */
#pragma once

#include <stdint.h>

#include <autility.hh>
#include <pin.hh>
#include <button.hh>
#include <protocol.hh>

#if defined(__DEBUG__)
#include <stdio.h>
#define __assert__(msg) printf("%s", (msg))
#else
#define __assert__(msg)
#endif

namespace blocks {

// we support up to 256 different types of controllers, but
// currently provide nowhere near this number
typedef uint8_t controller_id;

inline constexpr controller_id controller_inc(controller_id id)
{
    return id + 1;
}

/*
 * @enum class controller_type
 * @description
 */
enum class controller_type
{
    LINEAR_POT,
    LINEAR_SLIDER,
    TOGGLE_PAIR,
    SWITCH_TRIPPLE,
    UNDEFINED	
};

namespace detail {

/*
 * @template class controller_policy
 * @description
 */
template<
    controller_id Id, // Id == 0 is not valid!
    controller_type T,
    typename Pin>
class controller_policy {
	
private:
    // all memeber data is defined static constexpr, replying on the fact
    // that the compiler will optimize this initailization. moreover,
    // for this to work Id must be unique for each controller of a given
    // module! if, not the compiler will catch it and produce a static error
    static constexpr controller_id id_ = Id;
    static constexpr controller_type type_ = T;
    static constexpr Pin pin_ {};

public:
    // only default constructor exposed to application
    constexpr controller_policy()
    {
	// Verify that Controller ID is non-zero
	static_assert(Id != 0, "0 is not a valid Controller ID");
    }

    // as this intended to be allocated only once, at compile time,
    // then delete copy/assignment constructors
    controller_policy(controller_policy const&) = delete; 
    controller_policy& operator=(controller_policy const&) = delete; 

    /**
     * @method get_type
     * @description getter for controller type
     */
    auto get_type() const 
    {
	return type_;
    }

    /**
     * @method get_type
     * @description getter for controller ID
     */
    auto get_controller_id() const
    {
	return id_;
    }

    /**
     * @method get_type
     * @description getter for controller pin 1
     */    
    auto get_pin() const
    {
	return pin_;
    }
};

} // namespace detail

//------------------------------------------------------------------------------

/**
 * @template class controller
 *
 * @description this is the base template class and should never be
 * instantiate! If it is, then a compile time error will be raised 
 */
template<
    controller_id Id,
    controller_type Type,
    typename Pin1,
    typename Pin2 = pin<PIN_UNDEFINED, modify_policy_undefined>(),
    typename Pin3 = pin<PIN_UNDEFINED, modify_policy_undefined>(),
    typename Pin4 = pin<PIN_UNDEFINED, modify_policy_undefined>(),
    // some "fake" arguments to make sure this
    // only fires when there is no valid instance
    int X = 0, size_t T = sizeof(char)> 
class controller {    
    static_assert(
	sizeof(Type) < sizeof(T) && X != 0,
	"Unsupported controller");
};    

// now we define the valid specializations for each controller type

/**
 * @template class specialization for controller
 *
 * @description linearPot controller
 *
 * We provide a basic 'smoothing' average to avoid sending
 * 'tiny' variants in the anolog signal.
 */
template <
    controller_id Id,
    typename Pin>
class controller <
    Id,
    controller_type::LINEAR_POT,
    Pin> : public  detail::controller_policy<
                         Id,
                         controller_type::LINEAR_POT,
                         Pin>
{
private:
    static constexpr uint8_t num_readings_{10};
    
    uint16_t readings_[num_readings_];
    uint16_t old_average_;
    uint16_t total_;
    uint8_t index_;
    
public:
    typedef detail::controller_policy<
                         Id,
                         controller_type::LINEAR_POT,
                         Pin> controller_policy_;

    /**
     * @constructor controller
     * @description
     * 
     */
    constexpr controller() :
	readings_{0,0,0,0,0,0,0,0,0,0},
	old_average_(0),
	total_(0),
	index_(0)
    {
    }
    
    /**
     * @method setup
     *
     * @description
     */
    constexpr void setup() const
    {
	controller_policy_::get_pin().setup();
    }

    /**
     * @method run
     * @description
     * @returns a valid control packet (non-zero), or 0
     *
     * FIXME: add constexprs to get pot range, midi range, and so on, i.e.
     * avoid using magic numbers
     */
    template<typename F>
    void run(F pusher)
    {	
	total_ -= readings_[index_];
	
	// remap the value to MINI limits,
	// perform the remapping now to limit variation
	readings_[index_] =
	    utils::remap<0, 1024, 0, 128>(controller_policy_::get_pin().read());

	total_ += readings_[index_];

	index_ = (index_+ 1) % utils::arraysize(readings_);
	
	uint16_t average = total_ / utils::arraysize(readings_);
	
	if (old_average_ != average) {
	    old_average_ = average;
	    pusher(make_control_packet<Id>(average));
	}
    }
};

/**
 * @template class specialization for controller
 *
 * @description linearSlider controller
 * 
 * We provide abasic 'smoothing' average to avoid sending
 * 'tiny' variants in the anolog signal.
 */
template <
    controller_id Id,
    typename Pin>
class controller <
    Id,
    controller_type::LINEAR_SLIDER,
    Pin> : public  detail::controller_policy<
                         Id,
                         controller_type::LINEAR_SLIDER,
                         Pin>
{
private:
    static constexpr uint8_t num_readings_{10};
    uint16_t readings_[num_readings_];
    uint16_t old_average_;
    uint16_t total_;
    uint8_t index_;
public:
    typedef detail::controller_policy<
                         Id,
                         controller_type::LINEAR_SLIDER,
                         Pin> controller_policy_;

    /*
     * @constructor controller
     *
     * @description default constructor for linearSlider controller
     * 
     */    
    constexpr controller() :
	readings_{0,0,0,0,0,0,0,0,0,0},
	old_average_(0),
	total_(0),
	index_(0)
    {
    }

    /*
     * @method setup
     *
     * @description
     */
    constexpr void setup() const {
	controller_policy_::get_pin().setup();
    }

    /**
     * @method run
     * @description
     * @returns a valid control packet (non-zero), or 0
     *
     * FIXME: add constexprs to get slider range, midi range, and so on, i.e.
     * avoid using magic numbers
     */
    template<typename F>
    void run(F pusher)
    {	
	total_ -= readings_[index_];
	
	// remap the value to MINI limits,
	// perform the remapping now to limit variation
	readings_[index_] =
	    utils::remap<0, 1024, 0, 128>(controller_policy_::get_pin().read());

	total_ += readings_[index_];

	index_ = (index_+ 1) % utils::arraysize(readings_);
	
	uint16_t average = total_ / utils::arraysize(readings_);
	
	if (old_average_ != average) {
	    old_average_ = average;
	    pusher( make_control_packet<Id>(average) );
	}
    }
};


/**
 * @template class specialization for controller
 *
 * @description switchTripple controller
 */
template <
    controller_id Id,
    typename Pin1,
    typename Pin2>
class controller <
    Id,
    controller_type::SWITCH_TRIPPLE,
    Pin1,
    Pin2> : public  detail::controller_policy<
                         Id,
                         controller_type::SWITCH_TRIPPLE,
                         Pin1>
{
private:
    // note: as we are 100% static should not be need to initialize
    // anything here!

    static constexpr Pin2 pin2_ { };

public:
    typedef detail::controller_policy<
                         Id,
                         controller_type::SWITCH_TRIPPLE,
                         Pin1> controller_policy_;

    constexpr controller() 
    {
    }

    /*
     * @method setup
     *
     * @description
     */
    constexpr void setup() const
    {
	controller_policy_::get_pin().setup();
	pin2_.setup();
    }

};

/**
 * @template class specialization for controller
 *
 * @description togglePair controller
 *
 * Currently, this is very specific to Reaktor block's mod A & B
 * controls within a given block. The behaviour is as follows:
 *
 *  A led off
 *  B lef off
 *
 *  Repeat
 *    if A pressed, then
 *         send control on (127), if a led off, set a led on and b led off
 *         send control off (0), if a led on, set a led off
 *    if B pressed, then
 *         send control on (127), if b led off, set b led on and a led off
 *         send control off (0), if b led on, set b led off
 *
 *   Note that while the opposite led is switched of if on, when other pressed,
 *   no control off message is sent, as Reaktor does not require it.
 *
 * Additionally, using this controller needs to be slightly careful as it
 * assumes that the send Id (for button A), used as unique control data indentifier, 
 * is followed in sequence by an Id (for button B), but it is left implicit, thus
 * must be caefully allocated by the caller, even though not passed as an argument. 
 * 
 * FIXME: maybe rename to account for Reaktor behaviour?
 */
template <
    controller_id Id, // IdA = Id, IdB = controller_inc(Id)
    typename Pin1,    // button A
    typename Pin2,    // LED A
    typename Pin3,    // button B
    typename Pin4>    // LED B
class controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3,
    Pin4> : public  detail::controller_policy<
                         Id,
                         controller_type::TOGGLE_PAIR,
                         Pin1>
{
private:
    typedef detail::controller_policy<
                         Id,
                         controller_type::TOGGLE_PAIR,
                         Pin1> controller_policy_;
    
    
    // note the following only works by the fact that controllers
    // have unique IDs, which are passed as template parameter
    static constexpr Pin2 pin2_ {};
    static constexpr Pin3 pin3_ {};
    static constexpr Pin4 pin4_ {};

    // led's states
    uint16_t led_a_state_;
    uint16_t led_b_state_;

    // buttons
    button<Pin1> buttonA_;
    button<Pin3> buttonB_;
    
public:
    /**
     * @constructor controller
     * @description
     *  Default constructor for toggle pair controller
     */
    constexpr controller() :
	led_a_state_{HIGH},
	led_b_state_{HIGH},
	buttonA_{controller_policy_::get_pin()},
	buttonB_{pin3_}
    {
    }    
    
    /*
     * @method setup
     *
     * @description
     *  Setup method for controller, called just once before controller can be used
     */
    constexpr void setup() const
    {
	// setup button A & LED A
	buttonA_.setup();
	pin2_.setup();

	
	// setup button B & LED B
	buttonB_.setup();
	pin4_.setup();
    }

    /**
     * @template method run
     * description
     *   Run method for controller, called each iteration of control loop
     */
    template<typename F>
    void run(F pusher)
    {
	// we priortized button A, for no other reason other than we had to
	// pick one!
	if (buttonA_.run()) {
	    if (led_a_state_ == HIGH) {
		led_a_state_ = LOW;
		// control message off
		pusher( make_control_packet<Id>(0) );
	    }
	    else {
		led_a_state_ = HIGH;
		// control message on
		pusher( make_control_packet<Id>(127) );
		// turn B's led off
		led_b_state_ = LOW;
	    }
	}
	else if (buttonB_.run()) {
	    if (led_b_state_ == HIGH) {
		led_b_state_ = LOW;
		// control message off
		pusher( make_control_packet<controller_inc(Id)>(0) );
	    }
	    else {
		led_b_state_ = HIGH;
		// control message on
		pusher( make_control_packet<controller_inc(Id)>(127) );
		// turn A's led off
		led_a_state_ = LOW;
	    }
	}

	// update leds
	pin2_.write(led_a_state_);
	pin4_.write(led_b_state_);
    }
};

//------------------------------------------------------------------------------
// allocate any static constexprs

// SWITCH_TRIPPLE
template <
    controller_id Id,
    typename Pin1,
    typename Pin2>
constexpr Pin2 controller <
    Id,
    controller_type::SWITCH_TRIPPLE,
    Pin1,
    Pin2>::pin2_;

// TOGGLE_PAIR
template <
    controller_id Id,
    typename Pin1,
    typename Pin2,
    typename Pin3,
    typename Pin4>
constexpr Pin2 controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3,
    Pin4>::pin2_;

template <
    controller_id Id,
    typename Pin1,
    typename Pin2,
    typename Pin3,
    typename Pin4>
constexpr Pin3 controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3,
    Pin4>::pin3_;

template <
    controller_id Id,
    typename Pin1,
    typename Pin2,
    typename Pin3,
    typename Pin4>
constexpr Pin4 controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3,
    Pin4>::pin4_;


} // namespace blocks

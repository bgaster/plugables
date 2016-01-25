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
 */
#pragma once

#include <stdint.h>

#include <helper.hh>
#include <pin.hh>

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
    controller_id Id,
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
    { }

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
    // FIXME: this requies space, but allows us to only send changes
    int old_value;
    
public:
    typedef detail::controller_policy<
                         Id,
                         controller_type::LINEAR_POT,
                         Pin> controller_policy_;
    
    constexpr controller() :
	old_value(0) 
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
};

/**
 * @template class specialization for controller
 *
 * @description linearSlider controller
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
    constexpr controller() 
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
 */
template <
    controller_id Id,
    typename Pin1,
    typename Pin2,
    typename Pin3>
class controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3> : public  detail::controller_policy<
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
    
public:
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
	pin3_.setup();
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
    typename Pin3>
constexpr Pin2 controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3>::pin2_;

template <
    controller_id Id,
    typename Pin1,
    typename Pin2,
    typename Pin3>
constexpr Pin3 controller <
    Id,
    controller_type::TOGGLE_PAIR,
    Pin1,
    Pin2,
    Pin3>::pin3_;


} // namespace blocks

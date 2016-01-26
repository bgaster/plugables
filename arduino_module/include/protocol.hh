/*
 * @name protocol.hh
 * @author Benedict R. Gaster
 * @description
 *
 */
#pragma once

#include <Arduino.h>

#include <stdint.h>

namespace blocks {

typedef uint16_t control_packet;

/**
 * @
 * @ description
 */
template<uint32_t baud, uint8_t ModuleId, uint8_t num_controllers>
class protocol
{
private:
    control_packet packets_[num_controllers];    
    uint8_t num_packets_;
    
public:
    
    /**
     * @constructor protocol
     * @description default constructor
     */
    protocol() :
	packets_{0},
	num_packets_{0}
    {
    }

    // note copy is not deep!
    protocol(protocol<baud, ModuleId, num_controllers> const&) = default;
    protocol(protocol<baud, ModuleId, num_controllers>&&) = default;
    protocol& operator=(protocol<baud, ModuleId, num_controllers> const& rhs) = default;
    protocol& operator=(protocol<baud, ModuleId, num_controllers>&&) = default;

    /**
     * @method setup
     * @description
     */
    void setup()
    {
	Serial.begin(baud);

	while (!Serial) {
	     ; // wait for serial port to connect. Needed for Leonardo only
	}

	// // protocol requires we write 65 ('A') and then
	// // wait to handshake with 66 ('B') on the return
	// Serial.write(65);
	// int inByte = 0;
	// while (inByte  != 66) {	    
	//     inByte = Serial.read();
	// }
    }

    /**
     * @method push_control_packet
     * @description
     */
    void push_control_packet(control_packet packet)
    {
	packets_[num_packets_++] = packet;
    }    
    
    /**
     * @method send
     * @description
     */
    void send() 
    {
	// if we have packets to send, then send them :-)
	if (num_packets_ > 0) {
	    Serial.write(ModuleId);
	    Serial.write(num_packets_);
	    Serial.write(
	     	reinterpret_cast<const char*>(packets_),
	     	num_packets_ * sizeof(control_packet));	   

	    num_packets_ = 0;
	}
    }
};

//------------------------------------------------------------------------------
// allocate any statics and static constexprs

//template<uint32_t baud, uint8_t num_controllers>
//control_packet protocol<baud, num_controllers>::packets_[] = {0};

//------------------------------------------------------------------------------

/**
 * @method control_packet
 * @description 
 */
template<uint8_t Id>
inline control_packet make_control_packet(uint8_t value)
{
    return (value << 8 | Id);
}

inline constexpr control_packet invalid_control_packet()
{
    return 0;
}

} // namespace blocks



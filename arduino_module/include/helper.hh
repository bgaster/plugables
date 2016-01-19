// this is just a dump for stuff i'm not sure (yet) where it should go...

#pragma once

#include <stdint.h>

namespace blocks {

// midi control messages
// note: we used signed here as MIDI only supports 0-127!
typedef int8_t control_command;

static constexpr control_command CONTROL_COMMAND_UNDEFINED = -1;    

}; // namespace blocks

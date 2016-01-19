#include <stdio.h>
#include <stdint.h>

#include <pin.hh>
#include <controller.hh>
#include <module.hh>

using namespace blocks;

constexpr module_id mod_id{1};

constexpr module<
    1,
    controller<
	1,
	controller_type::LINEAR_POT,
	a0_ro>,
    controller<
	2,
	controller_type::TOGGLE_PAIR,
	d0_ro,
	d1_ro,
	d2_wo>> mod; 
 

// constexpr controller<
//     1,
//     controller_type::LINEAR_POT,
//     a0_ro> pot;

// constexpr controller<
//     2,
//     controller_type::TOGGLE_PAIR,
//     d0_ro,
//     d1_ro,
//     d2_wo> mod_ab;

void setup(void)
{
    mod.setup();
}

void execute_iteration()
{
}


int main(void)
{
    setup();
}

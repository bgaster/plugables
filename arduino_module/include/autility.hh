/*
 * Name: autility.hh
 * Author: Benedict R. Gaster
 * Desc:
 *
 */
#pragma once


namespace utils {

/*
 * @function map
 * @description a template version of Arduino's remap range function
 */
template<long in_min, long in_max, long out_min, long out_max>
inline long remap(long x)
{
  return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}

/*
 * @function arraysize
 * @description calculates the size of an array
 */
template <typename T,unsigned S>
inline unsigned arraysize(const T (&v)[S]) { return S; }

} // namespace utils


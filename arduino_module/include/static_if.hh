/*
 * Name: static_if.hh
 * Author: Benedict R. Gaster
 * Desc:
 *
 * Simple static_if implementation using generic lambdas.
 *
 * Based on a discussion on the Boost mailing list: 
 *      http://lists.boost.org/Archives/boost/2014/08/216607.php
 *
 * There seems to be ongoing resistance to adding static_if to the language,
 * personally I feel that it would be good to enchance C++ meta-programming
 * with if, while, and so on.
 */

#include <utility>

namespace blocks {

namespace static_if_detail {

struct identity {
    template<typename T>
    T operator()(T&& x) const {
        return std::forward<T>(x);
    }
};

template<bool Cond>
struct statement {
    template<typename F>
    void then(const F& f){
        f(identity());
    }

    template<typename F>
    void else_(const F&){}
};

template<>
struct statement<false> {
    template<typename F>
    void then(const F&){}

    template<typename F>
    void else_(const F& f){
        f(identity());
    }
};

} //end of namespace static_if_detail

template<bool Cond, typename F>
static_if_detail::statement<Cond> static_if(F const& f){
    static_if_detail::statement<Cond> if_;
    if_.then(f);
    return if_;
}

} // namespace blocks

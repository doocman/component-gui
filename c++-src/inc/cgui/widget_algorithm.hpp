#ifndef COMPONENT_GUI_CGUI_WIDGET_ALGORITHM_HPP
#define COMPONENT_GUI_CGUI_WIDGET_ALGORITHM_HPP

#include <cgui/cgui-call.hpp>

namespace cgui {

constexpr auto equals(auto&& v) {
  return [v = std::forward<decltype(v)>(v)] (std::equality_comparable_with<decltype(v)> auto&& rhs) {
    return v == rhs;
  };
}

template <typename Pred, typename OnFind, typename Tuple>
  requires has_for_each<Tuple, Pred> && (has_for_each<Tuple, OnFind> || has_for_each<Tuple, bp::trailing_curried<OnFind&&, std::size_t>>)
constexpr bool find_first_cb(Pred&& predicate, OnFind&& find_func, Tuple&& values) {
  auto call_f = [&find_func] <typename T> (bp::as_forward<T> t, std::ptrdiff_t i) {
    if constexpr (std::invocable<OnFind, T&&, std::size_t>) {
      std::invoke(std::forward<OnFind>(find_func), *t, static_cast<std::size_t>(i));
    } else {
      std::invoke(std::forward<OnFind>(find_func), *t);
    }
  };
  if constexpr(requires() { call::apply_to(values, bp::no_op); }) {
    // Tuple-like. We use the shortcutting from OR operators to only search up til the correct values.
    return call::apply_to(std::forward<Tuple>(values), [&predicate, &call_f]<typename... Ts>(Ts&&... vs) {
      std::ptrdiff_t i = 0;
      auto run_condition = [&predicate, &call_f, &i]<typename T0>(T0 v) {
        if (predicate(v.as_cref())) {
          call_f(v, i);
          return true;
        }
        ++i;
        return  false;
      };
      return (... || run_condition(bp::as_forward<Ts>(vs)));
    });
  } else {
    // Fallback implementation that must run on all elements using for_each.
    bool found = false;
    std::ptrdiff_t i = 0;
    auto run_condition = [&found, &predicate, &call_f, &i]<typename T>(T&& v) {
      if (!found && predicate(v)) {
        found = true;
        call_f(bp::as_forward<T>(v), i);
      }
      ++i;
      return found;
    };
    call::for_each(std::forward<Tuple>(values), run_condition);
    return found;
  }
}

}

#endif

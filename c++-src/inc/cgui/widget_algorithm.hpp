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
  requires has_for_each<Tuple, Pred> && has_for_each<Tuple, OnFind>
constexpr bool find_first_cb(Pred&& predicate, OnFind&& find_func, Tuple&& values) {
  if constexpr(requires() { call::apply_to(values, bp::no_op); }) {
    // Tuple-like. We use the shortcutting from OR operators to only search up til the correct values.
    return call::apply_to(std::forward<Tuple>(values), [&predicate, &find_func]<typename... Ts>(Ts&&... vs) {
      auto run_condition = [&predicate, &find_func]<typename T0>(T0&& v) {
        if (predicate(v)) {
          find_func(std::forward<T0>(v));
          return true;
        }
        return  false;
      };
      return (... || run_condition(std::forward<Ts>(vs)));
    });
  } else {
    // Fallback implementation that must run on all elements using for_each.
    bool found = false;
    auto run_condition = [&found, &predicate, &find_func]<typename Ts>(Ts&& v) {
      if (!found && predicate(v)) {
        found = true;
        find_func(std::forward<Ts>(v));
      }
      return found;
    };
    call::for_each(std::forward<Tuple>(values), run_condition);
    return found;
  }
}

}

#endif

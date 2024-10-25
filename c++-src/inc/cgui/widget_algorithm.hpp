#ifndef COMPONENT_GUI_CGUI_WIDGET_ALGORITHM_HPP
#define COMPONENT_GUI_CGUI_WIDGET_ALGORITHM_HPP

#include <cgui/cgui-call.hpp>
#include <cgui/geometry.hpp>

namespace cgui {

namespace call {
namespace impl {

struct find_sub_t {
  template <typename F> struct find_func_constraint {
    template <typename T>
      requires(std::invocable<F, T &&, std::size_t> || std::invocable<F, T &&>)
    constexpr void operator()(T &&) const {}
  };
  template <typename T, typename Predicate, typename FindFunction>
    requires(
        has_find_sub<T, Predicate, FindFunction> ||
        (std::invocable<do_for_each, T, Predicate> &&
         std::invocable<do_for_each, T, find_func_constraint<FindFunction>>))
  constexpr bool operator()(T &&tr, Predicate &&pr, FindFunction &&fr) const {
    auto t = bp::as_forward<T>(tr);
    auto p = bp::as_forward<Predicate>(pr);
    auto f = bp::as_forward<FindFunction>(fr);
    if constexpr (has_find_sub<T, Predicate, FindFunction>) {
      return _do_find_sub::call(*t, *p, *f);
    } else {
      auto call_f = [&f]<typename T2>(bp::as_forward<T2> t2, std::ptrdiff_t i) {
        if constexpr (std::invocable<FindFunction, T2 &&, std::size_t>) {
          std::invoke(*f, *t2, static_cast<std::size_t>(i));
        } else {
          std::invoke(*f, *t2);
        }
      };
      if constexpr (std::invocable<do_apply_to, T, bp::no_op_t>) {
        // Tuple-like. We use the shortcutting from OR operators to only search
        // up til the correct values.
        return do_apply_to::call(*t, [&p,
                                      &call_f]<typename... Ts>(Ts &&...vs_in) {
          auto run_all = [&p, &call_f]<std::size_t... is>(
                             std::index_sequence<is...>, Ts &&...vs) {
            auto run_condition = [&p, &call_f]<typename T0>(T0 v,
                                                            std::size_t fi) {
              if (predicate(v.as_cref())) {
                call_f(v, fi);
                return true;
              }
              return false;
            };

            return (... || run_condition(bp::as_forward<Ts>(vs), is));
          };
          return run_all(std::make_index_sequence<sizeof...(Ts)>{},
                         std::forward<Ts>(vs_in)...);
        });
      } else if constexpr (std::invocable<do_for_each, T, bp::no_op_t>) {
        // Fallback implementation that must run on all elements using for_each.
        bool found = false;
        auto run_condition = [&found, &p, &call_f]<typename T2>(T2 &&v,
                                                                std::size_t i) {
          if (!found && (*p)(std::as_const(v), i)) {
            found = true;
            call_f(bp::as_forward<T2>(v), i);
          }
          return found;
        };
        do_for_each{}(*t, run_condition);
        return found;
      } else {
        static_assert(
            std::is_same_v<T, void>,
            "No other fallback implementations available at the time");
      }
    }
    return false;
  }
};

struct find_sub_at_location_t {
  template <typename Pos> struct finder {
    Pos p;

    template <typename T>
      requires(std::invocable<_do_area, T const &>)
    constexpr bool operator()(T const &t) const {
      return hit_box(call::area(t), p);
    }
  };
  template <typename T, typename Pos, typename FindFunction>
    requires(has_find_sub_at_location<T, Pos, FindFunction> ||
             has_find_sub<T, finder<Pos>, FindFunction>)
  constexpr bool operator()(T &&tr, Pos const &p, FindFunction &&fr) const {
    auto t = bp::as_forward<T>(tr);
    auto f = bp::as_forward<FindFunction>(fr);
    if constexpr (has_find_sub_at_location<T, Pos, FindFunction>) {
      return _do_find_sub_at_location::call(*t, p, *f);
    } else {
      return find_sub_t{}(*t, finder<Pos>{p}, *f);
    }
  }
};

struct sub_accessor_t {
  template <typename SubId> struct index_finder {
    SubId i;
    constexpr bool operator()(auto &&, SubId const &i2) const {
      return i == i2;
    }
  };
  template <typename T, typename SubId> struct callback {
    SubId id;
    template <typename F>
    constexpr void operator()(bp::cvref_type<T> auto &&t, F &&f) {
      find_sub_t{}(
          std::forward<T>(t),
          [this](auto const &, auto const &id) { return this->id == id; },
          std::forward<F>(f));
    }
  };

  template <typename T, typename SubId, typename F>
    requires(has_sub_accessor<T &, SubId, arguments_marker_t<F>> ||
             requires(T &&t, index_finder<SubId> finder) {
               find_sub_t{}(t, finder, bp::no_op);
             })
  constexpr std::invocable<T &&, F> auto
  operator()(T &&t, SubId const &s, arguments_marker_t<F> am) const {
    if constexpr (has_sub_accessor<T, SubId, arguments_marker_t<F>>) {
      return _do_sub_accessor(std::forward<T>(t), s, am);
    } else {
      return callback<std::remove_cvref_t<T>, SubId>(s);
    }
  }
};

} // namespace impl

/// Iterates through list or tuple of objects given a predicate, calls a
/// function with the found object if found and returns true on found and false
/// otherwise.
inline constexpr impl::find_sub_t find_sub;
/// Iterates through list or tuple of objects with an area, calls a
/// function with the found object if it finds and object that covers the
/// specific position and returns true on found and false otherwise.
inline constexpr impl::find_sub_at_location_t find_sub_at_location;

/// Retrieves a function-like object for fast re-access of a specific element.
/// The function signature is Collection, Sub, Function-On-Sub, [Extra
/// arguments...]
inline constexpr impl::sub_accessor_t sub_accessor;
} // namespace call

constexpr auto equals(auto &&v) {
  return [v = std::forward<decltype(v)>(v)](
             std::equality_comparable_with<decltype(v)> auto &&rhs) {
    return v == rhs;
  };
}

template <typename Pred, typename OnFind, typename Tuple>
  requires has_for_each<Tuple, Pred> &&
           (has_for_each<Tuple, OnFind> ||
            has_for_each<Tuple, bp::trailing_curried<OnFind &&, std::size_t>>)
constexpr bool find_first_cb(Pred &&predicate, OnFind &&find_func,
                             Tuple &&values) {
  auto call_f = [&find_func]<typename T>(bp::as_forward<T> t,
                                         std::ptrdiff_t i) {
    if constexpr (std::invocable<OnFind, T &&, std::size_t>) {
      std::invoke(std::forward<OnFind>(find_func), *t,
                  static_cast<std::size_t>(i));
    } else {
      std::invoke(std::forward<OnFind>(find_func), *t);
    }
  };
  if constexpr (requires() { call::apply_to(values, bp::no_op); }) {
    // Tuple-like. We use the shortcutting from OR operators to only search up
    // til the correct values.
    return call::apply_to(
        std::forward<Tuple>(values),
        [&predicate, &call_f]<typename... Ts>(Ts &&...vs_in) {
          auto run_all = [&predicate, &call_f]<std::size_t... is>(
                             std::index_sequence<is...>, Ts &&...vs) {
            auto run_condition = [&predicate,
                                  &call_f]<typename T0>(T0 v, std::size_t fi) {
              if (predicate(v.as_cref())) {
                call_f(v, fi);
                return true;
              }
              return false;
            };

            return (... || run_condition(bp::as_forward<Ts>(vs), is));
          };
          return run_all(std::make_index_sequence<sizeof...(Ts)>{},
                         std::forward<Ts>(vs_in)...);
        });
  } else {
    // Fallback implementation that must run on all elements using for_each.
    bool found = false;
    auto run_condition = [&found, &predicate,
                          &call_f]<typename T>(T &&v, std::size_t i) {
      if (!found && predicate(v)) {
        found = true;
        call_f(bp::as_forward<T>(v), i);
      }
      return found;
    };
    call::for_each(std::forward<Tuple>(values), run_condition);
    return found;
  }
}

} // namespace cgui

#endif

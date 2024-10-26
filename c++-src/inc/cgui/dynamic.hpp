#ifndef COMPONENT_GUI_DYNAMIC_HPP
#define COMPONENT_GUI_DYNAMIC_HPP

#include <tuple>

#include <cgui/std-backport/utility.hpp>

namespace cgui::dynamic {
template <typename Displays>
class uni_sized_widget_list_impl : bp::empty_structs_optimiser<Displays> {
  using _base_t = bp::empty_structs_optimiser<Displays>;

public:
  using _base_t::_base_t;
};
template <typename State = widget_state_marker<int>>
struct widget_list_constraint {
  template <typename T>
    requires(has_render<T, dummy_renderer, widget_render_args<int, State>> /*&& !std::is_reference_v<std::unwrap_ref_decay_t<T>> &&
             std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>*/)
  constexpr void operator()(T &&) const {}
};

template <typename T, typename Constraint, typename... Args>
concept pass_or_build_pass =
    std::invocable<Constraint, T> ||
    (builder<T, Args...> &&
     requires(bp::as_forward<T> t, bp::as_forward<Args>... args, Constraint c) {
       c(call::build(*t, *args...));
     });

template <typename State = widget_state_marker<int>>
struct widget_list_builder_constraint {
  using all_states_t = all_states_in_marker_t<State>;
  template <typename T>
    requires(pass_or_build_pass<T, widget_list_constraint<State>, all_states_t>)
  constexpr void operator()(T &&) const {}
};
template <typename Displays>
class uni_sized_widget_list_builder_impl
    : bp::empty_structs_optimiser<Displays> {
  using _base_t = bp::empty_structs_optimiser<Displays>;

public:
  using _base_t::_base_t;

  template <typename... T2s,
            typename ResG = build::args_to_group_t<
                widget_list_builder_constraint<>, T2s...>,
            typename ResT = uni_sized_widget_list_builder_impl<ResG>>
  constexpr ResT displays(T2s &&...ds) && {
    static_assert(
        (std::invocable<widget_list_builder_constraint<>, T2s> && ...));
    return ResT(build::args_to_group(widget_list_builder_constraint{},
                                     std::forward<T2s>(ds)...));
  }

  template <typename State, State... states,
            typename Marker = widget_state_marker<State, states...>,
            typename ResT = uni_sized_widget_list_impl<
                build::build_group_t<widget_list_constraint<Marker>, Displays,
                                     all_states_in_marker_t<Marker>>>>
  constexpr ResT build(widget_states<State, states...>) {
    return ResT(build::build_group(widget_list_constraint{},
                                   std::move(*this).get_first(),
                                   all_states_in_marker_t<Marker>{}));
  }
};

constexpr uni_sized_widget_list_builder_impl<std::tuple<>>
uni_sized_widget_list_builder() {
  return {};
}
} // namespace cgui::dynamic

#endif // COMPONENT_GUI_DYNAMIC_HPP

#ifndef COMPONENT_GUI_DYNAMIC_HPP
#define COMPONENT_GUI_DYNAMIC_HPP

#include <functional>
#include <ranges>
#include <tuple>

#include <cgui/std-backport/tuple.hpp>
#include <cgui/std-backport/utility.hpp>
#include <cgui/std-backport/concepts.hpp>

namespace cgui::dynamic {
template <typename Displays>
class uni_sized_widget_list_impl : bp::empty_structs_optimiser<Displays> {
  using _base_t = bp::empty_structs_optimiser<Displays>;
  struct raw_element_t : bp::empty_structs_optimiser<Displays> {
    using _ebase_t = bp::empty_structs_optimiser<Displays>;
    using _ebase_t::_ebase_t;
  };
  std::vector<raw_element_t> elements_;
  default_size_wh element_size_{};

  constexpr default_rect element_area(std::size_t index) const {
    CGUI_ASSERT(index < size(elements_));
    return {};
  }

public:
  class ref_element_t {
    uni_sized_widget_list_impl *container_;
    std::size_t index_;

  public:
    constexpr ref_element_t(uni_sized_widget_list_impl &c, std::size_t i)
        : container_(&c), index_(i) {}
    [[nodiscard]] constexpr bounding_box auto area() const {
      return container_->element_area(index_);
    }
  };

  using _base_t::_base_t;

  constexpr void render(renderer auto &&r, auto &&b_arg) const {
    for (std::ptrdiff_t i = {}; auto& e : elements_) {
      auto const iu = static_cast<std::size_t>(i);
      auto w_arg = widget_render_args(element_area(iu), b_arg.button_state(i));
      call::render(e.get_first(), std::forward<decltype(r)>(r), w_arg);
    }
  }
  constexpr auto sub_accessor(std::size_t index) const {
    CGUI_ASSERT(index < size(elements_));
    return [index] (bp::cvref_type<uni_sized_widget_list_impl> auto&& self, auto&& cb) {
      CGUI_ASSERT(index < size(self.elements_));
      cb(ref_element_t(self, index));
    };
  }
  constexpr auto sub_accessor(std::size_t index, auto&&) const {
    return sub_accessor(index);
  }
  static constexpr Displays display_prototype() requires std::is_default_constructible_v<Displays> {
    return Displays{};
  }
  constexpr Displays display_prototype() const requires(!std::is_default_constructible_v<Displays> && std::is_copy_constructible_v<Displays>) {
    return this->get_first();
  }

  constexpr bool find_sub(std::predicate<ref_element_t &&, std::size_t> auto &&p,
                          std::invocable<ref_element_t, std::size_t> auto &&e) {
    return call::find_sub(std::views::transform(elements_, [this](auto &e) {
      return ref_element_t(
          *this, static_cast<std::size_t>(std::distance(elements_.data(), &e)));
    }), std::forward<decltype(p)>(p), std::forward<decltype(e)>(e));
  }
  constexpr bool find_sub_at_location(pixel_coord auto &&pos,
                                      std::invocable<ref_element_t> auto &&e) {
    if (call::height(element_size_) == 0) {
      return false;
    }
    auto pos_index = call::b_y(pos) / call::height(element_size_);
    if (pos_index < size(elements_)) {
      std::invoke(std::forward<decltype(e)>(e),
                  ref_element_t(*this, elements_[pos_index]));
      return true;
    }
    return false;
  }
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
template <typename Displays, typename Functions>
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

struct no_function_list {};

constexpr uni_sized_widget_list_builder_impl<std::tuple<>, no_function_list>
uni_sized_widget_list_builder() {
  return {};
}
} // namespace cgui::dynamic

#endif // COMPONENT_GUI_DYNAMIC_HPP

#ifndef COMPONENT_GUI_DYNAMIC_HPP
#define COMPONENT_GUI_DYNAMIC_HPP

#include <functional>
#include <ranges>
#include <tuple>

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/tuple.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui::dynamic {
template <typename Displays, typename Functions>
class uni_sized_widget_list_impl
    : bp::empty_structs_optimiser<Displays, Functions> {
  using _base_t = bp::empty_structs_optimiser<Displays, Functions>;
  struct raw_element_t : bp::empty_structs_optimiser<Displays, Functions> {
    using _ebase_t = bp::empty_structs_optimiser<Displays, Functions>;
    using _ebase_t::_ebase_t;

    constexpr decltype(auto) display() {
      return get<0>(*this);
    }
    constexpr decltype(auto) functions() {
      return get<1>(*this);
    }
  };
  std::vector<raw_element_t> elements_;
  int element_size_{128};

  constexpr int element_xy(std::size_t index) const {
    return element_size_ * static_cast<int>(index);
  }

  constexpr default_rect element_area(std::size_t index, auto w) const {
    CGUI_ASSERT(index < size(elements_));
    auto start_y = element_xy(index);
    return {{0, start_y}, {w, start_y + element_size_}};
  }

  constexpr _base_t &base() { return static_cast<_base_t &>(*this); }

public:
  class ref_element_t {
    uni_sized_widget_list_impl *container_;
    std::size_t index_;

  public:
    constexpr ref_element_t(uni_sized_widget_list_impl &c, std::size_t i)
        : container_(&c), index_(i) {}

    constexpr void set_state(auto &&, widget_back_propagater auto &&b) const {
      b.rerender();
    }
  };

  using _base_t::_base_t;

  template <typename T, typename BP>
    requires(has_handle<Functions, T, BP>)
  constexpr void handle(T &&t, BP &&b) {
    call::handle(get<1>(base()), std::forward<T>(t), std::forward<BP>(b));
  }
  constexpr void render(renderer auto &&r_org, auto &&b_arg) const {
    for (std::ptrdiff_t i = {}; auto &e : elements_) {
      auto const iu = static_cast<std::size_t>(i);
      auto w_arg = widget_render_args(element_area(0, call::width(b_arg)),
                                      b_arg.button_state(i));
      auto r = r_org.sub(element_area(iu, call::width(b_arg)));
      unused(r);
      call::for_each(e.get_first(),
                     [&](auto &&v) { call::render(v, r, w_arg); });
      ++i;
    }
  }
  constexpr auto sub_accessor(std::size_t index) const {
    CGUI_ASSERT(index < size(elements_));
    return [index](bp::cvref_type<uni_sized_widget_list_impl> auto &&self,
                   auto &&cb) {
      CGUI_ASSERT(index < size(self.elements_));
      cb(ref_element_t(self, index));
    };
  }
  constexpr auto sub_accessor(std::size_t index, auto &&) const {
    return sub_accessor(index);
  }
  constexpr decltype(auto) display_prototype() const
  {
    return this->get_first();
  }
  constexpr decltype(auto) function_prototype() const {
    return get<1>(static_cast<_base_t const&>(*this));
  }


  constexpr bool find_sub(std::predicate<ref_element_t, std::size_t> auto &&p,
                          std::invocable<ref_element_t, std::size_t> auto &&e) {
    return call::find_sub(
        std::views::transform(elements_,
                              [this](auto &e) {
                                return ref_element_t(
                                    *this,
                                    static_cast<std::size_t>(
                                        std::distance(elements_.data(), &e)));
                              }),
        std::forward<decltype(p)>(p), std::forward<decltype(e)>(e));
  }
  constexpr bool
  find_sub_at_location(pixel_coord auto &&pos,
                       std::invocable<ref_element_t, std::size_t> auto &&e) {
    if (element_size_ == 0) {
      return false;
    }
    auto pos_index = static_cast<std::size_t>(call::y_of(pos) / element_size_);
    if (pos_index < size(elements_)) {
      std::invoke(std::forward<decltype(e)>(e), ref_element_t(*this, pos_index),
                  pos_index);
      return true;
    }
    return false;
  }
  constexpr auto &list() { return elements_; }
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
template <typename T, typename... Ts>
concept is_list_function =
    (has_handle<T, Ts, basic_widget_back_propagater<>> && ...);
template <typename... Ts> struct list_function_constraint {
  template <is_list_function<Ts...> T> constexpr void operator()(T &&) const {}
};
template <typename Displays, typename Functions>
class uni_sized_widget_list_builder_impl
    : bp::empty_structs_optimiser<Displays, Functions> {
  using _base_t = bp::empty_structs_optimiser<Displays, Functions>;

  constexpr _base_t &&moved_base() noexcept {
    return static_cast<_base_t &&>(*this);
  }

public:
  using _base_t::_base_t;

  template <typename... T2s,
            typename ResG = build::args_to_group_t<
                widget_list_builder_constraint<>, T2s...>,
            typename ResT = uni_sized_widget_list_builder_impl<ResG, Functions>>
  constexpr ResT displays(T2s &&...ds) && {
    static_assert(
        (std::invocable<widget_list_builder_constraint<>, T2s> && ...));
    return ResT(build::args_to_group(widget_list_builder_constraint{},
                                     std::forward<T2s>(ds)...));
  }

  template <typename State, State... states, typename... Triggers,
            typename Marker = widget_state_marker<State, states...>,
            typename ResT = uni_sized_widget_list_impl<
                build::build_group_t<widget_list_constraint<Marker>, Displays,
                                     all_states_in_marker_t<Marker>>,
                build::build_group_t<list_function_constraint<Triggers...>,
                                     Functions, triggers<Triggers...>>>>
  constexpr ResT build(widget_states<State, states...>,
                       triggers<Triggers...> trigs) {
    return ResT(build::build_group(widget_list_constraint{},
                                   get<0>(moved_base()),
                                   all_states_in_marker_t<Marker>{}),
                build::build_group(list_function_constraint<Triggers...>{},
                                   get<1>(moved_base()), trigs));
  }
};

template <typename TriggerTuple> class std_function_list {
  static constexpr auto sz = std::tuple_size_v<TriggerTuple>;
  using arr_t = std::array<std::function<void()>, sz>;
  arr_t fs_;

  template <typename T>
  static constexpr bool valid_tag =
      requires() { bp::tuple_element_index_v<T, TriggerTuple>; };

public:
  std_function_list() { std::ranges::fill(fs_, bp::no_op); }
  template <typename T>
    requires(valid_tag<T>)
  constexpr std::function<void()> &get(T const &) {
    return fs_[bp::tuple_element_index_v<T, TriggerTuple>];
  }
  template <typename T, typename BP>
    requires(valid_tag<T>)
  void handle(T const &t, BP &&) {
    get(t)();
  }
};
struct std_function_list_builder {
  template <typename... Triggers>
  constexpr is_list_function<Triggers...> auto build(triggers<Triggers...>) && {
    return std_function_list<std::tuple<Triggers...>>();
  }
};

constexpr uni_sized_widget_list_builder_impl<std::tuple<>,
                                             std_function_list_builder>
uni_sized_widget_list_builder() {
  return {};
}
} // namespace cgui::dynamic

#endif // COMPONENT_GUI_DYNAMIC_HPP


#ifndef COMPONENT_GUI_CGUI_UI_EVENTS_HPP
#define COMPONENT_GUI_CGUI_UI_EVENTS_HPP

#include <bitset>
#include <chrono>
#include <tuple>

#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/type_traits.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui {

enum class input_events {
  system,
  mouse_move,
  mouse_button_down,
  mouse_button_up,
  mouse_exit,
  window_resized,
  touch_finger_down,
  touch_finger_up,
  touch_finger_move,
  time,
};

enum class interpreted_events {
  primary_click,
  context_menu_click,
};

class input_event_interpreter;
template <typename EventCategory, EventCategory evt_val, typename F>
class event_case_t;
template <typename Data, typename... Fs> class ui_event_switch_t;
template <interpreted_events> struct is_interpreted_event_impl;

template <input_events tEvt> struct input_event_identity {
  static constexpr input_events value = tEvt;
};

template <input_events> struct ui_event_constraints {
  template <typename> static constexpr bool type_passes = true;
};
template <> struct ui_event_constraints<input_events::mouse_button_down> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> pixel_coord;
    call::mouse_button(t);
  };
};
template <> struct ui_event_constraints<input_events::mouse_button_up> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> pixel_coord;
    call::mouse_button(t);
  };
};
template <> struct ui_event_constraints<input_events::mouse_move> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> pixel_coord;
  };
};
template <> struct ui_event_constraints<input_events::window_resized> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::size_of(t) } -> size_wh;
  };
};

template <typename T, input_events tEvt>
concept ui_event_c = ui_event_constraints<tEvt>::template type_passes<T>;

/// Return type for event deduction function. Use template parameter to indicate
/// what events a backend event *could* be, while using the return value for
/// what it actually *is*.
/// \tparam evts CGUI event types that an event could be.
template <input_events... evts> struct subset_ui_events {
  static_assert(sizeof...(evts) > 0, "You must at least specify 1 event type");
  input_events val;
  constexpr explicit(false) subset_ui_events(input_events v) noexcept : val(v) {
    CGUI_ASSERT(((v == evts) || ...));
  }
  constexpr explicit(false) operator input_events() const noexcept {
    return val;
  }

  template <input_events tEvt>
  static consteval bool can_be_event(input_event_identity<tEvt>) noexcept {
    return ((tEvt == evts) || ...);
  }
};

/// Single event optimisation specialisation, that lacks any member field, and
/// also does not need any input arguments.
/// \tparam evt Event type that this particular subset is.
template <input_events evt> struct subset_ui_events<evt> {
  constexpr explicit(false) subset_ui_events(input_events v) noexcept {
    assert(v == evt);
    unused(v);
  }
  constexpr subset_ui_events() noexcept = default;
  constexpr explicit(false) operator input_events() const noexcept {
    return evt;
  }

  template <input_events tEvt>
  static consteval bool can_be_event(input_event_identity<tEvt> = {}) noexcept {
    return (tEvt == evt);
  }
};

template <typename T>
concept subset_ui_event_c = std::convertible_to<T, input_events> && requires() {
  {
    std::remove_cvref_t<T>::can_be_event(
        input_event_identity<input_events::system>{})
  } -> std::convertible_to<bool>;
  {
    std::remove_cvref_t<T>::can_be_event(
        input_event_identity<input_events::mouse_move>{})
  } -> std::convertible_to<bool>;
  {
    std::remove_cvref_t<T>::can_be_event(
        input_event_identity<input_events::mouse_button_up>{})
  } -> std::convertible_to<bool>;
  {
    std::remove_cvref_t<T>::can_be_event(
        input_event_identity<input_events::mouse_button_down>{})
  } -> std::convertible_to<bool>;
};

namespace call {
namespace impl {
struct do_event_type {
  template <typename T>
    requires(requires(T &&t) {
      { _do_event_type{}(std::forward<T>(t)) } -> subset_ui_event_c;
    })
  constexpr subset_ui_event_c auto operator()(T &&t) const {
    return _do_event_type{}(std::forward<T>(t));
  }
};
} // namespace impl
inline constexpr impl::do_event_type event_type;
} // namespace call

template <typename T>
concept has_event_type =
    requires(bp::as_forward<T> t) { call::event_type(*t); };

template <input_events tEvt, typename T> consteval bool can_be_event() {
  if constexpr (has_event_type<T>) {
    using subset_t =
        std::remove_cvref_t<decltype(call::event_type(std::declval<T &&>()))>;
    return subset_t::can_be_event(input_event_identity<tEvt>{});
  } else {
    return false;
  }
}
template <input_events tEvt, typename T, typename... Unused> constexpr bool is_event(T &&evt, Unused&&...) {
  if constexpr (can_be_event<tEvt, T>()) {
    return static_cast<input_events>(call::event_type(evt)) == tEvt;
  } else {
    unused(evt);
    return false;
  }
}

template <interpreted_events enum_v, typename T>
constexpr bool is_event(T const &evt, input_event_interpreter const &i) {
  return is_interpreted_event_impl<enum_v>::event_switch()(i, evt);
}

template <typename T, input_events... tEvents>
concept event_types = (can_be_event<tEvents, T>() || ...);

template <input_events> struct dummy_event;

template <input_events tEvt>
constexpr subset_ui_events<tEvt> event_type(dummy_event<tEvt> const &) {
  return {};
}

template <> struct dummy_event<input_events::system> {};
template <> struct dummy_event<input_events::time> {
  std::chrono::nanoseconds delta_time{};
};
template <> struct dummy_event<input_events::mouse_exit> {};
template <> struct dummy_event<input_events::mouse_move> {
  default_pixel_coord pos{};
};
template <> struct dummy_event<input_events::mouse_button_down> {
  default_pixel_coord pos{};
  mouse_buttons button_id = mouse_buttons::primary;
};
template <> struct dummy_event<input_events::mouse_button_up> {
  default_pixel_coord pos{};
  mouse_buttons button_id = mouse_buttons::primary;
};
template <> struct dummy_event<input_events::window_resized> {
  default_size_wh sz{};
};
template <> struct dummy_event<input_events::touch_finger_move> {
  default_pixel_coord pos{};
  int finger_index{};
};
template <> struct dummy_event<input_events::touch_finger_down> {
  default_pixel_coord pos{};
  mouse_buttons button_id = mouse_buttons::primary;
  int finger_index{};
};
template <> struct dummy_event<input_events::touch_finger_up> {
  default_pixel_coord pos{};
  mouse_buttons button_id = mouse_buttons::primary;
  int finger_index{};
};

template <typename> constexpr bool is_dummy_event_v = false;
template <input_events tEvt>
constexpr bool is_dummy_event_v<dummy_event<tEvt>> = true;

template <typename T>
concept is_dummy_event_c = is_dummy_event_v<T>;

template <is_dummy_event_c T>
  requires(requires(T const &t) { t.pos; })
constexpr auto position(T const &t) {
  return t.pos;
}

template <is_dummy_event_c T>
  requires(requires(T const &t) { t.button_id; })
constexpr mouse_buttons mouse_button(T const &t) {
  return t.button_id;
}
template <is_dummy_event_c T>
  requires(requires(T const &t) { t.sz; })
constexpr auto size_of(T const &t) {
  return t.sz;
}
template <is_dummy_event_c T>
  requires(requires(T const &t) { t.finger_index; })
constexpr auto finger_index(T const &t) {
  return t.finger_index;
}
constexpr auto delta_time(is_dummy_event_c auto const &t)
  requires requires() { t.delta_time; }
{
  return t.delta_time;
}

using dummy_mouse_move_event = dummy_event<input_events::mouse_move>;
using dummy_mouse_down_event = dummy_event<input_events::mouse_button_down>;
using dummy_mouse_up_event = dummy_event<input_events::mouse_button_up>;
using dummy_mouse_exit_event = dummy_event<input_events::mouse_exit>;
using dummy_window_resized_event = dummy_event<input_events::window_resized>;
using dummy_touch_finger_down_event =
    dummy_event<input_events::touch_finger_down>;
using dummy_touch_finger_up_event = dummy_event<input_events::touch_finger_up>;
using dummy_touch_finger_move_event =
    dummy_event<input_events::touch_finger_move>;
using dummy_time_event = dummy_event<input_events::time>;

struct cgui_mouse_exit_event {
  static constexpr subset_ui_events<input_events::mouse_exit>
  event_type(auto &&) {
    return {};
  }
};

namespace impl {
template <typename F, typename... Args>
constexpr bool event_case_invoke(F &&f, Args &&...args) {
  using result_t = std::invoke_result_t<F, Args...>;
  if constexpr (std::is_void_v<result_t>) {
    std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
    return true;
  } else {
    static_assert(std::is_convertible_v<result_t, bool>);
    return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
  }
}
template <typename T, typename... Ts>
constexpr T&& first_of_pack_or_empty(T&& in, Ts&&...) {
  return std::forward<T>(in);
}
constexpr std::type_identity<void> first_of_pack_or_empty() {
  return {};
}
} // namespace impl

template <typename, typename> struct _impl_has_case_for;

template <typename Data, input_events... evts, typename... Fs, typename Evt>
struct _impl_has_case_for<
    ui_event_switch_t<Data, event_case_t<input_events, evts, Fs>...>, Evt> {
  static constexpr bool value = (can_be_event<evts, Evt>() || ...);
};

template <typename SwitchCase, typename Evt>
static constexpr bool _impl_has_case_for_v =
    _impl_has_case_for<SwitchCase, Evt>::value;

template <interpreted_events evt_v, typename Evt>
consteval bool can_be_event() {
  using impl_switch_t =
      decltype(is_interpreted_event_impl<evt_v>::event_switch());
  return _impl_has_case_for_v<impl_switch_t, Evt>;
}

template <typename EventCategory, EventCategory evt_val, typename F>
class event_case_t : bp::empty_structs_optimiser<F> {
  template <typename Evt, typename... Ts>
  static constexpr bool valid_function =
      std::invocable<F, Evt &&, Ts &&...> || std::invocable<F, Evt &&> ||
      std::invocable<F>;

public:
  using bp::empty_structs_optimiser<F>::empty_structs_optimiser;

  template <typename Evt, typename... Ts>
    requires(can_be_event<evt_val, Evt>() && valid_function<Evt, Ts...>)
  constexpr bool operator()(Evt &&e, Ts &&...args) {
    if (is_event<evt_val>(e, impl::first_of_pack_or_empty(args...))) {
      if constexpr (std::invocable<F, Evt &&, Ts &&...>) {
        return impl::event_case_invoke(this->get_first(), std::forward<Evt>(e),
                                       std::forward<Ts>(args)...);
      } else if constexpr (std::invocable<F, Evt &&>) {
        return impl::event_case_invoke(this->get_first(), std::forward<Evt>(e));
      } else {
        static_assert(std::invocable<F>, "Bad signature of function F");
        unused(e, args...);
        return impl::event_case_invoke(this->get_first());
      }
    }
    return false;
  }
  template <typename Evt, typename... Ts>
    requires(!can_be_event<evt_val, Evt>())
  constexpr bool operator()(Evt &&, Ts &&...) {
    return false;
  }
};
template <input_events evt_v, typename T>
constexpr event_case_t<input_events, evt_v, std::remove_cvref_t<T>>
event_case(T &&in) {
  return event_case_t<input_events, evt_v, std::remove_cvref_t<T>>{
      std::forward<T>(in)};
}
template <interpreted_events evt_v, typename T>
constexpr event_case_t<interpreted_events, evt_v, std::remove_cvref_t<T>>
event_case(T &&in) {
  return event_case_t<interpreted_events, evt_v, std::remove_cvref_t<T>>{
      std::forward<T>(in)};
}

namespace impl {
template <typename> constexpr bool is_event_case_raw = false;
template <typename EventCategory, EventCategory e, typename T>
constexpr bool is_event_case_raw<event_case_t<EventCategory, e, T>> = true;
template <> constexpr bool is_event_case_raw<input_event_interpreter> = true;
template <typename T>
constexpr bool is_event_case = is_event_case_raw<std::remove_cvref_t<T>>;
template <typename...> constexpr bool is_only_input_events = false;
template <input_events... evts, typename... Ts>
constexpr bool is_only_input_events<event_case_t<input_events, evts, Ts>...> =
    true;

template <typename... Ts> struct first_type_or_void;
template <typename T, typename... Ts> struct first_type_or_void<T, Ts...> {
  using type = T;
};
template <> struct first_type_or_void<> {
  using type = void;
};
template <typename... Ts>
using first_type_or_void_t = typename first_type_or_void<Ts...>::type;
} // namespace impl

template <has_event_type Evt, typename Data, input_events... evt_vs,
          typename... Fs>
  requires(bp::is_unique(evt_vs...) && !impl::is_event_case<Data> &&
           ((std::invocable<Fs> ||
             std::invocable<Fs, dummy_event<evt_vs> const &> ||
             std::invocable<Fs, dummy_event<evt_vs> const &, Data &&>) &&
            ...))
constexpr bool
ui_event_switch(Evt &&e, Data &&d,
                event_case_t<input_events, evt_vs, Fs> &&...cases) {
  // The forwarding operator is fine to use hre since the cases leaves both e
  // and d untouched if the case does not correspond to the correct event.
  return (cases(std::forward<Evt>(e), std::forward<Data>(d)) || ...);
}
template <has_event_type Evt, input_events... evt_vs,
          bp::invocable_or_invocable_args<dummy_event<evt_vs> const &>... Fs>
  requires(bp::is_unique(evt_vs...))
constexpr bool
ui_event_switch(Evt &&e, event_case_t<input_events, evt_vs, Fs> &&...cases) {
  // The forwarding operator is fine to use hre since the cases leaves both e
  // and d untouched if the case does not correspond to the correct event.
  return (cases(std::forward<Evt>(e)) || ...);
}

template <typename Data, typename... Fs>
class ui_event_switch_t : bp::empty_structs_optimiser<Data, Fs...> {
  using _base_t = bp::empty_structs_optimiser<Data, Fs...>;
  static constexpr auto &&to_base(auto &&self) {
    using t = bp::copy_cvref_t<_base_t &, decltype(self)>;
    return static_cast<t>(self);
  }

public:
  using _base_t::_base_t;
  static constexpr bool call(bp::cvref_type<ui_event_switch_t> auto &&self,
                             auto &&evt, auto &&...args)
    requires(((std::invocable<Fs, decltype(evt)> ||
                   std::invocable<Fs, decltype(evt), Data &&> ||
                   std::invocable < Fs,
               decltype(evt), Data &&, decltype(args)... >) &&
              ...) &&
             (impl::is_only_input_events<Fs...> ||
              bp::cvref_type<impl::first_type_or_void_t<decltype(args)...>,
                             input_event_interpreter>))
  {
    using evt_t = decltype(evt);
    auto sf = bp::as_forward<decltype(self)>(self);
    auto ef = bp::as_forward<evt_t>(evt);
    if constexpr (sizeof...(args) == 0) {
      return call::apply_to(to_base(*sf), [ef](auto &&data, auto &&...cases) {
        return ui_event_switch(*ef, std::forward<decltype(data)>(data),
                               std::forward<decltype(cases)>(cases)...);
      });
    } else {
      auto case_caller = [ef, args_tuple = std::forward_as_tuple(
                                  args...)]<typename Case>(Data &&d, Case &&c) {
        auto cf = bp::as_forward<Case>(c);
        auto df = bp::as_forward<Data>(d);
        if constexpr (std::invocable<Case, evt_t>) {
          return std::invoke(*cf, *ef);
        } else if constexpr (std::invocable<Case, evt_t, Data &&>) {
          return std::invoke(*cf, *ef, *df);
        } else {
          // The following line may introduce trouble on some versions of
          // MSVC... static_assert(std::invocable<Case, evt_t, Data&&,
          // decltype(args)...>);
          return std::apply(
              [&]<typename... Ts2>(Ts2 &&...args) {
                return std::invoke(*cf, *ef, *df, std::forward<Ts2>(args)...);
              },
              args_tuple);
        }
      };
      return call::apply_to(to_base(*sf), [&case_caller]<typename... Cases>(
                                               Data &&d, Cases &&...cs) {
        return (case_caller(std::forward<Data>(d), std::forward<Cases>(cs)) ||
                ...);
      });
    }
  }

  constexpr bool operator()(auto &&evt, auto &&...args) & {
    return call(*this, std::forward<decltype(evt)>(evt),
                std::forward<decltype(args)>(args)...);
  }
  constexpr bool operator()(auto &&evt, auto &&...args) const & {
    return call(*this, std::forward<decltype(evt)>(evt),
                std::forward<decltype(args)>(args)...);
  }
  constexpr bool operator()(auto &&evt, auto &&...args) && {
    return call(std::move(*this), std::forward<decltype(evt)>(evt),
                std::forward<decltype(args)>(args)...);
  }
  constexpr bool operator()(auto &&evt, auto &&...args) const && {
    return call(std::move(*this), std::forward<decltype(evt)>(evt),
                std::forward<decltype(args)>(args)...);
  }
};
template <typename Data, typename... EventCategory, EventCategory... evt_vs,
          typename... Fs>
constexpr ui_event_switch_t<std::unwrap_ref_decay_t<Data>,
                            event_case_t<input_events, evt_vs, Fs>...>
saved_ui_event_switch(Data &&d,
                      event_case_t<EventCategory, evt_vs, Fs> &&...cases) {
  return {std::forward<Data>(d), std::move(cases)...};
}

template <typename Event> class interpreted_event {
  Event const *evt_;
  input_event_interpreter const *interpreter_;

public:
  constexpr interpreted_event(Event &e, input_event_interpreter &i) noexcept
      : evt_(&e), interpreter_(&i) {}
  constexpr decltype(auto) event() const { return this->get_first(); }
  constexpr input_event_interpreter const &interpreter() const noexcept {
    return *interpreter_;
  }
#define CGUI_EVT_METHOD_(NAME)                                                 \
  template <typename... Ts>                                                    \
    requires(std::invocable<decltype(call::NAME), Event const &, Ts...>)       \
  constexpr decltype(auto) NAME(Ts &&...args) {                                \
    return call::NAME(event(), std::forward<decltype(args)>(args)...);         \
  }

#undef CGUI_EVT_METHOD_
};

struct interpreter_configuration {
  std::chrono::nanoseconds touch_context_menu_hold_ =
      std::chrono::milliseconds(500);
};

class input_event_interpreter {
  interpreter_configuration conf_{};
  std::chrono::nanoseconds current_time_{};
  std::chrono::nanoseconds touch_start_{};
  int touch_first_index_{};
  std::bitset<16> touch_down_{};
  constexpr auto event_switch() {
    return saved_ui_event_switch(
        std::ref(*this),
        event_case<input_events::time>(
            [](auto const &e, input_event_interpreter &self) {
              self.current_time_ += call::delta_time(e);
            }),
        event_case<input_events::touch_finger_down>(
            [](auto const &e, input_event_interpreter &self) {
              CGUI_ASSERT(call::finger_index(e) < self.touch_down_.size());
              if (self.touch_count() == 0) {
                self.touch_first_index_ = call::finger_index(e);
                self.touch_start_ = self.current_time_;
              }
              self.touch_down_.set(call::finger_index(e), true);
            }));
  }

  constexpr std::size_t touch_count() const noexcept {
    return touch_down_.count();
  }

public:
  template <typename Evt> constexpr void update(Evt const &e) {
    event_switch()(e);
  }

  constexpr bool is_touch_context_menu(int index) const {
    CGUI_DEBUG_ONLY(auto a1 = index == touch_first_index_;)
    CGUI_DEBUG_ONLY(auto a2 = touch_count() == 1;)
    CGUI_DEBUG_ONLY(auto a3 = conf_.touch_context_menu_hold_ <
                              (current_time_ - touch_start_);)
    CGUI_DEBUG_ONLY(unused(a1, a2, a3);)
    return (index == touch_first_index_) && (touch_count() == 1) &&
           (conf_.touch_context_menu_hold_ < (current_time_ - touch_start_));
  }
};

template <typename T>
concept is_interpreted_event = requires(T const &t) {
  { t.interpreter() } -> std::convertible_to<input_event_interpreter const &>;
};

template <>
struct is_interpreted_event_impl<interpreted_events::primary_click> {
  static constexpr auto event_switch() {
    return saved_ui_event_switch(
        empty_state{},
        event_case<input_events::mouse_button_up>(
            [](auto const &e, auto const &) {
              return call::mouse_button(e) == mouse_buttons::primary;
            }),
        event_case<input_events::touch_finger_up>(
            [](auto const &e, auto const &i) {
              return !i.is_touch_context_menu(call::finger_index(e));
            }));
  }
};
template <>
struct is_interpreted_event_impl<interpreted_events::context_menu_click> {
  static constexpr auto event_switch() {
    using enum input_events;
    return saved_ui_event_switch(
        empty_state{},
        event_case<mouse_button_up>([](auto const &e, auto &&...) {
          return call::mouse_button(e) == mouse_buttons::secondary;
        }),
        event_case<input_events::touch_finger_up>(
            [](auto const &e, auto const &i) {
              return i.is_touch_context_menu(call::finger_index(e));
            }));
  }
};

template <has_event_type Evt, bp::cvref_type<input_event_interpreter> Interp,
          typename Data, typename... EventCategory, EventCategory... evt_vs,
          typename... Fs>
  requires(bp::is_unique(evt_vs...) && !impl::is_event_case<Data>)
constexpr bool
ui_event_switch(Evt &&e, Interp &i, Data &&d,
                event_case_t<EventCategory, evt_vs, Fs> &&...cases) {
  // The forwarding operator is fine to use hre since the cases leaves both e
  // and d untouched if the case does not correspond to the correct event.
  return (cases(std::forward<Evt>(e), i, std::forward<Data>(d)) || ...);
}
template <has_event_type Evt, bp::cvref_type<input_event_interpreter> Interp,
          typename... EventCategory, EventCategory... evt_vs, typename... Fs>
  requires(bp::is_unique(evt_vs...))
constexpr bool
ui_event_switch(Evt &&e, Interp &i,
                event_case_t<EventCategory, evt_vs, Fs> &&...cases) {
  // The forwarding operator is fine to use hre since the cases leaves both e
  // and d untouched if the case does not correspond to the correct event.
  return (cases(std::forward<Evt>(e), i) || ...);
}

} // namespace cgui

#endif

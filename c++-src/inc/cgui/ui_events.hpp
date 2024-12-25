
#ifndef COMPONENT_GUI_CGUI_UI_EVENTS_HPP
#define COMPONENT_GUI_CGUI_UI_EVENTS_HPP

#include <chrono>
#include <cmath>
#include <tuple>
#include <variant>

#include "cgui-call.hpp"
#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui {

/// Codes representing physical keys on a keyboard, regardless of localisation
/// and layout.
/// Values taken from USB standard:
/// https://usb.org/sites/default/files/hut1_5.pdf
enum class keycode {
  unknown = 0,
  lctrl = 224,
  rctrl = 228,
};

enum class input_events {
  system,
  window_resized,
  mouse_move,
  mouse_button_down,
  mouse_button_up,
  mouse_exit,
  mouse_scroll,
  key_down,
  key_up,
  touch_down,
  touch_up,
  touch_move,
};

enum class interpreted_events {
  primary_click = 1,
  context_menu_click,
  pointer_drag_start,
  pointer_drag_move,
  pointer_drag_finished_destination,
  pointer_drag_finished_source,
  pointer_hover,
  pointer_hold,
  pointer_enter,
  pointer_exit,
  scroll,
  zoom
};

template <typename EventEnum, EventEnum val> struct event_identity {
  static constexpr EventEnum value = val;
};
template <input_events tEvt>
using input_event_identity = event_identity<input_events, tEvt>;
template <interpreted_events tEvt>
using interpreted_event_identity = event_identity<interpreted_events, tEvt>;

template <input_events> struct input_event_constraints {
  template <typename> static constexpr bool type_passes = true;
};
template <> struct input_event_constraints<input_events::mouse_button_down> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> point_coordinate;
    call::mouse_button(t);
  };
};
template <> struct input_event_constraints<input_events::mouse_button_up> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> point_coordinate;
    call::mouse_button(t);
  };
};
template <> struct input_event_constraints<input_events::mouse_move> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> point_coordinate;
  };
};
template <> struct input_event_constraints<input_events::key_down> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::raw_key(t) } -> std::convertible_to<keycode>;
  };
};
template <> struct input_event_constraints<input_events::key_up> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::raw_key(t) } -> std::convertible_to<keycode>;
  };
};
template <> struct input_event_constraints<input_events::window_resized> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::size_of(t) } -> point_size_wh;
  };
};
template <> struct input_event_constraints<input_events::touch_down> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> point_coordinate;
    { call::finger_index(t) } -> std::convertible_to<int>;
  };
};
template <> struct input_event_constraints<input_events::touch_up> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> point_coordinate;
    { call::finger_index(t) } -> std::convertible_to<int>;
  };
};
template <> struct input_event_constraints<input_events::touch_move> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> point_coordinate;
    { call::finger_index(t) } -> std::convertible_to<int>;
  };
};

template <typename T>
concept has_time_stamp = requires(T const &t) { call::time_stamp(t); };

template <typename T, input_events tEvt>
concept input_event_c = input_event_constraints<tEvt>::template type_passes<T>;

/// Return type for event deduction function. Use template parameter to indicate
/// what events a backend event *could* be, while using the return value for
/// what it actually *is*.
/// \tparam evts CGUI event types that an event could be.
template <typename EventEnum, EventEnum... evts> struct subset_events {
  static_assert(sizeof...(evts) > 0, "You must at least specify 1 event type");
  EventEnum val;
  constexpr explicit(false) subset_events(EventEnum v) noexcept : val(v) {
    CGUI_ASSERT(((v == evts) || ...));
  }
  constexpr explicit(false) operator EventEnum() const noexcept { return val; }

  template <EventEnum tEvt>
  static consteval bool
  can_be_event(event_identity<EventEnum, tEvt> = {}) noexcept {
    return ((tEvt == evts) || ...);
  }
};

/// Single event optimisation specialisation, that lacks any member field, and
/// also does not need any input arguments.
/// \tparam evt Event type that this particular subset is.
template <typename EventEnum, EventEnum evt>
struct subset_events<EventEnum, evt> {
  constexpr explicit(false) subset_events(EventEnum v) noexcept {
    assert(v == evt);
    unused(v);
  }
  constexpr subset_events() noexcept = default;
  constexpr explicit(false) operator EventEnum() const noexcept { return evt; }

  template <EventEnum tEvt>
  static consteval bool
  can_be_event(event_identity<EventEnum, tEvt> = {}) noexcept {
    return (tEvt == evt);
  }
};

template <typename T, typename EventType>
concept has_can_be_event = requires() {
  std::remove_cvref_t<T>::template can_be_event<EventType{}>();
  std::remove_cvref_t<T>::can_be_event(
      event_identity<EventType, EventType{}>());
};

template <input_events... evts>
using subset_input_events = subset_events<input_events, evts...>;
template <interpreted_events... evts>
using subset_interpreted_events = subset_events<interpreted_events, evts...>;

template <typename T>
concept subset_input_event_c = std::convertible_to<T, input_events> &&
  requires()
{
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
template <typename T>
concept subset_interpreted_event_c =
    std::convertible_to<T, interpreted_events> &&
  requires()
{
  {
    std::remove_cvref_t<T>::can_be_event(
        interpreted_event_identity<interpreted_events::primary_click>{})
  } -> std::convertible_to<bool>;
};

namespace call {
namespace impl {
struct do_event_type {
  template <typename T>
    requires(requires(T &&t) {
      { _do_event_type{}(std::forward<T>(t)) } -> subset_input_event_c;
    })
  constexpr subset_input_event_c auto operator()(T &&t) const {
    return _do_event_type{}(std::forward<T>(t));
  }
  template <typename T>
    requires(requires(T &&t) {
      { _do_event_type{}(std::forward<T>(t)) } -> subset_interpreted_event_c;
    })
  constexpr subset_interpreted_event_c auto operator()(T &&t) const {
    return _do_event_type{}(std::forward<T>(t));
  }
};
} // namespace impl
inline constexpr impl::do_event_type event_type;
} // namespace call

template <typename T>
concept has_event_type =
    requires(bp::as_forward<T> t) { call::event_type(*t); };
template <typename T, typename EventType>
concept has_event_type_of_type =
    has_event_type<T> && requires(bp::as_forward<T> t) {
      { call::event_type(*t) } -> has_can_be_event<EventType>;
    };

template <input_events tEvt, typename T> consteval bool can_be_event() {
  if constexpr (has_event_type_of_type<T, input_events>) {
    using subset_t =
        std::remove_cvref_t<decltype(call::event_type(std::declval<T &&>()))>;
    return subset_t::can_be_event(input_event_identity<tEvt>{});
  } else {
    return false;
  }
}
template <interpreted_events tEvt, typename T> consteval bool can_be_event() {
  if constexpr (has_event_type_of_type<T, interpreted_events>) {
    using subset_t =
        std::remove_cvref_t<decltype(call::event_type(std::declval<T &&>()))>;
    return subset_t::can_be_event(interpreted_event_identity<tEvt>{});
  } else {
    return false;
  }
}
template <input_events tEvt, typename T> constexpr bool is_event(T &&evt) {
  if constexpr (can_be_event<tEvt, T>()) {
    return static_cast<input_events>(call::event_type(evt)) == tEvt;
  } else {
    unused(evt);
    return false;
  }
}
template <interpreted_events tEvt, typename T>
constexpr bool is_event(T &&evt) {
  if constexpr (can_be_event<tEvt, T>()) {
    return static_cast<interpreted_events>(call::event_type(evt)) == tEvt;
  } else {
    unused(evt);
    return false;
  }
}

template <typename T, input_events... tEvents>
concept event_types = (can_be_event<tEvents, T>() || ...);

struct common_event_data {
  std::chrono::steady_clock::time_point ts = std::chrono::steady_clock::now();
};
template <typename TimePoint> struct interpreted_event_basic {
  using time_point_t = TimePoint;
  time_point_t ts;

  constexpr explicit interpreted_event_basic(has_time_stamp auto const &e)
      : ts(call::time_stamp(e)) {}
  constexpr explicit interpreted_event_basic(auto const &tp)
    requires(std::constructible_from<time_point_t, decltype(tp)>)
      : ts(tp) {}
  constexpr interpreted_event_basic(interpreted_event_basic const &) noexcept(
      std::is_nothrow_copy_constructible_v<time_point_t>) = default;
  constexpr interpreted_event_basic &
  operator=(interpreted_event_basic const &) noexcept(
      std::is_nothrow_copy_assignable_v<time_point_t>) = default;
};

template <typename T>
concept any_interpreted_event_c = true;

template <input_events> struct default_event;

template <input_events tEvt>
constexpr subset_input_events<tEvt> event_type(default_event<tEvt> const &) {
  return {};
}

template <> struct default_event<input_events::system> {
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_exit> {
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_move> {
  default_point_coordinate pos{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_scroll> {
  default_point_coordinate pos{};
  float dx{};
  float dy{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_button_down> {
  default_point_coordinate pos{};
  mouse_buttons button_id = mouse_buttons::primary;
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_button_up> {
  default_point_coordinate pos{};
  mouse_buttons button_id = mouse_buttons::primary;
  common_event_data common_data{};
};
template <> struct default_event<input_events::key_down> {
  keycode rawkey{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::key_up> {
  keycode rawkey{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::touch_down> {
  default_point_coordinate pos{};
  int finger_index{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::touch_up> {
  default_point_coordinate pos{};
  int finger_index{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::touch_move> {
  default_point_coordinate pos{};
  int finger_index{};
  common_event_data common_data{};
};
template <> struct default_event<input_events::window_resized> {
  default_point_size_wh sz{};
  common_event_data common_data{};
};

template <typename> constexpr bool is_cgui_default_event_v = false;
template <input_events tEvt>
constexpr bool is_cgui_default_event_v<default_event<tEvt>> = true;

template <typename T>
concept is_cgui_default_event_c = is_cgui_default_event_v<T>;

template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.pos; })
constexpr auto position(T const &t) {
  return t.pos;
}
template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.finger_index; })
constexpr auto finger_index(T const &t) {
  return t.finger_index;
}

template <is_cgui_default_event_c T> constexpr auto time_stamp(T const &t) {
  return t.common_data.ts;
}

template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.button_id; })
constexpr mouse_buttons mouse_button(T const &t) {
  return t.button_id;
}
template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.sz; })
constexpr auto size_of(T const &t) {
  return t.sz;
}
template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.dx; })
constexpr auto delta_x(T const &t) {
  return t.dx;
}
template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.dy; })
constexpr auto delta_y(T const &t) {
  return t.dy;
}
template <is_cgui_default_event_c T>
  requires(requires(T const &t) { t.rawkey; })
constexpr keycode raw_key(T const &t) {
  return t.rawkey;
}

using default_mouse_move_event = default_event<input_events::mouse_move>;
using default_mouse_down_event = default_event<input_events::mouse_button_down>;
using default_mouse_up_event = default_event<input_events::mouse_button_up>;
using default_mouse_exit_event = default_event<input_events::mouse_exit>;
using default_mouse_scroll_event = default_event<input_events::mouse_scroll>;
using default_key_down_event = default_event<input_events::key_down>;
using default_key_up_event = default_event<input_events::key_up>;
using default_touch_down_event = default_event<input_events::touch_down>;
using default_touch_up_event = default_event<input_events::touch_up>;
using default_touch_move_event = default_event<input_events::touch_move>;
using default_window_resized_event =
    default_event<input_events::window_resized>;

template <typename T, interpreted_events... ie_vs>
concept interpreted_event_types = (can_be_event<ie_vs, T>() || ...);

template <interpreted_events> struct interpreted_event_impl;
template <interpreted_events ie_v,
          typename TimePoint = std::chrono::steady_clock::time_point>
struct interpreted_event : interpreted_event_impl<ie_v> {
  using _base_t = interpreted_event_impl<ie_v>;
  interpreted_event_basic<TimePoint> common_data;
  template <typename Evt>
    requires(std::constructible_from<_base_t, Evt> &&
             std::constructible_from<interpreted_event_basic<TimePoint>, Evt>)
  constexpr explicit interpreted_event(Evt const &evt)
      : _base_t(evt), common_data(evt) {}
  constexpr interpreted_event(_base_t const &impl, TimePoint const &ts)
      : _base_t(impl), common_data(ts) {}
  template <typename... Ts>
    requires(std::constructible_from<_base_t, Ts && ...>)
  constexpr explicit interpreted_event(TimePoint const &ts, Ts &&...args)
      : _base_t(std::forward<Ts>(args)...), common_data(ts) {}

  constexpr subset_interpreted_events<ie_v> event_type() const noexcept {
    return {};
  }
};

template <interpreted_events evt_val, typename TP>
constexpr subset_interpreted_events<evt_val>
event_type(interpreted_event<evt_val, TP> const &) {
  return {};
}

template <interpreted_events ie_v, typename C>
constexpr bool is_cgui_default_event_v<interpreted_event<ie_v, C>> = true;

template <interpreted_events ie_v>
inline constexpr auto create_interpreted_event_from_tp =
    []<typename TP, typename... Args>(TP const &time_stamp, Args &&...args) {
      using impl_t = interpreted_event_impl<ie_v>;
      return interpreted_event<ie_v, TP>(impl_t(std::forward<Args>(args)...),
                                         time_stamp);
    };
template <interpreted_events ie_v>
inline constexpr auto create_interpreted_event_from_event =
    []<has_time_stamp Evt, typename... Args>(Evt const &trig_event,
                                             Args &&...args) {
      return create_interpreted_event_from_tp<ie_v>(
          call::time_stamp(trig_event), std::forward<Args>(args)...);
    };

template <> struct interpreted_event_impl<interpreted_events::primary_click> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
};
template <>
struct interpreted_event_impl<interpreted_events::context_menu_click> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
};
template <>
struct interpreted_event_impl<interpreted_events::pointer_drag_start> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
};
template <>
struct interpreted_event_impl<interpreted_events::pointer_drag_move> {
  using position_t = default_point_coordinate;
  position_t start_pos{};
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &sp,
                                            point_coordinate auto const &cp)
      : start_pos(copy_coordinate<position_t>(sp)),
        pos(copy_coordinate<position_t>(cp)) {}
};
template <>
struct interpreted_event_impl<
    interpreted_events::pointer_drag_finished_source> {
  using position_t = default_point_coordinate;
  position_t start_pos{};
  position_t end_pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &sp,
                                            point_coordinate auto const &ep)
      : start_pos(copy_coordinate<position_t>(sp)),
        end_pos(copy_coordinate<position_t>(ep)) {}
};
template <>
struct interpreted_event_impl<
    interpreted_events::pointer_drag_finished_destination> {
  using position_t = default_point_coordinate;
  position_t start_pos{};
  position_t end_pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &sp,
                                            point_coordinate auto const &ep)
      : start_pos(copy_coordinate<position_t>(sp)),
        end_pos(copy_coordinate<position_t>(ep)) {}
};
template <> struct interpreted_event_impl<interpreted_events::pointer_hover> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
};
template <> struct interpreted_event_impl<interpreted_events::pointer_hold> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
};
template <> struct interpreted_event_impl<interpreted_events::pointer_enter> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
};
template <> struct interpreted_event_impl<interpreted_events::pointer_exit> {};
template <> struct interpreted_event_impl<interpreted_events::scroll> {
  using position_t = default_point_coordinate;
  position_t pos{};
  float dx{};
  float dy{};
  constexpr interpreted_event_impl(point_coordinate auto const &p, float dxi,
                                   float dyi)
      : pos(copy_coordinate<position_t>(p)), dx(dxi), dy(dyi) {}
};
template <> struct interpreted_event_impl<interpreted_events::zoom> {
  using position_t = default_point_coordinate;
  position_t pos{};
  float scale_x{}; ///> Number above 1. -> zoom in / make things bigger.
  float scale_y{}; ///> Number above 1. -> zoom in / make things bigger.
  constexpr interpreted_event_impl(point_coordinate auto const &p, float scx,
                                   float scy)
      : pos(copy_coordinate<position_t>(p)), scale_x(scx), scale_y(scy) {}
};

struct cgui_mouse_exit_event {
  static constexpr subset_input_events<input_events::mouse_exit>
  event_type(auto &&) {
    return {};
  }
};

template <typename Interpreter> struct state_interpreter_pair {
  using interpreter = Interpreter;
  using state_t = typename interpreter::state;
  state_t state;
  constexpr explicit(false)
      state_interpreter_pair(std::convertible_to<state_t> auto &&s)
      : state(std::forward<decltype(s)>(s)) {}
  static constexpr auto const &get_settings(auto &&t) noexcept {
    return std::get<typename interpreter::settings>(t);
  }
};
template <typename Interpreter>
constexpr auto
_to_state(state_interpreter_pair<Interpreter> *sip) -> decltype(&sip->state) {
  if (sip == nullptr) {
    return nullptr;
  } else {
    return &sip->state;
  }
}

template <typename WidgetT> struct query_result {
  WidgetT *w_{};

  constexpr explicit operator bool() const noexcept { return w_ != nullptr; }
};
template <typename Pred, typename OnFind, typename OnNoFind,
          interpreted_events... events>
struct query_interpreted_events_t {
  Pred p_;
  OnFind f_;
  OnNoFind no_f_;
  static_assert(
      noexcept(std::declval<OnNoFind>()()),
      "The OnNoFind should be noexcept as it is called from a destructor!");
  mutable bool any_found_{};

  constexpr query_interpreted_events_t(Pred p, OnFind f, OnNoFind no_f)
      : p_(std::move(p)), f_(std::move(f)), no_f_(std::move(no_f)) {}

  constexpr bool operator()(auto &in, auto &&s) const {
    if (p_(in)) {
      f_(in, std::forward<decltype(s)>(s));
      any_found_ = true;
      return true;
    }
    return false;
  }
  constexpr ~query_interpreted_events_t() {
    if (!any_found_) {
      no_f_();
    }
  }
};

template <interpreted_events... events, typename Pred, typename OnFind,
          typename OnNoFind = bp::no_op_t>
  requires(std::predicate<Pred, dummy_widget const &>)
constexpr query_interpreted_events_t<std::remove_cvref_t<Pred>,
                                     std::remove_cvref_t<OnFind>,
                                     std::remove_cvref_t<OnNoFind>, events...>
query_interpreted_events(Pred &&p, OnFind &&f, OnNoFind &&nf = {}) {
  return {std::forward<Pred>(p), std::forward<OnFind>(f),
          std::forward<OnNoFind>(nf)};
}

template <interpreted_events... events, typename OnFind,
          typename OnNoFind = bp::no_op_t>
constexpr auto query_interpreted_events(point_coordinate auto const &pos,
                                        OnFind &&f, OnNoFind &&nf = {}) {
  return query_interpreted_events<events...>(
      [pos](auto const &w) { return hit_box(call::area(w), pos); },
      std::forward<OnFind>(f), std::forward<OnNoFind>(nf));
}

template <typename TimePoint = typename std::chrono::steady_clock::time_point,
          template <typename> typename... Interpreters>
class event_interpreter
    : bp::empty_structs_optimiser<Interpreters<TimePoint>...> {
  using fickle_state_t =
      std::variant<empty_placeholder_t,
                   state_interpreter_pair<Interpreters<TimePoint>>...>;
  using time_point_t = TimePoint;

  template <typename Interpreter, typename Evt, typename Q>
  constexpr bool interpret(Evt &&e, Q &&q) {
    if constexpr (Interpreter::template can_handle<
                      std::remove_cvref_t<Evt>>()) {
      this->get(std::type_identity<Interpreter>())
          .handle(std::forward<Evt>(e), q);
    }
    return false;
  }

public:
  template <typename Evt, typename ToQuery>
  constexpr bool handle(Evt const &e, ToQuery &&q) {
    return (interpret<Interpreters<TimePoint>>(e, q) || ...);
  }
  template <std::convertible_to<time_point_t> TP, typename F>
  constexpr void pass_time(TP &&tp, F &&f) {
    unused(tp, f);
  }

  template <template <typename> typename Interpreter>
    requires(
        (std::is_same_v<Interpreter<TimePoint>, Interpreters<TimePoint>>) ||
        ...)
  friend constexpr auto &settings(event_interpreter &ei) {
    return ei.get(std::type_identity<Interpreter<TimePoint>>{}).get_settings();
  }
};

template <typename EventType, EventType evt_val, typename F>
class event_case_t : bp::empty_structs_optimiser<F> {
  template <typename Evt, typename... Ts>
  static constexpr bool valid_function =
      std::invocable<F, Evt &&, Ts &&...> || std::invocable<F, Evt &&> ||
      std::invocable<F>;

public:
  static constexpr EventType event_enum = evt_val;
  using bp::empty_structs_optimiser<F>::empty_structs_optimiser;

  template <typename Evt, typename... Ts>
    requires(can_be_event<evt_val, Evt>() && valid_function<Evt, Ts...>)
  constexpr bool operator()(Evt &&e, Ts &&...args) {
    if (is_event<evt_val>(e)) {
      if constexpr (std::invocable<F, Evt &&, Ts &&...>) {
        std::invoke(this->get_first(), std::forward<Evt>(e),
                    std::forward<Ts>(args)...);
      } else if constexpr (std::invocable<F, Evt &&>) {
        std::invoke(this->get_first(), std::forward<Evt>(e));
      } else {
        static_assert(std::invocable<F>, "Bad signature of function F");
        std::invoke(this->get_first());
        unused(e, args...);
      }
      return true;
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
template <typename EvtType, EvtType e, typename T>
constexpr bool is_event_case_raw<event_case_t<EvtType, e, T>> = true;
template <typename... Ts>
constexpr bool is_event_case =
    (is_event_case_raw<std::remove_cvref_t<Ts>> && ...);
} // namespace impl

template <has_event_type Evt, typename Data, typename... Cases>
  requires(bp::is_weakly_unique(std::remove_cvref_t<Cases>::event_enum...) &&
           !impl::is_event_case<Data> && impl::is_event_case<Cases...>)
constexpr bool ui_event_switch(Evt &&e, Data &&d, Cases &&...cases) {
  // The forwarding operator is fine to use hre since the cases leaves both e
  // and d untouched if the case does not correspond to the correct event.
  return (cases(std::forward<Evt>(e), std::forward<Data>(d)) || ...);
}
template <has_event_type Evt, typename... Cases>
  requires(bp::is_weakly_unique(std::remove_cvref_t<Cases>::event_enum...) &&
           impl::is_event_case<Cases...>)
constexpr bool ui_event_switch(Evt &&e, Cases &&...cases) {
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
    requires((std::invocable<Fs, decltype(evt)> ||
                  std::invocable<Fs, decltype(evt), Data &&> ||
                  std::invocable < Fs,
              decltype(evt), Data &&, decltype(args)... >) &&
             ...)
  {
    using evt_t = decltype(evt);
    auto sf = bp::as_forward<decltype(self)>(self);
    auto ef = bp::as_forward<evt_t>(evt);
    if constexpr (sizeof...(args) == 0) {
      return call::apply_to(to_base(*sf), [&ef](auto &&data, auto &&...cases) {
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
      return call::apply_to(*to_base(*sf), [&case_caller]<typename... Cases>(
                                               Data &&d, Cases &&...cs) {
        return (case_caller(std::forward<Data>(d), std::forward<Cases>(cs)) ||
                ...);
      });
    }
  }

  constexpr bool operator()(auto &&evt, auto &&...args) & {
    return ui_event_switch_t::call(*this, std::forward<decltype(evt)>(evt),
                                   std::forward<decltype(args)>(args)...);
  }
  constexpr bool operator()(auto &&evt, auto &&...args) const & {
    return ui_event_switch_t::call(*this, std::forward<decltype(evt)>(evt),
                                   std::forward<decltype(args)>(args)...);
  }
  constexpr bool operator()(auto &&evt, auto &&...args) && {
    return ui_event_switch_t::call(std::move(*this),
                                   std::forward<decltype(evt)>(evt),
                                   std::forward<decltype(args)>(args)...);
  }
  constexpr bool operator()(auto &&evt, auto &&...args) const && {
    return ui_event_switch_t::call(std::move(*this),
                                   std::forward<decltype(evt)>(evt),
                                   std::forward<decltype(args)>(args)...);
  }
};
template <typename Data, typename... Cases>
  requires(bp::is_weakly_unique(std::remove_cvref_t<Cases>::event_enum...) &&
           impl::is_event_case<Cases...>)
constexpr ui_event_switch_t<std::unwrap_ref_decay_t<Data>,
                            std::remove_cvref_t<Cases>...>
saved_ui_event_switch(Data &&d, Cases &&...cases) {
  return {std::forward<Data>(d), std::forward<Cases>(cases)...};
}

struct primary_mouse_click_translator_settings {
  int drag_threshold = 5;
};

class interpreter_widget_cache {
  void const *impl_{};
  default_point_rect area_{};

public:
  constexpr explicit interpreter_widget_cache(auto &w)
    requires(!bp::cvref_type<decltype(w), interpreter_widget_cache>)
      : impl_(&w), area_(copy_box<default_point_rect>(call::area(w))) {}
  constexpr interpreter_widget_cache() noexcept = default;

  constexpr void reset() noexcept { impl_ = nullptr; }
  constexpr void reset(auto &w) noexcept {
    impl_ = &w;
    area_ = copy_box<default_point_rect>(call::area(w));
  }

  constexpr bool refers_to(auto &w) const {
    return static_cast<void const *>(&w) == impl_;
  }
  constexpr explicit operator bool() const { return impl_ != nullptr; }
  constexpr default_point_rect const &area() const noexcept { return area_; }

  constexpr friend bool
  operator==(interpreter_widget_cache const &lhs,
             interpreter_widget_cache const &rhs) noexcept {
    return lhs.impl_ == rhs.impl_;
  }
};

constexpr auto is_cached_widget(interpreter_widget_cache const &cw) {
  return [&cw](auto &w) { return cw.refers_to(w); };
}
inline auto is_cached_widget(interpreter_widget_cache const &&) = delete;

template <interpreted_events evt_type, typename TP, typename Q,
          typename... Args>
  requires(
      std::constructible_from<interpreted_event<evt_type, TP>, TP, Args...>)
constexpr interpreter_widget_cache
_invoke_with_interpreted_event(Q &&q, point_coordinate auto const &pos,
                               TP const &tp, Args &&...args) {
  using event_t = interpreted_event<evt_type, TP>;
  interpreter_widget_cache cached{};
  q(query_interpreted_events<evt_type>(
      pos, [&]<typename W, typename S>(W &w, S &&sender) {
        CGUI_ASSERT(!cached); // called more than once!
        cached.reset(w);
        std::forward<S>(sender)(w, event_t(tp, std::forward<Args>(args)...));
      }));
  return cached;
}
template <interpreted_events... evt_types, typename TP, typename Q>
constexpr interpreter_widget_cache _invoke_with_interpreted_event(
    Q &&q, default_point_coordinate pos,
    interpreted_event<evt_types, TP> const &...events) {
  interpreter_widget_cache cached{};
  q(query_interpreted_events<evt_types...>(
      pos, [&]<typename W, typename S>(W &w, S &&sender) {
        CGUI_ASSERT(!cached); // called more than once!
        cached.reset(w);
        auto invoker = [&w, &sender]<typename E>(E const &e) {
          if constexpr (std::invocable<S, W &, E>) {
            sender(w, e);
          }
        };
        bp::run_for_each(invoker, events...);
      }));
  return cached;
}
template <interpreted_events evt_type, typename Q, typename TP,
          typename... Args>
constexpr void send_to_cached_widget(Q &&q, TP tp,
                                     interpreter_widget_cache const &cw,
                                     Args &&...args) {
  using event_t = interpreted_event<evt_type, TP>;
  bool called{};
  q(query_interpreted_events<evt_type>(
      is_cached_widget(cw), [&]<typename W, typename S>(W &w, S &&sender) {
        CGUI_ASSERT(!called); // called more than once!
        called = true;
        std::forward<S>(sender)(w, event_t(tp, std::forward<Args>(args)...));
      }));
}

template <input_events... ievs> struct _interpreter_can_handle {
  template <typename E>
  static constexpr auto op = [] { return (can_be_event<ievs, E>() || ...); };
};

struct _primary_mouse_click_translator_base {
  class keymod_state {
    bool lctrl_{};
    bool rctrl_{};

    template <bool val> constexpr void set_from_keycode(keycode c) {
      switch (c) {
        using enum keycode;
      case lctrl:
        lctrl_ = val;
        break;
      case rctrl:
        rctrl_ = val;
        break;
      default:
        // no work
        break;
      }
    }

  public:
    constexpr bool ctrl() const noexcept { return lctrl_ || rctrl_; }

    constexpr void set_down(keycode c) noexcept { set_from_keycode<true>(c); }
    constexpr void set_up(keycode c) noexcept { set_from_keycode<false>(c); }
  };
};

template <typename TimePoint>
class primary_mouse_click_translator : _primary_mouse_click_translator_base {
  struct first_down_t {
    default_point_coordinate click_position{};
    interpreter_widget_cache clicked_widget{};
  };
  struct drag_t {
    default_point_coordinate start_position{};
    interpreter_widget_cache drag_start_widget{};
  };
  struct hold_no_drag_t {
    interpreter_widget_cache widget{};

    template <typename Q>
    constexpr hold_no_drag_t(Q &&q, interpreter_widget_cache const &prev_widget,
                             point_coordinate auto const &pos, TimePoint ts)
        : widget(prev_widget) {
      using enum interpreted_events;
      q(query_interpreted_events<pointer_enter, pointer_hover>(
          pos,
          [&]<typename W, typename S>(W &w, S &&sender) {
            if (!prev_widget.refers_to(w)) {
              if (prev_widget) {
                send_to_cached_widget<pointer_exit>(q, ts, prev_widget);
              }
              widget.reset(w);
              if constexpr (std::invocable<
                                S, W &,
                                interpreted_event<pointer_enter, TimePoint>>) {
                sender(w, interpreted_event<pointer_enter, TimePoint>(ts, pos));
              }
            }
            if constexpr (std::invocable<
                              S, W &,
                              interpreted_event<pointer_hold, TimePoint>>) {
              sender(w, interpreted_event<pointer_hold, TimePoint>(ts, pos));
            }
          },
          [this, &q, ts]() noexcept {
            if (widget) {
              send_to_cached_widget<pointer_exit>(q, ts, widget);
            }
            widget.reset();
          }));
    }

    template <typename Q>
    constexpr hold_no_drag_t(Q &&q, point_coordinate auto const &pos,
                             TimePoint ts)
        : widget(_invoke_with_interpreted_event(
              q, pos,
              interpreted_event<interpreted_events::pointer_enter, TimePoint>(
                  ts, pos),
              interpreted_event<interpreted_events::pointer_hold, TimePoint>(
                  ts, pos))) {}
  };
  struct hover_t {
    interpreter_widget_cache widget{};

    constexpr hover_t() = default;

    template <typename Q>
    constexpr hover_t(Q &&q, interpreter_widget_cache const &prev_widget,
                      point_coordinate auto const &pos, TimePoint ts)
        : widget(prev_widget) {
      using enum interpreted_events;
      q(query_interpreted_events<pointer_enter, pointer_hover>(
          pos,
          [&]<typename W, typename S>(W &w, S &&sender) {
            if (!prev_widget.refers_to(w)) {
              if (prev_widget) {
                send_to_cached_widget<pointer_exit>(q, ts, prev_widget);
              }
              widget.reset(w);
              if constexpr (std::invocable<
                                S, W &,
                                interpreted_event<pointer_enter, TimePoint>>) {
                sender(w, interpreted_event<pointer_enter, TimePoint>(ts, pos));
              }
            }
            if constexpr (std::invocable<
                              S, W &,
                              interpreted_event<pointer_hover, TimePoint>>) {
              sender(w, interpreted_event<pointer_hover, TimePoint>(ts, pos));
            }
          },
          [this, &q, ts]() noexcept {
            if (widget) {
              send_to_cached_widget<pointer_exit>(q, ts, widget);
            }
            widget.reset();
          }));
    }

    template <typename Q>
    constexpr hover_t(Q &&q, point_coordinate auto const &pos, TimePoint ts)
        : widget(_invoke_with_interpreted_event(
              q, pos,
              interpreted_event<interpreted_events::pointer_enter, TimePoint>(
                  ts, pos),
              interpreted_event<interpreted_events::pointer_hover, TimePoint>(
                  ts, pos))) {}
  };

  using state = std::variant<hover_t, first_down_t, hold_no_drag_t, drag_t>;

  static constexpr state pointer_visit(auto &&cb, auto &&q, auto const &e,
                                       auto &&s_in) {
    return std::visit([&](auto &s) -> state { return cb(s, q, e); }, s_in);
  }

public:
  using settings = primary_mouse_click_translator_settings;

private:
  state s_;
  keymod_state keys_;
  settings conf_;

  template <typename F>
  static constexpr void
  _cancel(state const &, F &&,
          primary_mouse_click_translator_settings const &) {}
  template <typename F>
  static constexpr void
  _cancel(state const *v, F &&f,
          primary_mouse_click_translator_settings const &conf) {
    if (v != nullptr) {
      _cancel(*v, f, conf);
    }
  }

  template <typename E, typename Q>
  static constexpr state
  _move(state const &s, E const &e, Q &&q, keymod_state const &,
        primary_mouse_click_translator_settings const &conf) {
    return std::visit(
        [&]<typename T>(T const &s) -> state {
          if constexpr (std::is_same_v<T, drag_t>) {
            _invoke_with_interpreted_event<
                interpreted_events::pointer_drag_move>(
                q, s.start_position, call::time_stamp(e), s.start_position,
                call::position(e));
            return s;
          } else if constexpr (std::same_as<T, first_down_t>) {
            if (distance_sqr(s.click_position.value(),
                             call::position(e).value()) >
                (conf.drag_threshold * conf.drag_threshold)) {
              if (_invoke_with_interpreted_event(
                      q, s.click_position,
                      interpreted_event<interpreted_events::pointer_drag_start,
                                        TimePoint>(call::time_stamp(e),
                                                   call::position(e)),
                      interpreted_event<interpreted_events::pointer_drag_move,
                                        TimePoint>(call::time_stamp(e),
                                                   s.click_position,
                                                   call::position(e)))) {
                return drag_t{.start_position = s.click_position,
                              .drag_start_widget = s.clicked_widget};
              } else {
                return hold_no_drag_t(q, s.clicked_widget, call::position(e),
                                      call::time_stamp(e));
              }
            }
            return s;
          } else if constexpr (std::same_as<T, hover_t>) {
            return hover_t(q, s.widget, call::position(e), call::time_stamp(e));
          } else if constexpr (std::same_as<T, hold_no_drag_t>) {
            return hold_no_drag_t(q, s.widget, call::position(e),
                                  call::time_stamp(e));
          } else {
            return s;
          }
        },
        s);
  }

  template <typename E, typename Q>
  static constexpr state
  _bdown(state const &v, E const &e, Q &&q, keymod_state const &,
         primary_mouse_click_translator_settings const &) {
    if (std::holds_alternative<hover_t>(v)) {
      interpreter_widget_cache w{};
      if (call::mouse_button(e) == mouse_buttons::primary) {
        w = _invoke_with_interpreted_event<interpreted_events::pointer_hold>(
            q, call::position(e), call::time_stamp(e), call::position(e));
        return first_down_t{
            .click_position =
                copy_coordinate<default_point_coordinate>(call::position(e)),
            .clicked_widget = w};
      }
      return v;
    } else {
      return v;
    }
  }

  template <typename E, typename Q>
  static constexpr state
  _bup(state const &v, E const &e, Q &&q, keymod_state const &,
       primary_mouse_click_translator_settings const &conf) {
    if (call::mouse_button(e) == mouse_buttons::secondary) {
      _cancel(v, q, conf);
      _invoke_with_interpreted_event<interpreted_events::context_menu_click>(
          q, call::position(e), call::time_stamp(e), call::position(e));
      return v;
    }
    return std::visit(
        [&]<typename T>(T const &s) -> state {
          if constexpr (std::is_same_v<T, drag_t>) {
            send_to_cached_widget<
                interpreted_events::pointer_drag_finished_source>(
                q, call::time_stamp(e), s.drag_start_widget, s.start_position,
                call::position(e));
            _invoke_with_interpreted_event<
                interpreted_events::pointer_drag_finished_destination>(
                q, call::position(e), call::time_stamp(e), s.start_position,
                call::position(e));
            return hover_t(q, s.drag_start_widget, call::position(e),
                           call::time_stamp(e));
          } else if constexpr (bp::same_as_any<T, first_down_t,
                                               hold_no_drag_t>) {
            auto w = _invoke_with_interpreted_event<
                interpreted_events::primary_click>(
                q, call::position(e), call::time_stamp(e), call::position(e));
            return hover_t(q, w, call::position(e), call::time_stamp(e));
          } else {
            static_assert(std::is_same_v<T, hover_t>,
                          "Assertion means we have more states to care about");
            return s;
          }
        },
        v);
  }
  template <typename Q>
  static constexpr auto evt_switch(Q &&qi, state &s_in, keymod_state &k_in,
                                   settings const &confi) {
    return saved_ui_event_switch(
        std::tuple<Q &&, state &, keymod_state &, settings const &>(
            qi, s_in, k_in, confi),
        event_case<input_events::mouse_button_down>( //
            [](auto const &e, auto &&d) {
              auto &[q, s, ks, conf] = d;
              s = _bdown(std::as_const(s), e, q, std::as_const(ks), conf);
            }),
        event_case<input_events::mouse_move>( //
            [](auto const &e, auto &&d) {
              auto &[q, s, ks, conf] = d;
              s = _move(std::as_const(s), e, q, std::as_const(ks), conf);
            }),
        event_case<input_events::mouse_button_up>( //
            [](auto const &e, auto &&d) {
              auto &[q, s, ks, conf] = d;
              s = _bup(std::as_const(s), e, q, std::as_const(ks), conf);
            }),
        event_case<input_events::mouse_scroll>([](auto const &e, auto &&d) {
          auto &[q, s, ks, conf] = d;
          if (ks.ctrl()) {
            constexpr float base_scale = 1.05f;
            auto scale = std::pow(base_scale, call::delta_y(e));
            _invoke_with_interpreted_event<interpreted_events::zoom>(
                q, call::position(e), call::time_stamp(e), call::position(e),
                scale, scale);
          } else {
            _invoke_with_interpreted_event<interpreted_events::scroll>(
                q, call::position(e), call::time_stamp(e), call::position(e),
                call::delta_x(e), call::delta_y(e));
          }
          unused(s, conf);
        }),
        event_case<input_events::key_down>([](auto const &e, auto &&d) {
          auto &ks = std::get<keymod_state &>(d);
          ks.set_down(call::raw_key(e));
        }),
        event_case<input_events::key_up>([](auto const &e, auto &&d) {
          auto &ks = std::get<keymod_state &>(d);
          ks.set_up(call::raw_key(e));
        }) //
    );
  }

public:
  template <typename E>
  static constexpr auto can_handle = _interpreter_can_handle<
      input_events::mouse_button_down, input_events::mouse_button_up,
      input_events::mouse_move, input_events::mouse_scroll,
      input_events::key_down, input_events::key_up>::op<E>;

  template <typename Evt, typename Q>
  constexpr void handle(Evt const &e, Q &&q) {
    evt_switch(q, s_, keys_, std::as_const(conf_))(e);
  }
  template <typename Q> constexpr void pass_time(TimePoint const &, Q &&) {}

  constexpr settings &get_settings() noexcept { return conf_; }
};

struct _touch_translator_base {
  struct settings {
    std::chrono::milliseconds context_menu_hold{500};
    int drag_threshold = 5;
    int zoom_threshold = 5;
    int scroll_threshold = 5;
  };

  struct no_fingers_t {};
  struct first_down_t {
    interpreter_widget_cache held_widget{};
    default_point_coordinate down_position{};
    default_point_coordinate last_position{};

    constexpr first_down_t(auto &&hw, default_point_coordinate pos)
        : held_widget(std::forward<decltype(hw)>(hw)), down_position(pos),
          last_position(pos) {}
  };
  struct drag_t {
    interpreter_widget_cache held_widget{};
    default_point_coordinate down_position{};
  };
  struct hold_no_drag_t {
    interpreter_widget_cache held_widget{};
    default_point_coordinate down_position{};
    default_point_coordinate last_position{};
  };
  struct _scroll_zoom_base {
    interpreter_widget_cache widget;
    default_point_coordinate down_position{};
    default_point_coordinate last_position;
    int combo_state_index{};
    constexpr _scroll_zoom_base(auto &&w, default_point_coordinate dp,
                                default_point_coordinate lp, int csi) noexcept
        : widget(w), down_position(dp), last_position(lp),
          combo_state_index(csi) {}
  };

  struct scroll_t : _scroll_zoom_base {
    using _scroll_zoom_base::_scroll_zoom_base;
  };
  struct zoom_t : _scroll_zoom_base {
    using _scroll_zoom_base::_scroll_zoom_base;
  };
  struct scroll_zoom_t : _scroll_zoom_base {
    using _scroll_zoom_base::_scroll_zoom_base;
  };
  struct gesture_finished_t {};
  template <typename T>
  static constexpr bool is_scroller =
      bp::same_as_any<T, scroll_t, scroll_zoom_t>;
  template <typename T>
  static constexpr bool is_zoomer = bp::same_as_any<T, zoom_t, scroll_zoom_t>;

  using state_var =
      std::variant<no_fingers_t, first_down_t, drag_t, hold_no_drag_t, scroll_t,
                   zoom_t, scroll_zoom_t, gesture_finished_t>;

  template <typename T>
    requires(bp::same_as_any<T, scroll_t, zoom_t, scroll_zoom_t>)
  static constexpr void
  enter(auto &&q, auto const &e, default_point_coordinate position,
        std::pair<default_point_coordinate, default_point_coordinate> const
            &down_positions,
        std::pair<state_var *, int> result1,
        std::pair<state_var *, int> result2,
        default_point_coordinate r2_position, auto const &scroll_or_zoom_val,
        auto const &...opt_zoom_value) {
    using time_point_t = std::remove_cvref_t<decltype(call::time_stamp(e))>;
    using scroll_event_t =
        interpreted_event<interpreted_events::scroll, time_point_t>;
    using zoom_event_t =
        interpreted_event<interpreted_events::zoom, time_point_t>;
    auto get_zoom = [&]() {
      if constexpr (std::same_as<T, scroll_zoom_t>) {
        static_assert(sizeof...(opt_zoom_value) == 1, "Missing zoom-value");
        return bp::first_of(opt_zoom_value...);
      } else {
        static_assert(sizeof...(opt_zoom_value) == 0, "Too many arguments");
        return scroll_or_zoom_val;
      }
    };
    // We had to set this in a separate function to work-around a bug in clang
    // that caused it to try to decompose scroll_or_zoom_val when it was a
    // simple float-value (in the pure zoom-case).
    auto do_send_scroll = [&](auto &w, auto &&s) {
      if constexpr (is_scroller<T>) {
        auto [scx, scy] = scroll_or_zoom_val;
        s(w, scroll_event_t(call::time_stamp(e), position, scx, scy));
      } else {
        unused(w, s);
      }
    };
    // We had to set this in a separate function to work-around a bug in clang
    // that caused it to try to use the pair in scroll_or_zoom_val when it was
    // in the pure scroll case..
    auto do_send_zoom = [&](auto &w, auto &&s) {
      if constexpr (is_zoomer<T>) {
        auto [scx, scy] = get_zoom();
        s(w, zoom_event_t(call::time_stamp(e), position, scx, scy));
      } else {
        unused(w, s);
      }
    };
    q(query_interpreted_events<interpreted_events::scroll,
                               interpreted_events::zoom>(
        position, [&]<typename W, typename S>(W &w, S &&sender) {
          constexpr bool should_scroll =
              is_scroller<T>; // std::invocable<S, W&, scroll_event_t> &&
                              // is_scroller<T>;
          constexpr bool should_zoom =
              std::invocable<S, W &, zoom_event_t> && is_zoomer<T>;
          if constexpr (should_scroll) {
            do_send_scroll(w, sender);
          }
          if constexpr (should_zoom) {
            do_send_zoom(w, sender);
          }
          if constexpr (should_scroll && should_zoom) {
            *result1.first = scroll_zoom_t(w, down_positions.first,
                                           call::position(e), result2.second);
            *result2.first = scroll_zoom_t(w, down_positions.second,
                                           r2_position, result1.second);
          } else if constexpr (should_scroll) {
            *result1.first = scroll_t(w, down_positions.first,
                                      call::position(e), result2.second);
            *result2.first =
                scroll_t(w, down_positions.second, r2_position, result1.second);
          } else if constexpr (should_zoom) {
            *result1.first = zoom_t(w, down_positions.first, call::position(e),
                                    result2.second);
            *result2.first =
                zoom_t(w, down_positions.second, r2_position, result1.second);
          }
        }));
  }
};

template <typename TimePoint> class touch_translator : _touch_translator_base {

  static constexpr int max_fingers = 5;

  struct state_t {
    state_var state;
    int finger_index{};
    constexpr state_t() noexcept = default;
    template <typename T>
      requires(std::constructible_from<state_var, T>)
    constexpr state_t(T &&s_in, int fi) noexcept
        : state(std::forward<T>(s_in)), finger_index(fi) {}
    constexpr void reset() {
      std::cout << "ASDAD RESETTING\n";
      state = no_fingers_t{};
    }
  };

  std::array<state_t, max_fingers> states_;
  settings conf_;
  int active_fingers_{};

  template <typename... AllowedTypes>
  constexpr bool get_combo_state(int finger_to_avoid, TimePoint,
                                 auto &&callback) {
    if (active_fingers_ < 2) {
      return false;
    }
    for (int i{}; auto &s_fi : states_) {
      if (s_fi.finger_index != finger_to_avoid) {
        if (std::visit(
                [&callback, i]<typename S>(S &s) {
                  if constexpr (bp::same_as_any<S, AllowedTypes...>) {
                    callback(s, i);
                    return true;
                  } else {
                    return false;
                  }
                },
                s_fi.state)) {
          return true;
        }
      }
      ++i;
    }
    return false;
  }

  // Search for a state that can be associated with index.
  constexpr std::pair<state_var *, int> state_for_finger(int index) noexcept {
    state_t *first_unused{};
    int first_unused_index{};
    for (int i = 0; auto &s : states_) {
      if (s.finger_index == index) {
        return {&s.state, i};
      }
      if (first_unused == nullptr &&
          std::holds_alternative<no_fingers_t>(s.state)) {
        first_unused = &s;
        first_unused_index = i;
      }
      ++i;
    }
    if (first_unused != nullptr) {
      first_unused->finger_index = index;
      return {&first_unused->state, first_unused_index};
    }
    return {nullptr, -1};
  }

  static constexpr std::pair<float, float>
  zoom_from_distances(float const &old_distance,
                      point_coordinate auto const &new_distances) {
    auto get_scale = [&](auto xy_of) {
      return std::clamp(
          std::abs(static_cast<float>(xy_of(new_distances.value()))) /
                  old_distance +
              0.0001f,
          1.f / 128.f, 128.f);
    };
    return {get_scale(call::x_of), get_scale(call::y_of)};
  }

  static constexpr std::pair<float, float>
  get_zoom_value(point_coordinate auto const &p1_org,
                 point_coordinate auto const &p1_new,
                 point_coordinate auto const &p2_org,
                 point_coordinate auto const &p2_new) {
    auto old_distances = sub(p2_org, p1_org);
    auto old_dist = length(old_distances.value());
    auto new_distances = sub(p2_new, p1_new);
    return zoom_from_distances(old_dist, new_distances);
  }

  static constexpr std::optional<std::pair<float, float>> get_opt_zoom_value(
      point_coordinate auto const &p1_org, point_coordinate auto const &p1_new,
      point_coordinate auto const &p2_org, point_coordinate auto const &p2_new,
      settings const &conf) {
    auto old_distances = sub(p2_org, p1_org);
    auto new_distances = sub(p2_new, p1_new);
    auto old_dist = length(old_distances.value());
    auto new_dist = length(new_distances.value());
    if (std::abs(old_dist - new_dist) >= conf.zoom_threshold) {
      return zoom_from_distances(old_dist, new_distances);
    }
    return {};
  }
  static constexpr std::pair<float, float>
  get_scroll_value(auto const &old_center, auto const &new_center) {
    auto center_diff = sub(new_center, old_center).value();
    return {call::x_of(center_diff), call::y_of(center_diff)};
  }
  static constexpr std::optional<std::pair<float, float>>
  get_opt_scroll_value(point_coordinate auto const &p1_org,
                       point_coordinate auto const &p1_new,
                       point_coordinate auto const &p2_org,
                       point_coordinate auto const &p2_new, settings const &s) {
    auto old_center = divide(add(p1_org, p2_org), 2);
    auto new_center = divide(add(p1_new, p2_new), 2);
    auto pot_value = get_scroll_value(old_center, new_center);
    auto [x, y] = pot_value;
    auto dist_sqr = square_value(x) + square_value(y);
    if (dist_sqr > (s.scroll_threshold * s.scroll_threshold)) {
      return pot_value;
    }
    return {};
  }

  template <typename S, typename Q, typename Event>
    requires(is_scroller<S> || is_zoomer<S>)
  constexpr void move_in_state(S &s, Q &&q, Event const &e,
                               state_var &main_state) {
    CGUI_ASSERT(s.combo_state_index >= 0);
    CGUI_ASSERT(s.combo_state_index < max_fingers);
    auto &cs_holder = states_[s.combo_state_index];
    CGUI_ASSERT(std::holds_alternative<S>(cs_holder.state));
    auto &cs = std::get<S>(cs_holder.state);
    CGUI_ASSERT(cs.combo_state_index != s.combo_state_index);
    CGUI_ASSERT(cs.widget == s.widget);
    auto new_pos = center_between(call::position(e), cs.last_position);
    auto old_pos = center_between(s.last_position, cs.last_position);
    if constexpr (is_scroller<S>) {
      // send scroll
      auto [scx, scy] = get_scroll_value(old_pos, new_pos);
      send_to_cached_widget<interpreted_events::scroll>(
          q, call::time_stamp(e), s.widget, new_pos, scx, scy);
    }
    if constexpr (is_zoomer<S>) {
      auto [dx, dy] = get_zoom_value(s.last_position, call::position(e),
                                     cs.last_position, cs.last_position);
      send_to_cached_widget<interpreted_events::zoom>(
          q, call::time_stamp(e), s.widget, new_pos, dx, dy);
      // send zoom
    }
    s.last_position = call::position(e);
    if constexpr (!is_scroller<S> || !is_zoomer<S>) {
      auto constexpr to_scroll_zoom_t = [](auto &&in_state) {
        return scroll_zoom_t(std::forward<decltype(in_state)>(in_state).widget,
                             in_state.down_position, in_state.last_position,
                             in_state.combo_state_index);
      };
      auto constexpr set_both_states = [=]<typename... T1, typename... T2>(
                                           std::pair<T1 &, T2 &>... states) {
        unused((states.first = to_scroll_zoom_t(std::move(states.second)))...);
      };
      auto constexpr to_ref_pair =
          []<typename T1, typename T2>(
              T1 &&f, T2 &&s) -> std::pair<T1 &&, T2 &&> { return {f, s}; };
      if constexpr (!is_scroller<S>) {
        // check if scroll should be enabled, then enable it and change the
        // states.
        if (auto scroll = get_opt_scroll_value(
                cs.down_position, cs.last_position, s.down_position,
                call::position(e), conf_)) {

          auto const [scx, scy] = *scroll;
          send_to_cached_widget<interpreted_events::scroll>(
              q, call::time_stamp(e), s.widget, new_pos, scx, scy);
        }
      } else if constexpr (!is_zoomer<S>) {
        // check if zoom should be enabled, then enable it and change the
        // states.
        if (auto zl =
                get_opt_zoom_value(cs.down_position, cs.last_position,
                                   s.down_position, call::position(e), conf_)) {
          auto [dx, dy] = *zl;
          send_to_cached_widget<interpreted_events::zoom>(
              q, call::time_stamp(e), s.widget, new_pos, dx, dy);
          set_both_states(to_ref_pair(main_state, s),
                          to_ref_pair(cs_holder.state, cs));
        }
      }
    }
  }

  constexpr void reset_state(state_var &s) {
    CGUI_ASSERT((!std::holds_alternative<no_fingers_t>(s)));
    s = no_fingers_t{};
    --active_fingers_;
  }

  static constexpr interpreter_widget_cache
  move_no_drag(auto &&q, auto const &e, interpreter_widget_cache org_w) {
    auto constexpr exit_widget = [](interpreter_widget_cache const &w,
                                    TimePoint const &tp, auto &&qin) {
      send_to_cached_widget<interpreted_events::pointer_exit>(qin, tp, w);
    };
    q(query_interpreted_events<interpreted_events::pointer_enter,
                               interpreted_events::pointer_hold>(
        call::position(e),
        [&]<typename W, typename S>(W &w, S &&sender) {
          auto tp = call::time_stamp(e);
          if (!org_w.refers_to(w)) {
            exit_widget(org_w, tp, q);
            org_w.reset(w);
            if constexpr (std::invocable<S &&, W &,
                                         interpreted_event<
                                             interpreted_events::pointer_enter,
                                             TimePoint>>) {
              sender(w, interpreted_event<interpreted_events::pointer_enter,
                                          TimePoint>(tp, call::position(e)));
            }
          }
          if constexpr (std::invocable<
                            S &&, W &,
                            interpreted_event<interpreted_events::pointer_hold,
                                              TimePoint>>) {
            sender(
                w,
                interpreted_event<interpreted_events::pointer_hold, TimePoint>(
                    tp, call::position(e)));
          }
        },
        [&]() noexcept {
          exit_widget(org_w, call::time_stamp(e), q);
          org_w.reset();
        }));
    return org_w;
  }

  template <typename Q>
  constexpr auto evt_switch(Q &&qi, state_var &s_in, int const &state_index,
                            settings const &confi) {
    return saved_ui_event_switch(
        std::forward_as_tuple(qi, s_in, confi, state_index, *this),
        // ---------------------
        event_case<input_events::touch_down>([](auto const &e, auto &&d) {
          auto &[q, s, conf, si, self] = d;
          auto const pos = call::position(e);
          if (std::holds_alternative<no_fingers_t>(s)) {
            ++self.active_fingers_;
          } else {
            // Something is terribly wrong if we hit this.
            CGUI_ASSERT(false);
          }
          s = first_down_t(
              _invoke_with_interpreted_event(
                  q, pos,
                  interpreted_event<interpreted_events::pointer_enter>(
                      call::time_stamp(e), pos),
                  interpreted_event<interpreted_events::pointer_hold>(
                      call::time_stamp(e), pos)),
              pos);
        }),
        // ---------------------
        event_case<input_events::touch_up>([](auto const &e, auto &&d) {
          auto &[q, s, conf, si, self] = d;
          std::visit(
              [&]<typename S>(S &sv) {
                if constexpr (bp::same_as_any<S, first_down_t,
                                              hold_no_drag_t>) {
                  _invoke_with_interpreted_event<
                      interpreted_events::primary_click>(q, call::position(e),
                                                         call::time_stamp(e),
                                                         call::position(e));
                  send_to_cached_widget<interpreted_events::pointer_exit>(
                      q, call::time_stamp(e), sv.held_widget);
                } else if constexpr (std::is_same_v<S, drag_t>) {
                  send_to_cached_widget<
                      interpreted_events::pointer_drag_finished_source>(
                      q, call::time_stamp(e), sv.held_widget, sv.down_position,
                      call::position(e));
                  send_to_cached_widget<interpreted_events::pointer_exit>(
                      q, call::time_stamp(e), sv.held_widget);
                  _invoke_with_interpreted_event<
                      interpreted_events::pointer_drag_finished_destination>(
                      q, call::position(e), call::time_stamp(e),
                      sv.down_position, call::position(e));
                } else if constexpr (bp::same_as_any<S, zoom_t, scroll_t,
                                                     scroll_zoom_t>) {
                  CGUI_ASSERT(
                      (sv.combo_state_index < std::ssize(self.states_) &&
                       sv.combo_state_index >= 0));
                  auto &s2 = self.states_[sv.combo_state_index];
                  s2.state = gesture_finished_t{};
                  send_to_cached_widget<interpreted_events::pointer_exit>(
                      q, call::time_stamp(e), sv.widget);
                }
              },
              s);
          self.reset_state(s);
        }),
        // ---------------------
        event_case<input_events::touch_move>([](auto const &e, auto &&d) {
          auto &[q, s, conf, si, self] = d;
          std::visit(
              [&]<typename S>(S &sv) {
                // ===
                if constexpr (bp::same_as_any<S, first_down_t,
                                              hold_no_drag_t>) {
                  sv.last_position = call::position(e);
                }
                if constexpr (is_scroller<S> || is_zoomer<S>) {
                  self.move_in_state(sv, q, e, s);
                } else if constexpr (std::is_same_v<S, first_down_t>) {
                  if (self.template get_combo_state<first_down_t,
                                                    hold_no_drag_t>(
                          call::finger_index(e), call::time_stamp(e),
                          [&](auto &s2, int si2) {
                            auto combo_dist_sqr =
                                distance_sqr(s2.down_position.value(),
                                             call::position(e).value());
                            auto org_center = center_between(s2.down_position,
                                                             sv.down_position);

                            auto zoom_value = get_opt_zoom_value(
                                s2.down_position, s2.last_position,
                                sv.down_position, call::position(e), conf);
                            auto scroll_value = get_opt_scroll_value(
                                s2.down_position, s2.last_position,
                                sv.down_position, call::position(e), conf);
                            CGUI_ASSERT(si2 < max_fingers);
                            auto &s2_vfi = self.states_[si2];
                            auto s2_pos = s2.last_position;
                            if (zoom_value && scroll_value) {
                              touch_translator::enter<scroll_zoom_t>(
                                  q, e, org_center,
                                  {sv.down_position, s2.down_position},
                                  {&s, si}, {&s2_vfi.state, si2}, s2_pos,
                                  *scroll_value, *zoom_value);
                            } else if (zoom_value) {
                              touch_translator::enter<zoom_t>(
                                  q, e, org_center,
                                  {sv.down_position, s2.down_position},
                                  {&s, si}, {&s2_vfi.state, si2}, s2_pos,
                                  *zoom_value);
                            } else if (scroll_value) {
                              touch_translator::enter<scroll_t>(
                                  q, e, org_center,
                                  {sv.down_position, s2.down_position},
                                  {&s, si}, {&s2_vfi.state, si2}, s2_pos,
                                  *scroll_value);
                            }
                          })) {
                    return;
                  }

                  auto dist_sqr = distance_sqr(sv.down_position.value(),
                                               call::position(e).value());

                  if (dist_sqr > (conf.drag_threshold * conf.drag_threshold)) {
                    auto drag_widget = _invoke_with_interpreted_event(
                        q, sv.down_position,
                        interpreted_event<
                            interpreted_events::pointer_drag_start, TimePoint>(
                            call::time_stamp(e), call::position(e)),
                        interpreted_event<interpreted_events::pointer_drag_move,
                                          TimePoint>(call::time_stamp(e),
                                                     sv.down_position,
                                                     call::position(e)));
                    if (drag_widget) {
                      s = drag_t{drag_widget, sv.down_position};
                    } else {
                      s = hold_no_drag_t{
                          move_no_drag(q, e, std::move(sv.held_widget)),
                          sv.down_position, call::position(e)};
                    }
                  } else {
                    send_to_cached_widget<interpreted_events::pointer_hold>(
                        q, call::time_stamp(e), sv.held_widget,
                        call::position(e));
                  }
                } else if constexpr (bp::same_as_any<S, hold_no_drag_t>) {
                  sv.held_widget =
                      move_no_drag(q, e, std::move(sv.held_widget));
                }
                // ===
                else if constexpr (std::is_same_v<S, drag_t>) {
                  send_to_cached_widget<interpreted_events::pointer_drag_move>(
                      q, call::time_stamp(e), sv.held_widget, sv.down_position,
                      call::position(e));
                }
                // ===
                else if constexpr (std::is_same_v<S, hold_no_drag_t>) {
                  _invoke_with_interpreted_event<
                      interpreted_events::pointer_hold, TimePoint>(
                      q, call::position(e), call::time_stamp(e),
                      call::position(e));
                }
              },
              s);
        })
        // ---------------------
    );
  }

public:
  template <typename E>
  static constexpr auto can_handle =
      _interpreter_can_handle<input_events::touch_down, input_events::touch_up,
                              input_events::touch_move>::op<E>;

  template <typename Evt, typename Q>
  constexpr void handle(Evt const &e, Q &&q) {
    if (auto [s, index] = state_for_finger(call::finger_index(e));
        s != nullptr) {
      evt_switch(q, *s, index, std::as_const(conf_))(e);
    }
  }

  constexpr settings &get_settings() noexcept { return conf_; }
};

template <typename TimePoint = std::chrono::steady_clock>
using default_event_interpreter =
    event_interpreter<TimePoint, primary_mouse_click_translator,
                      touch_translator>;
} // namespace cgui

#endif

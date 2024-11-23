
#ifndef COMPONENT_GUI_CGUI_UI_EVENTS_HPP
#define COMPONENT_GUI_CGUI_UI_EVENTS_HPP

#include <chrono>
#include <tuple>
#include <variant>

#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui {

enum class input_events {
  system,
  mouse_move,
  mouse_button_down,
  mouse_button_up,
  mouse_exit,
  window_resized,
};

template <input_events tEvt> struct input_event_identity {
  static constexpr input_events value = tEvt;
};

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
template <> struct input_event_constraints<input_events::window_resized> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::size_of(t) } -> point_size_wh;
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
template <input_events... evts> struct subset_input_events {
  static_assert(sizeof...(evts) > 0, "You must at least specify 1 event type");
  input_events val;
  constexpr explicit(false) subset_input_events(input_events v) noexcept
      : val(v) {
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
template <input_events evt> struct subset_input_events<evt> {
  constexpr explicit(false) subset_input_events(input_events v) noexcept {
    assert(v == evt);
    unused(v);
  }
  constexpr subset_input_events() noexcept = default;
  constexpr explicit(false) operator input_events() const noexcept {
    return evt;
  }

  template <input_events tEvt>
  static consteval bool can_be_event(input_event_identity<tEvt> = {}) noexcept {
    return (tEvt == evt);
  }
};

template <typename T>
concept subset_input_event_c =
    std::convertible_to<T, input_events> && requires() {
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
      { _do_event_type{}(std::forward<T>(t)) } -> subset_input_event_c;
    })
  constexpr subset_input_event_c auto operator()(T &&t) const {
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
template <input_events tEvt, typename T> constexpr bool is_event(T &&evt) {
  if constexpr (can_be_event<tEvt, T>()) {
    return static_cast<input_events>(call::event_type(evt)) == tEvt;
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
template <> struct default_event<input_events::mouse_exit> {};
template <> struct default_event<input_events::mouse_move> {
  default_point_coordinate pos{};
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

using default_mouse_move_event = default_event<input_events::mouse_move>;
using default_mouse_down_event = default_event<input_events::mouse_button_down>;
using default_mouse_up_event = default_event<input_events::mouse_button_up>;
using default_mouse_exit_event = default_event<input_events::mouse_exit>;
using default_window_resized_event =
    default_event<input_events::window_resized>;

enum class interpreted_events {
  primary_click = 1,
  context_menu_click,
  pointer_drag_start,
  pointer_drag_move,
  pointer_drag_finished,
  pointer_hover,
  pointer_hold,
  pointer_enter,
  pointer_exit,
};

template <typename T, interpreted_events... ie_vs>
concept interpreted_event_types = (can_be_event<ie_vs, T>() || ...);

template <interpreted_events> struct interpreted_event_impl;
template <interpreted_events ie_v, typename TimePoint>
struct interpreted_event : interpreted_event_impl<ie_v> {
  using _base_t = interpreted_event_impl<ie_v>;
  interpreted_event_basic<TimePoint> common_data;
  constexpr explicit interpreted_event(auto const &evt)
      : _base_t(evt), common_data(evt) {}
  constexpr interpreted_event(_base_t const &impl, TimePoint const &ts)
      : _base_t(impl), common_data(ts) {}
  template <typename... Ts>
    requires(std::constructible_from<_base_t, Ts && ...>)
  constexpr explicit interpreted_event(TimePoint const &ts, Ts &&...args)
      : _base_t(std::forward<Ts>(args)...), common_data(ts) {}
};

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
struct interpreted_event_impl<interpreted_events::pointer_drag_finished> {
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
template <typename Pred, typename OnFind, interpreted_events... events>
struct query_interpreted_events_t {
  Pred p_;
  OnFind f_;

  constexpr query_interpreted_events_t(Pred p, OnFind f)
      : p_(std::move(p)), f_(std::move(f)) {}

  constexpr bool operator()(auto &in, auto &&s) const {
    if (p_(in)) {
      f_(in, std::forward<decltype(s)>(s));
      return true;
    }
    return false;
  }
};

template <interpreted_events... events, typename Pred, typename OnFind>
  requires(std::predicate<Pred, dummy_widget const &>)
constexpr query_interpreted_events_t<std::remove_cvref_t<Pred>,
                                     std::remove_cvref_t<OnFind>, events...>
query_interpreted_events(Pred &&p, OnFind &&f) {
  return {std::forward<Pred>(p), std::forward<OnFind>(f)};
}

template <interpreted_events... events, typename OnFind>
constexpr auto query_interpreted_events(default_point_coordinate pos,
                                        OnFind &&f) {
  return query_interpreted_events<events...>(
      [pos](auto const &w) { return hit_box(call::area(w), pos); },
      std::forward<OnFind>(f));
}

template <typename TimePoint = typename std::chrono::steady_clock::time_point,
          template <typename> typename... Interpreters>
class event_interpreter
    : bp::empty_structs_optimiser<Interpreters<TimePoint>...> {
  using fickle_state_t =
      std::variant<empty_placeholder_t,
                   state_interpreter_pair<Interpreters<TimePoint>>...>;
  using time_point_t = TimePoint;

  fickle_state_t state_{};
  std::tuple<typename Interpreters<TimePoint>::settings...> settings_;

  template <typename Interp> constexpr auto const &get_settings() const {
    return std::get<typename Interp::settings>(settings_);
  }

  constexpr void cancel_state(auto &&f) {
    std::visit(
        [this, &f]<typename T>(T &t) {
          if constexpr (!std::is_same_v<T, empty_placeholder_t>) {
            using interp = typename T::interpreter;
            interp::cancel_state(t.state, std::forward<decltype(f)>(f),
                                 get_settings<interp>());
          }
        },
        state_);
  }

  template <typename Interpreter, typename Evt, typename Q>
  constexpr bool interpret(Evt &&e, Q &&q) {
    if constexpr (Interpreter::template can_handle<
                      std::remove_cvref_t<Evt>>()) {
      auto *interpreter_state =
          std::get_if<state_interpreter_pair<Interpreter>>(&state_);
      auto new_state = Interpreter::handle(
          std::forward<Evt>(e), q, interpreter_state,
          std::get<typename Interpreter::settings>(std::as_const(settings_)));
      if (new_state) {
        if (interpreter_state == nullptr) {
          cancel_state(q);
        }
        state_ = state_interpreter_pair<Interpreter>{*new_state};
        return true;
      } else if (interpreter_state != nullptr) {
        state_ = empty_placeholder_t{};
      }
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
    state_ = std::visit(
        [this, tp = bp::as_forward<TP>(tp),
         f = bp::as_forward<F>(f)]<typename T>(T &t) -> fickle_state_t {
          if constexpr (!std::is_same_v<T, empty_placeholder_t>) {
            using interpreter_t = T::interpreter;
            if constexpr (requires() {
                            interpreter_t::pass_time(*tp, *f, t.state,
                                                     t.get_settings(settings_));
                          }) {
              auto new_state = interpreter_t::pass_time(
                  *tp, *f, t.state, t.get_settings(settings_));
              if (new_state) {
                return *new_state;
              }
            }
          }
          return {};
        },
        state_);
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
      return call::apply_to(*to_base(*sf), [&case_caller]<typename... Cases>(
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

public:
  constexpr explicit interpreter_widget_cache(auto &w) : impl_(&w) {}
  constexpr interpreter_widget_cache() noexcept = default;

  constexpr void reset() noexcept { impl_ = nullptr; }
  constexpr void reset(auto &w) noexcept { impl_ = &w; }

  constexpr bool refers_to(auto &w) const {
    return static_cast<void const *>(&w) == impl_;
  }
  constexpr explicit operator bool() const { return impl_ != nullptr; }
};

constexpr auto is_cached_widget(interpreter_widget_cache const &cw) {
  return [&cw](auto &w) { return cw.refers_to(w); };
}
constexpr auto is_cached_widget(interpreter_widget_cache const &&) = delete;

template <interpreted_events evt_type, typename TP, typename Q,
          typename... Args>
constexpr interpreter_widget_cache
_invoke_with_interpreted_event(Q &&q, default_point_coordinate pos,
                               TP const &tp, Args &&...args) {
  using event_t = interpreted_event<evt_type, TP>;
  interpreter_widget_cache cached{};
  q(query_interpreted_events<evt_type>(
      pos, [&]<typename W, typename S>(W &w, S &&sender) {
        CGUI_ASSERT(!cached); // called more than once!
        cached.reset(w);
        std::forward<S>(sender)(w, // std::forward<W>(w),
                                event_t(tp, std::forward<Args>(args)...));
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

template <typename T, typename TIn>
constexpr std::optional<T> _to_optional(TIn &&in) {
  if constexpr (std::is_convertible_v<TIn, std::optional<T>>) {
    return std::forward<TIn>(in);
  } else {
    return std::optional<T>(std::in_place, std::forward<TIn>(in));
  }
}

template <typename TimePoint> class primary_mouse_click_translator {
  struct first_down_t {
    TimePoint click_time{};
    default_point_coordinate click_position{};
    interpreter_widget_cache clicked_widget{};
  };
  struct drag_t {
    default_point_coordinate start_position{};
    interpreter_widget_cache drag_start_widget{};
  };
  struct hold_no_drag_t {
    interpreter_widget_cache clicked_widget{};
  };
  struct hover_t {
    interpreter_widget_cache widget{};
  };

public:
  using state = std::variant<first_down_t, hold_no_drag_t, drag_t, hover_t>;
  /*
  struct state {
    using sub_state_t = std::variant<first_down_t, hold_no_drag_t, drag_t>;
    interpreter_widget_cache last_widget{};
    sub_state_t sub_state;
  };*/

private:
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
  static constexpr std::optional<state>
  _move(state const *v, E const &e, Q &&q,
        primary_mouse_click_translator_settings const &conf) {
    if (v != nullptr) {
      return std::visit(
          [&]<typename T>(T const &s) -> std::optional<state> {
            if constexpr (std::is_same_v<T, drag_t>) {
              _invoke_with_interpreted_event<
                  interpreted_events::pointer_drag_move>(
                  q, s.start_position, call::time_stamp(e), s.start_position,
                  call::position(e));
              return _to_optional<state>(s);
            } else if constexpr (std::same_as<T, first_down_t>) {
              if (distance_sqr(s.click_position.value(),
                               call::position(e).value()) >
                  (conf.drag_threshold * conf.drag_threshold)) {
                if (_invoke_with_interpreted_event<
                        interpreted_events::pointer_drag_start>(
                        q, s.click_position, call::time_stamp(e),
                        call::position(e))) {
                  return _to_optional<state>(
                      drag_t{.start_position = s.click_position,
                             .drag_start_widget = s.clicked_widget});
                } else {
                  return _to_optional<state>(hold_no_drag_t{s.clicked_widget});
                }
              }
              return _to_optional<state>(s);
            } else if constexpr (std::same_as<T, hover_t>) {
              hover_t res_state = s;
              using enum interpreted_events;
              q(query_interpreted_events<
                  pointer_enter,
                  pointer_hover>(call::position(e), [&]<typename W, typename S>(
                                                        W &w, S &&sender) {
                if (!res_state.widget.refers_to(w)) {
                  if (res_state.widget) {
                    send_to_cached_widget<pointer_exit>(q, call::time_stamp(e),
                                                        res_state.widget);
                  }
                  res_state.widget.reset(w);
                  if constexpr (std::invocable<S, W &,
                                               interpreted_event<pointer_enter,
                                                                 TimePoint>>) {
                    sender(w, interpreted_event<pointer_enter, TimePoint>(
                                  call::time_stamp(e), call::position(e)));
                  }
                }
                if constexpr (std::invocable<S, W &,
                                             interpreted_event<pointer_hover,
                                                               TimePoint>>) {
                  sender(w, interpreted_event<pointer_hover, TimePoint>(
                                call::time_stamp(e), call::position(e)));
                }
              }));
              return _to_optional<state>(res_state);
            } else {
              return _to_optional<state>(s);
            }
          },
          *v);
    } else {
      auto cw = _invoke_with_interpreted_event(
          q, call::position(e),
          interpreted_event<interpreted_events::pointer_enter, TimePoint>(
              call::time_stamp(e), call::position(e)),
          interpreted_event<interpreted_events::pointer_hover, TimePoint>(
              call::time_stamp(e), call::position(e)));
      return _to_optional<state>(hover_t(cw));
    }
  }

  template <typename E, typename Q>
  static constexpr std::optional<state>
  _bdown(state const *v, E const &e, Q &&q,
         primary_mouse_click_translator_settings const &) {
    if (v == nullptr) {
      interpreter_widget_cache w{};
      if (call::mouse_button(e) == mouse_buttons::primary) {
        w = _invoke_with_interpreted_event<interpreted_events::pointer_hold>(
            q, call::position(e), call::time_stamp(e), call::position(e));
        if (w) {
          return std::optional<state>(
              std::in_place,
              first_down_t{.click_time = call::time_stamp(e),
                           .click_position =
                               copy_coordinate<default_point_coordinate>(
                                   call::position(e)),
                           .clicked_widget = w});
        }
      }
      return std::nullopt;
    } else {
      return *v;
    }
  }

  template <typename E, typename Q>
  static constexpr std::optional<state>
  _bup(state const *v, E const &e, Q &&q,
       primary_mouse_click_translator_settings const &conf) {
    if (call::mouse_button(e) == mouse_buttons::secondary) {
      _cancel(v, q, conf);
      _invoke_with_interpreted_event<interpreted_events::context_menu_click>(
          q, call::position(e), call::time_stamp(e), call::position(e));
      return {};
    }
    if (v != nullptr) {
      return std::visit(
          [&]<typename T>(T const &s) -> std::optional<state> {
            if constexpr (std::is_same_v<T, drag_t>) {
              _invoke_with_interpreted_event<
                  interpreted_events::pointer_drag_finished>(
                  q, s.start_position, call::time_stamp(e), s.start_position,
                  call::position(e));
              return std::nullopt;
            } else if constexpr (bp::same_as_any<T, first_down_t,
                                                 hold_no_drag_t>) {
              _invoke_with_interpreted_event<interpreted_events::primary_click>(
                  q, call::position(e), call::time_stamp(e), call::position(e));
              return std::nullopt;
            }
            return _to_optional<state>(s);
          },
          *v);
    } else {
      // We should never arrive here, doing so would indicate either lost or
      // unordered events.
      // f(create_interpreted_event_from_event<interpreted_events::primary_click>(
      //    e, call::position(e)));
      return std::nullopt;
    }
  }

public:
  using settings = primary_mouse_click_translator_settings;
  template <typename E>
  static constexpr auto can_handle =
      _interpreter_can_handle<input_events::mouse_button_down,
                              input_events::mouse_button_up,
                              input_events::mouse_move>::op<E>;

  template <typename Q>
  static constexpr auto evt_switch(Q &&qi, state const *in,
                                   std::optional<state> &out,
                                   settings const &confi) {
    return saved_ui_event_switch(
        std::tuple<Q &&, state const *, std::optional<state> &,
                   settings const &>(qi, in, out, confi),
        event_case<input_events::mouse_button_down>( //
            [](auto const &e, auto &&d) {
              auto &[q, s_in, s_out, conf] = d;
              s_out = _bdown(s_in, e, q, conf);
            }),
        event_case<input_events::mouse_move>( //
            [](auto const &e, auto &&d) {
              auto &[q, s_in, s_out, conf] = d;
              s_out = _move(s_in, e, q, conf);
            }),
        event_case<input_events::mouse_button_up>( //
            [](auto const &e, auto &&d) {
              auto &[q, s_in, s_out, conf] = d;
              s_out = _bup(s_in, e, q, conf);
            }));
  }
  template <typename Evt, typename Q>
  static constexpr std::optional<state> handle(Evt const &e, Q &&q, auto &&s_in,
                                               settings const &conf) {
    std::optional<state> s_out;
    evt_switch(q, _to_state(s_in), s_out, conf)(e);
    return s_out;
  }
  template <typename Q>
  static constexpr std::optional<state>
  pass_time(TimePoint const &, Q &&, state s_in, settings const &) {
    return s_in;
  }
  static constexpr void cancel_state(state &s, auto &&f, settings const &conf) {
    _cancel(s, f, conf);
  }
};

template <typename TimePoint = std::chrono::steady_clock>
using default_event_interpreter =
    event_interpreter<TimePoint, primary_mouse_click_translator>;
} // namespace cgui

#endif

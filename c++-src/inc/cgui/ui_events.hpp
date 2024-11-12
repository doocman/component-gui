
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
  default_point_coordinate pos;
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_button_down> {
  default_point_coordinate pos;
  mouse_buttons button_id;
  common_event_data common_data{};
};
template <> struct default_event<input_events::mouse_button_up> {
  default_point_coordinate pos;
  mouse_buttons button_id;
  common_event_data common_data{};
};
template <> struct default_event<input_events::window_resized> {
  default_point_size_wh sz;
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

enum class interpreted_events { primary_click = 1 };
enum class early_event_tag { preliminary, confirmed, cancelled };

template <interpreted_events, typename...> struct interpreted_event_impl;
template <typename Impl, early_event_tag eet, typename TimePoint>
struct interpreted_event : Impl {
  interpreted_event_basic<TimePoint> common_data;
  constexpr explicit interpreted_event(auto const &evt)
      : Impl(evt), common_data(evt) {}
  constexpr interpreted_event(Impl const &impl, TimePoint const &ts)
      : Impl(impl), common_data(ts) {}
};

template <typename Impl, early_event_tag et, typename C>
constexpr bool is_cgui_default_event_v<interpreted_event<Impl, et, C>> = true;

template <early_event_tag eet, interpreted_events ie_v, typename... Ts>
inline constexpr auto create_interpreted_event_from_tp =
    []<typename TP, typename... Args>(TP const &time_stamp, Args &&...args) {
      using impl_t = interpreted_event_impl<ie_v, Ts...>;
      return interpreted_event<impl_t, eet, TP>(
          impl_t(std::forward<Args>(args)...), time_stamp);
    };
template <early_event_tag eet, interpreted_events ie_v, typename... Ts>
inline constexpr auto create_interpreted_event_from_event =
    []<has_time_stamp Evt, typename... Args>(Evt const &trig_event,
                                             Args &&...args) {
      return create_interpreted_event_from_tp<eet, ie_v, Ts...>(
          call::time_stamp(trig_event), std::forward<Args>(args)...);
    };

template <typename Impl, typename C>
using confirmed_interpreted_event =
    interpreted_event<Impl, early_event_tag::confirmed, C>;
template <typename Impl, typename C>
using preliminary_interpreted_event =
    interpreted_event<Impl, early_event_tag::preliminary, C>;
template <typename Impl, typename C>
using cancelled_interpreted_event =
    interpreted_event<Impl, early_event_tag::cancelled, C>;

template <> struct interpreted_event_impl<interpreted_events::primary_click> {
  using position_t = default_point_coordinate;
  position_t pos{};
  constexpr explicit interpreted_event_impl(auto const &e)
    requires(requires() {
      { call::position(e) } -> std::convertible_to<position_t>;
    })
      : pos(call::position(e)) {}
  constexpr explicit interpreted_event_impl(point_coordinate auto const &p)
      : pos(copy_coordinate<position_t>(p)) {}
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

template <typename TimePoint = typename std::chrono::steady_clock::time_point,
          template <typename> typename... Interpreters>
class event_interpreter
    : bp::empty_structs_optimiser<Interpreters<TimePoint>...> {
  using fickle_state_t =
      std::variant<empty_state,
                   state_interpreter_pair<Interpreters<TimePoint>>...>;
  using time_point_t = TimePoint;

  fickle_state_t state_{};
  std::tuple<typename Interpreters<TimePoint>::settings...> settings_;

  template <typename Interpreter, typename Evt, typename F>
  constexpr bool interpret(Evt &&e, F &&f) {
    auto new_state = Interpreter::handle(
        std::forward<Evt>(e), std::forward<F>(f),
        std::get_if<state_interpreter_pair<Interpreter>>(&state_),
        std::get<typename Interpreter::settings>(std::as_const(settings_)));
    if (new_state) {
      state_ = state_interpreter_pair<Interpreter>{*new_state};
      return true;
    }
    return false;
  }

public:
  template <typename Evt, typename F>
  constexpr bool handle(Evt const &e, F &&f) {
    return (interpret<Interpreters<TimePoint>>(e, f) || ...);
  }
  template <std::convertible_to<time_point_t> TP, typename F>
  constexpr void pass_time(TP &&tp, F &&f) {
    state_ = std::visit(
        [this, tp = bp::as_forward<TP>(tp),
         f = bp::as_forward<F>(f)]<typename T>(T &t) -> fickle_state_t {
          if constexpr (!std::is_same_v<T, empty_state>) {
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

template <input_events evt_val, typename F>
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
constexpr event_case_t<evt_v, std::remove_cvref_t<T>> event_case(T &&in) {
  return event_case_t<evt_v, std::remove_cvref_t<T>>{std::forward<T>(in)};
}

namespace impl {
template <typename> constexpr bool is_event_case_raw = false;
template <input_events e, typename T>
constexpr bool is_event_case_raw<event_case_t<e, T>> = true;
template <typename T>
constexpr bool is_event_case = is_event_case_raw<std::remove_cvref_t<T>>;
} // namespace impl

template <has_event_type Evt, typename Data, input_events... evt_vs,
          typename... Fs>
  requires(bp::is_unique(evt_vs...) && !impl::is_event_case<Data> &&
           ((std::invocable<Fs> || std::invocable<Fs, Evt &&> ||
             std::invocable<Fs, Evt &&, Data &&>) &&
            ...))
constexpr bool ui_event_switch(Evt &&e, Data &&d,
                               event_case_t<evt_vs, Fs> &&...cases) {
  // The forwarding operator is fine to use hre since the cases leaves both e
  // and d untouched if the case does not correspond to the correct event.
  return (cases(std::forward<Evt>(e), std::forward<Data>(d)) || ...);
}
template <has_event_type Evt, input_events... evt_vs,
          bp::invocable_or_invocable_args<Evt>... Fs>
  requires(bp::is_unique(evt_vs...))
constexpr bool ui_event_switch(Evt &&e, event_case_t<evt_vs, Fs> &&...cases) {
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
template <typename Data, input_events... evt_vs, typename... Fs>
constexpr ui_event_switch_t<std::unwrap_ref_decay_t<Data>,
                            event_case_t<evt_vs, Fs>...>
saved_ui_event_switch(Data &&d, event_case_t<evt_vs, Fs> &&...cases) {
  return {std::forward<Data>(d), std::move(cases)...};
}

struct primary_mouse_click_translator_settings {
  std::chrono::nanoseconds double_click_timeout =
      std::chrono::milliseconds(200);
};

template <typename F, typename C, typename... Args>
constexpr void _invoke_with_interpreted_event(F &&f, C &&c, Args &&...args) {
  using event_t = std::invoke_result_t<C, Args...>;
  if constexpr (std::invocable<F, event_t>) {
    std::invoke(std::forward<F>(f),
                std::invoke(std::forward<C>(c), std::forward<Args>(args)...));
  }
}

template <typename TimePoint> struct primary_mouse_click_translator {
  struct state {
    TimePoint click_point;
    default_point_coordinate click_position;
  };
  using settings = primary_mouse_click_translator_settings;
  template <typename F>
  static constexpr auto evt_switch(F &&f, state const *in,
                                   std::optional<state> &out) {
    return saved_ui_event_switch(
        std::tuple<F &&, state const *, std::optional<state> &>(f, in, out),
        event_case<input_events::mouse_button_up>([](auto const &e, auto &&d) {
          auto &[f, s_in, s_out] = d;
          s_out = state{
              call::time_stamp(e),
              copy_coordinate<default_point_coordinate>(call::position(e))};
          _invoke_with_interpreted_event(f,
                                         create_interpreted_event_from_event<
                                             early_event_tag::preliminary,
                                             interpreted_events::primary_click>,
                                         e, call::position(e));
        }));
  }
  template <typename Evt, typename F>
  static constexpr std::optional<state> handle(Evt const &e, F &&f, auto &&s_in,
                                               settings const &) {
    std::optional<state> s_out;
    unused(s_in);
    evt_switch(f, nullptr, s_out)(e);
    return s_out;
  }
  template <typename F>
  static constexpr std::optional<state>
  pass_time(TimePoint const &tp, F &&f, state s, settings const &conf) {
    if (tp - s.click_point > conf.double_click_timeout) {
      _invoke_with_interpreted_event(
          f,
          create_interpreted_event_from_tp<early_event_tag::confirmed,
                                           interpreted_events::primary_click>,
          s.click_point, s.click_position);
      return {};
    }
    return s;
  }
};
} // namespace cgui

#endif

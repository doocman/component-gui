
#ifndef COMPONENT_GUI_CGUI_UI_EVENTS_HPP
#define COMPONENT_GUI_CGUI_UI_EVENTS_HPP

#include <tuple>

#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui {

enum class ui_events {
  system,
  mouse_move,
  mouse_button_down,
  mouse_button_up,
  mouse_exit,
  window_resized,
};

template <ui_events tEvt> struct ui_event_identity {
  static constexpr ui_events value = tEvt;
};

template <ui_events> struct ui_event_constraints {
  template <typename> static constexpr bool type_passes = true;
};
template <> struct ui_event_constraints<ui_events::mouse_button_down> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> pixel_coord;
    call::mouse_button(t);
  };
};
template <> struct ui_event_constraints<ui_events::mouse_button_up> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> pixel_coord;
    call::mouse_button(t);
  };
};
template <> struct ui_event_constraints<ui_events::mouse_move> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::position(t) } -> pixel_coord;
  };
};
template <> struct ui_event_constraints<ui_events::window_resized> {
  template <typename T>
  static constexpr bool type_passes = requires(T const &t) {
    { call::size_of(t) } -> size_wh;
  };
};

template <typename T, ui_events tEvt>
concept ui_event_c = ui_event_constraints<tEvt>::template type_passes<T>;

/// Return type for event deduction function. Use template parameter to indicate
/// what events a backend event *could* be, while using the return value for
/// what it actually *is*.
/// \tparam evts CGUI event types that an event could be.
template <ui_events... evts> struct subset_ui_events {
  static_assert(sizeof...(evts) > 0, "You must at least specify 1 event type");
  ui_events val;
  constexpr explicit(false) subset_ui_events(ui_events v) noexcept : val(v) {
    CGUI_ASSERT(((v == evts) || ...));
  }
  constexpr explicit(false) operator ui_events() const noexcept { return val; }

  template <ui_events tEvt>
  static consteval bool can_be_event(ui_event_identity<tEvt>) noexcept {
    return ((tEvt == evts) || ...);
  }
};

/// Single event optimisation specialisation, that lacks any member field, and
/// also does not need any input arguments.
/// \tparam evt Event type that this particular subset is.
template <ui_events evt> struct subset_ui_events<evt> {
  constexpr explicit(false) subset_ui_events(ui_events v) noexcept {
    assert(v == evt);
    unused(v);
  }
  constexpr subset_ui_events() noexcept = default;
  constexpr explicit(false) operator ui_events() const noexcept { return evt; }

  template <ui_events tEvt>
  static consteval bool can_be_event(ui_event_identity<tEvt> = {}) noexcept {
    return (tEvt == evt);
  }
};

template <typename T>
concept subset_ui_event_c = std::convertible_to<T, ui_events> &&
  requires()
{
  {
    std::remove_cvref_t<T>::can_be_event(ui_event_identity<ui_events::system>{})
  } -> std::convertible_to<bool>;
  {
    std::remove_cvref_t<T>::can_be_event(
        ui_event_identity<ui_events::mouse_move>{})
  } -> std::convertible_to<bool>;
  {
    std::remove_cvref_t<T>::can_be_event(
        ui_event_identity<ui_events::mouse_button_up>{})
  } -> std::convertible_to<bool>;
  {
    std::remove_cvref_t<T>::can_be_event(
        ui_event_identity<ui_events::mouse_button_down>{})
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

template <ui_events tEvt, typename T> consteval bool can_be_event() {
  if constexpr (has_event_type<T>) {
    using subset_t =
        std::remove_cvref_t<decltype(call::event_type(std::declval<T &&>()))>;
    return subset_t::can_be_event(ui_event_identity<tEvt>{});
  } else {
    return false;
  }
}
template <ui_events tEvt, typename T> constexpr bool is_event(T &&evt) {
  if constexpr (can_be_event<tEvt, T>()) {
    return static_cast<ui_events>(call::event_type(evt)) == tEvt;
  } else {
    unused(evt);
    return false;
  }
}

template <typename T, ui_events... tEvents>
concept event_types = (can_be_event<tEvents, T>() || ...);

template <ui_events> struct dummy_event;

template <ui_events tEvt>
constexpr subset_ui_events<tEvt> event_type(dummy_event<tEvt> const &) {
  return {};
}

template <> struct dummy_event<ui_events::system> {};
template <> struct dummy_event<ui_events::mouse_exit> {};
template <> struct dummy_event<ui_events::mouse_move> {
  default_pixel_coord pos;
};
template <> struct dummy_event<ui_events::mouse_button_down> {
  default_pixel_coord pos;
  mouse_buttons button_id;
};
template <> struct dummy_event<ui_events::mouse_button_up> {
  default_pixel_coord pos;
  mouse_buttons button_id;
};
template <> struct dummy_event<ui_events::window_resized> {
  default_size_wh sz;
};

template <typename> constexpr bool is_dummy_event_v = false;
template <ui_events tEvt>
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

using dummy_mouse_move_event = dummy_event<ui_events::mouse_move>;
using dummy_mouse_down_event = dummy_event<ui_events::mouse_button_down>;
using dummy_mouse_up_event = dummy_event<ui_events::mouse_button_up>;
using dummy_mouse_exit_event = dummy_event<ui_events::mouse_exit>;
using dummy_window_resized_event = dummy_event<ui_events::window_resized>;

struct cgui_mouse_exit_event {
  static constexpr subset_ui_events<ui_events::mouse_exit> event_type(auto &&) {
    return {};
  }
};

template <ui_events evt_val, typename F>
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
template <ui_events evt_v, typename T>
constexpr event_case_t<evt_v, std::remove_cvref_t<T>> event_case(T &&in) {
  return event_case_t<evt_v, std::remove_cvref_t<T>>{std::forward<T>(in)};
}

namespace impl {
template <typename> constexpr bool is_event_case_raw = false;
template <ui_events e, typename T>
constexpr bool is_event_case_raw<event_case_t<e, T>> = true;
template <typename T>
constexpr bool is_event_case = is_event_case_raw<std::remove_cvref_t<T>>;
} // namespace impl

template <has_event_type Evt, typename Data, ui_events... evt_vs,
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
template <has_event_type Evt, ui_events... evt_vs,
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
              decltype(evt), Data &&, decltype(args)... >) && ...)
  {
    using evt_t = decltype(evt);
    auto sf = bp::as_forward<decltype(self)>(self);
    auto ef = bp::as_forward<evt_t>(evt);
    if constexpr (sizeof...(args) == 0) {
      return call::apply_to(to_base(*sf), [ef](auto &&data, auto &&...cases) {
        ui_event_switch(*ef, std::forward<decltype(data)>(data),
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
template <typename Data, ui_events... evt_vs, typename... Fs>
constexpr ui_event_switch_t<std::unwrap_ref_decay_t<Data>,
                            event_case_t<evt_vs, Fs>...>
saved_ui_event_switch(Data &&d, event_case_t<evt_vs, Fs> &&...cases) {
  return {std::forward<Data>(d), std::move(cases)...};
}
} // namespace cgui

#endif

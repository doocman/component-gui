
#ifndef COMPONENT_GUI_CGUI_TYPES_HPP
#define COMPONENT_GUI_CGUI_TYPES_HPP

#include <algorithm>
#include <cassert>
#include <concepts>
#include <optional>
#include <ranges>
#include <ratio>
#include <tuple>
#include <utility>

#if __has_include("dooc/named_args_tuple.hpp")
#include <dooc/named_args_tuple.hpp>
#define CGUI_HAS_NAMED_ARGS 1
#endif

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/functional.hpp>
#include <cgui/std-backport/tuple.hpp>
#include <cgui/std-backport/type_traits.hpp>
#include <cgui/std-backport/utility.hpp>

#include <cgui/cgui-call.hpp>
#include <cgui/warnings.hpp>

namespace cgui {

template <typename T, typename... Ts>
concept builder = requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {
  call::build(*t, *args...);
};

template <typename...> struct build_result {};
template <typename... Ts, builder<Ts...> T> struct build_result<T, Ts...> {
  using type =
      decltype(call::build(std::declval<T &&>(), std::declval<Ts &&>()...));
};

template <typename T, typename... TArgs>
using build_result_t = typename build_result<T, TArgs...>::type;

template <typename T, typename TCB>
concept has_for_each = requires(bp::as_forward<T> t, bp::as_forward<TCB> cb) {
  call::for_each(*t, *cb);
};

template <typename T> class not_null {
  T v_;

public:
  constexpr explicit(false) not_null(T p) noexcept : v_(std::move(p)) {}
  not_null(std::nullptr_t) = delete;
  constexpr T const &value() const noexcept { return v_; }
  constexpr explicit(false) operator T const &() const noexcept {
    return value();
  }
  constexpr T &value() noexcept { return v_; }
  constexpr explicit(false) operator T &() noexcept { return value(); }
  constexpr decltype(auto) operator*() noexcept
    requires(bp::dereferencable<T &>)
  {
    return *v_;
  }
  constexpr decltype(auto) operator*() const noexcept
    requires(bp::dereferencable<T const &>)
  {
    return *v_;
  }
  constexpr decltype(auto) operator->() noexcept
    requires(bp::has_pointer_op<T &>)
  {
    return v_.operator->();
  }
  constexpr decltype(auto) operator->() const noexcept
    requires(bp::has_pointer_op<T const &>)
  {
    return v_.operator->();
  }
};

template <typename T> not_null(T *) -> not_null<T *>;

template <typename> struct pixel_type;

template <typename T>
concept pixel_coord_value_t = std::integral<T> || std::floating_point<T>;

template <typename T>
concept member_pixel_type = requires() { typename T::pixel_type; } &&
                            pixel_coord_value_t<typename T::pixel_type>;

template <member_pixel_type T> struct pixel_type<T> {
  using type = T::pixel_type;
};
template <typename T>
  requires(!bp::pure_value<T> && !member_pixel_type<T>)
struct pixel_type<T> : pixel_type<std::remove_cvref_t<T>> {};

template <typename T>
concept has_pixel_type = requires() { typename pixel_type<T>::type; } &&
                         pixel_coord_value_t<typename pixel_type<T>::type>;

template <typename T> using pixel_type_t = pixel_type<T>::type;

template <typename T>
concept pixel_coord_value_cv_t = pixel_coord_value_t<std::remove_cvref_t<T>>;

template <typename T>
concept pixel_coord = requires(T &&t) {
  { call::x_of(t) } -> pixel_coord_value_cv_t;
  { call::y_of(t) } -> pixel_coord_value_cv_t;
};

template <typename T, typename TVal>
concept pixel_coord_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      call::x_of(*t, *v);
      call::y_of(*t, *v);
    };
template <typename T, typename TVal>
concept pixel_coord_ref = requires(bp::as_forward<T> t) {
  { call::x_of(*t) } -> std::assignable_from<TVal>;
  { call::y_of(*t) } -> std::assignable_from<TVal>;
};
template <typename T, typename TVal>
concept pixel_coord_mut = pixel_coord_ref<T, TVal> || pixel_coord_set<T, TVal>;

template <typename T>
concept colour = requires(T &&t) {
  { call::red(t) } -> pixel_coord_value_cv_t;
  { call::blue(t) } -> pixel_coord_value_cv_t;
  { call::green(t) } -> pixel_coord_value_cv_t;
  { call::alpha(t) } -> pixel_coord_value_cv_t;
};

template <typename T> struct basic_default_pixel_coord {
  T x;
  T y;

  template <typename T2 = T>
    requires(!std::is_same_v<T2, std::remove_cvref_t<T2>>)
  constexpr explicit(false) operator std::remove_cvref_t<T>() const {
    return {.x = x, .y = y};
  }
};

using default_pixel_coord = basic_default_pixel_coord<int>;

template <typename T>
constexpr int x_of(basic_default_pixel_coord<T> const &c) {
  return c.x;
}
template <typename T>
constexpr int y_of(basic_default_pixel_coord<T> const &c) {
  return c.y;
}
template <typename T> constexpr int &x_of(basic_default_pixel_coord<T> &c) {
  return c.x;
}
template <typename T> constexpr int &y_of(basic_default_pixel_coord<T> &c) {
  return c.y;
}

template <typename T> struct basic_colour_t {
  T red, green, blue, alpha;
};
template <typename T> struct basic_rgb_t {
  T r, g, b;
  static constexpr auto &&red(auto &&c) {
    return std::forward<decltype(c)>(c).r;
  }
  static constexpr auto &&green(auto &&c) {
    return std::forward<decltype(c)>(c).r;
  }
  static constexpr auto &&blue(auto &&c) {
    return std::forward<decltype(c)>(c).r;
  }
  static constexpr T alpha(auto &&) { return std::numeric_limits<T>::max(); }
};

using default_colour_t = basic_colour_t<std::uint_least8_t>;
using default_rgb_t = basic_rgb_t<std::uint_least8_t>;

template <typename T> constexpr T &red(basic_colour_t<T> &c) noexcept {
  return c.red;
}
template <typename T> constexpr T red(basic_colour_t<T> const &c) noexcept {
  return c.red;
}
template <typename T> constexpr T &blue(basic_colour_t<T> &c) noexcept {
  return c.blue;
}
template <typename T> constexpr T blue(basic_colour_t<T> const &c) noexcept {
  return c.blue;
}
template <typename T> constexpr T &green(basic_colour_t<T> &c) noexcept {
  return c.green;
}
template <typename T> constexpr T green(basic_colour_t<T> const &c) noexcept {
  return c.green;
}
template <typename T> constexpr T &alpha(basic_colour_t<T> &c) noexcept {
  return c.alpha;
}
template <typename T> constexpr T alpha(basic_colour_t<T> const &c) noexcept {
  return c.alpha;
}

template <typename T, typename TFrom>
concept mutable_pixel_coord_value =
    bp::is_mutable_by<T, TFrom> && pixel_coord_value_cv_t<T>;

struct default_rect {
  default_pixel_coord tl;
  default_pixel_coord br;

  static constexpr default_rect from_xyxy(int x1, int y1, int x2, int y2) {
    return {{x1, y1}, {x2, y2}};
  }
};
constexpr default_pixel_coord top_left(default_rect const &r) noexcept {
  return r.tl;
}
constexpr default_pixel_coord &top_left(default_rect &r) noexcept {
  return r.tl;
}
constexpr default_pixel_coord bottom_right(default_rect const &r) noexcept {
  return r.br;
}
constexpr default_pixel_coord &bottom_right(default_rect &r) noexcept {
  return r.br;
}

template <typename T>
concept boolean_like = std::is_constructible_v<bool, T>;

template <typename T, typename TVal>
concept optional_like = boolean_like<T> && requires(T &&t) {
  { *std::forward<T>(t) } -> std::convertible_to<TVal &&>;
};

template <typename T, typename TChar>
concept basic_readable_text =
    std::ranges::input_range<T> &&
    std::is_same_v<TChar, std::remove_cvref_t<std::ranges::range_value_t<T>>>;

template <typename T>
concept readable_textc = basic_readable_text<T, char>;
template <typename T>
concept readable_textwc = basic_readable_text<T, wchar_t>;
template <typename T>
concept readable_text16 = basic_readable_text<T, char16_t>;
template <typename T>
concept readable_text32 = basic_readable_text<T, char32_t>;
#if __cpp_char8_t >= 201811L
template <typename T>
concept readable_text8 = basic_readable_text<T, char8_t>;
#elif CHAR_BIT == 8
template <typename T>
concept readable_text8 = basic_readable_text<T, char>;
#endif

enum class mouse_buttons {
  primary = 1,
  middle = 2,
  secondary = 3,
  // any other is backend defined here.
};

namespace call {
namespace impl {

struct do_mouse_button {
  constexpr std::convertible_to<mouse_buttons> auto
  operator()(auto &&operand) const
    requires(std::is_invocable_r_v<mouse_buttons, _do_mouse_button,
                                   decltype(operand)>)
  {
    return _do_mouse_button{}(std::forward<decltype(operand)>(operand));
  }
};

}; // namespace impl

inline constexpr impl::do_mouse_button mouse_button;
} // namespace call

template <typename T>
concept size_wh = requires(T const &t) {
  { call::width(t) } -> bp::not_void;
  { call::height(t) } -> bp::not_void;
};

template <typename T = int> struct basic_size_wh {
  T w;
  T h;

  static constexpr auto &&width(bp::cvref_type<basic_size_wh> auto &&wh) {
    return std::forward<decltype(wh)>(wh).w;
  }
  static constexpr auto &&height(bp::cvref_type<basic_size_wh> auto &&wh) {
    return std::forward<decltype(wh)>(wh).h;
  }
};
using default_size_wh = basic_size_wh<int>;

template <typename T, typename... TArgs>
concept has_handle =
    requires(bp::as_forward<T> t, bp::as_forward<TArgs>... args) {
      call::handle(*t, *args...);
    };
template <typename T, typename... TArgs>
concept has_set_state =
    requires(bp::as_forward<T> t, bp::as_forward<TArgs>... args) {
      call::set_state(*t, *args...);
    };

constexpr auto multiply_alpha(colour auto c, std::uint_least8_t alpha) {
  call::alpha(c,
              static_cast<std::uint_least8_t>((call::alpha(c) * alpha) / 255));
  return c;
}

template <typename T>
concept bounding_box_xxyy = requires(T const &t) {
  { call::l_x(t) } -> pixel_coord_value_cv_t;
  { call::t_y(t) } -> pixel_coord_value_cv_t;
  { call::r_x(t) } -> pixel_coord_value_cv_t;
  { call::b_y(t) } -> pixel_coord_value_cv_t;
};
template <typename T, typename TFrom>
concept bounding_box_xxyy_mut = requires(bp::as_forward<T> t) {
  { call::l_x(*t) } -> mutable_pixel_coord_value<TFrom>;
  { call::t_y(*t) } -> mutable_pixel_coord_value<TFrom>;
  { call::r_x(*t) } -> mutable_pixel_coord_value<TFrom>;
  { call::b_y(*t) } -> mutable_pixel_coord_value<TFrom>;
};
template <typename T, typename TVal>
concept bounding_box_xxyy_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      call::l_x(*t, *v);
      call::t_y(*t, *v);
      call::r_x(*t, *v);
      call::b_y(*t, *v);
    };

template <typename T>
concept bounding_box_xwyh = requires(T const &t) {
  { call::l_x(t) } -> pixel_coord_value_cv_t;
  { call::width(t) } -> pixel_coord_value_cv_t;
  { call::t_y(t) } -> pixel_coord_value_cv_t;
  { call::height(t) } -> pixel_coord_value_cv_t;
};

template <typename T, typename TFrom>
concept bounding_box_xwyh_mut = requires(bp::as_forward<T> t) {
  { call::l_x(*t) } -> mutable_pixel_coord_value<TFrom>;
  { call::width(*t) } -> mutable_pixel_coord_value<TFrom>;
  { call::t_y(*t) } -> mutable_pixel_coord_value<TFrom>;
  { call::height(*t) } -> mutable_pixel_coord_value<TFrom>;
};
template <typename T, typename TVal>
concept bounding_box_xwyh_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      call::l_x(*t, *v);
      call::width(*t, *v);
      call::t_y(*t, *v);
      call::height(*t, *v);
    };

template <typename T>
concept bounding_box_coord = requires(T const &t) {
  { call::top_left(t) } -> pixel_coord;
  { call::bottom_right(t) } -> pixel_coord;
};
template <typename T, typename TFrom>
concept bounding_box_coord_mut = requires(bp::as_forward<T> t) {
  { call::top_left(*t) } -> pixel_coord_mut<TFrom>;
  { call::bottom_right(*t) } -> pixel_coord_mut<TFrom>;
};
template <typename T, typename TVal>
concept bounding_box_coord_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      call::top_left(*t, *v);
      call::bottom_right(*t, *v);
    };

template <typename T>
concept bounding_box =
    bounding_box_coord<T> || bounding_box_xwyh<T> || bounding_box_xxyy<T>;
template <typename T, typename TFrom>
concept mutable_bounding_box =
    bounding_box_coord_mut<T, TFrom> || bounding_box_xwyh_mut<T, TFrom> ||
    bounding_box_xxyy_mut<T, TFrom> || bounding_box_coord_set<T, TFrom> ||
    bounding_box_xwyh_set<T, TFrom> || bounding_box_xxyy_set<T, TFrom>;

template <typename T, typename... TArgs>
concept set_state_with_rerender =
    has_set_state<T, TArgs...> &&
    requires(bp::as_forward<T> t, bp::as_forward<TArgs>... args) {
      { call::set_state(*t, *args...) } -> bounding_box;
    };

template <typename T, typename TBox = default_rect>
concept display_state_callbacks =
    bounding_box<TBox> && requires(T &t, TBox const &cbox) {
      t.rerender();
      t.rerender(cbox);
    };

constexpr auto x_view(bounding_box auto &&b) {
  return std::views::iota(call::l_x(b), call::r_x(b));
}
constexpr auto y_view(bounding_box auto &&b) {
  return std::views::iota(call::t_y(b), call::b_y(b));
}

template <typename T, typename TCoord = default_pixel_coord,
          typename TColour = default_colour_t>
concept single_pixel_draw = pixel_coord<TCoord> && colour<TColour> &&
                            std::invocable<T, TCoord, TColour>;
template <typename T, typename TCoord = default_pixel_coord>
concept single_alpha_draw =
    pixel_coord<TCoord> && std::invocable<T, TCoord, std::uint_least8_t>;
struct dummy_pixel_drawer {
  constexpr void operator()(pixel_coord auto &&, colour auto &&) {}
};
static_assert(single_pixel_draw<dummy_pixel_drawer>);
struct dummy_alpha_drawer {
  constexpr void
  operator()(pixel_coord auto &&,
             std::convertible_to<std::uint_least8_t> auto &&) const {}
};
static_assert(single_alpha_draw<dummy_alpha_drawer>);

template <typename T, typename TCB = dummy_pixel_drawer>
concept canvas_pixel_callback =
    single_pixel_draw<TCB> && std::invocable<T, TCB>;

template <typename T, typename TRect = default_rect,
          typename TCB = dummy_pixel_drawer>
concept pixel_draw_callback = bounding_box<TRect> && single_pixel_draw<TCB> &&
                              std::invocable<T, TRect, TCB>;
template <typename T, typename TRect = default_rect,
          typename TCB = dummy_alpha_drawer>
concept alpha_draw_callback = bounding_box<TRect> && single_alpha_draw<TCB> &&
                              std::invocable<T, TRect, TCB>;

struct dummy_pixel_draw_callback {
  constexpr void operator()(bounding_box auto &&,
                            single_pixel_draw auto &&) const {}
};
static_assert(pixel_draw_callback<dummy_pixel_draw_callback>);
struct dummy_alpha_draw_callback {
  constexpr void operator()(bounding_box auto &&,
                            single_alpha_draw auto &&) const {}
};
static_assert(alpha_draw_callback<dummy_alpha_draw_callback>);

template <typename T, typename TArea = default_rect,
          typename TDrawPixels = dummy_pixel_draw_callback,
          typename TDrawAlpha = dummy_alpha_draw_callback,
          typename TColour = default_colour_t>
concept renderer = requires(T &t, TArea const &a, TDrawPixels &&pixel_cb,
                            TDrawAlpha &&alpha_cb, TColour const &col) {
  call::draw_pixels(t, a, pixel_cb);
  call::draw_alpha(t, a, alpha_cb);
  t.sub(a);
  t.sub(a, col);
  t.with(col);
};

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

template <typename T, typename... TVals>
concept has_native_fill =
    requires(bp::as_forward<T> t, bp::as_forward<TVals>... args) {
      call::fill(*t, *args...);
    };

template <typename T, typename... TVals>
concept has_draw_pixels =
    requires(bp::as_forward<T> t, bp::as_forward<TVals>... args) {
      call::draw_pixels(*t, *args...);
    };

template <typename T>
concept has_event_type =
    requires(bp::as_forward<T> t) { call::event_type(*t); };

template <colour TC> struct fill_on_draw_pixel {
  TC c;
  constexpr void
  operator()(bounding_box auto &&b,
             single_pixel_draw<default_pixel_coord, TC> auto &&cb) const {
    for (auto y : y_view(b)) {
      for (auto x : x_view(b)) {
        cb(default_pixel_coord{x, y}, c);
      }
    }
  }
};

constexpr auto fill =
    []<typename T, bounding_box TB, colour TC>(T &&v, TB const &b, TC const &c)
  requires(has_native_fill<T, TB, TC> ||
           has_draw_pixels<T, TB, fill_on_draw_pixel<TC>>)
{
  auto vf = bp::as_forward<decltype(v)>(v);
  if constexpr (has_native_fill<T, TB, TC>) {
    return call::fill(*vf, b, c);
  } else {
    return call::draw_pixels(*vf, b, fill_on_draw_pixel<TC>{c});
  }
};

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
using dummy_window_resized_event = dummy_event<ui_events::window_resized>;

struct cgui_mouse_exit_event {
  static constexpr subset_ui_events<ui_events::mouse_exit> event_type(auto &&) {
    return {};
  }
};

namespace call {
namespace impl {

template <typename T, typename TC>
concept has_from_xyxy = requires(bp::as_forward<TC> v) {
  { std::remove_cvref_t<T>::from_xyxy(*v, *v, *v, *v) } -> bounding_box;
};
template <typename T, typename TC>
concept has_from_xywh = requires(bp::as_forward<TC> v) {
  { std::remove_cvref_t<T>::from_xywh(*v, *v, *v, *v) } -> bounding_box;
};
template <typename T, typename TC>
concept has_from_tlbr_ils = requires(bp::as_forward<TC> v) {
  { std::remove_cvref_t<T>::from_tlbr({*v, *v}, {*v, *v}) } -> bounding_box;
};
template <typename T, typename TC>
concept has_from_tlbr = requires(bp::as_forward<TC> v) {
  { std::remove_cvref_t<T>::from_tlbr(*v, *v) } -> bounding_box;
};
template <typename T, typename TC>
concept has_bbox_init =
    has_from_xyxy<T, TC> || has_from_xywh<T, TC> ||
    has_from_tlbr<T, basic_default_pixel_coord<std::remove_cvref_t<TC>>> ||
    has_from_tlbr_ils<T, TC>;

struct do_from_xyxy {
  template <typename TXY, has_bbox_init<TXY> T>
  constexpr bounding_box auto operator()(std::type_identity<T> const &, TXY xl,
                                         TXY yt, TXY xr, TXY yb) const {
    using raw_t = std::remove_cvref_t<T>;
    if constexpr (has_from_xyxy<T, TXY>) {
      return raw_t::from_xyxy(std::move(xl), std::move(yt), std::move(xr),
                              std::move(yb));
    } else if constexpr (has_from_xywh<T, TXY>) {
      return raw_t::from_xywh(std::move(xl), std::move(yt), xr - xl, yb - yt);
    } else if constexpr (has_from_tlbr_ils<T, TXY>) {
      return raw_t::from_tlbr({std::move(xl), std::move(yt)},
                              {std::move(xr), std::move(yb)});
    } else {
      return raw_t::from_tlbr(
          basic_default_pixel_coord<TXY>(std::move(xl), std::move(yt)),
          basic_default_pixel_coord<TXY>(std::move(xr), std::move(yb)));
    }
  }
};
struct do_from_xywh {
  template <typename TXY, has_bbox_init<TXY> T>
  constexpr bounding_box auto operator()(std::type_identity<T> const &ti, TXY x,
                                         TXY y, TXY w, TXY h) const {
    if constexpr (has_from_xywh<T, TXY>) {
      return T::from_xywh(x, y, w, h);
    } else {
      return do_from_xyxy{}(ti, x, y, x + w, y + h);
    }
  }
};
struct do_from_tlbr {
  template <pixel_coord TC, typename T>
    requires(has_from_tlbr<T, TC> ||
             has_bbox_init<T, decltype(call::x_of(std::declval<TC &&>()))>)
  constexpr bounding_box auto operator()(std::type_identity<T> const &ti,
                                         TC &&tl, TC &&br) const {
    if constexpr (has_from_tlbr<T, TC>) {
      return T::from_tlbr(std::forward<TC>(tl), std::forward<TC>(br));
    } else {
      return do_from_xyxy{}(ti, call::x_of(tl), call::y_of(tl), call::x_of(br),
                            call::y_of(br));
    }
  }
};
}; // namespace impl

template <typename T, typename TXY>
constexpr auto box_from_xyxy(TXY xl, TXY yt, std::type_identity_t<TXY> xr,
                             std::type_identity_t<TXY> yb,
                             std::type_identity<T> = {}) {
  if constexpr (impl::has_bbox_init<T, TXY>) {
    return impl::do_from_xyxy{}(std::type_identity<T>{}, xl, yt, xr, yb);
  } else {
    return impl::do_from_xyxy{}(std::type_identity<extend_api_t<T>>{}, xl, yt,
                                xr, yb);
  }
}
template <typename T, typename TXY>
constexpr auto box_from_xywh(TXY x, TXY y, TXY w, TXY h,
                             std::type_identity<T> = {}) {
  if constexpr (impl::has_bbox_init<T, TXY>) {
    return impl::do_from_xywh{}(std::type_identity<T>{}, x, y, w, h);
  } else {
    return impl::do_from_xywh{}(std::type_identity<extend_api_t<T>>{}, x, y, w,
                                h);
  }
}
template <typename T, typename TC>
constexpr auto box_from_tlbr(TC &&tl, TC &&br, std::type_identity<T> = {}) {
  if constexpr (impl::has_from_tlbr<T, TC> ||
                impl::has_bbox_init<T, decltype(call::x_of(tl))>) {
    return impl::do_from_tlbr{}(std::type_identity<T>{}, std::forward<TC>(tl),
                                std::forward<TC>(br));
  } else {
    return impl::do_from_tlbr{}(std::type_identity<extend_api_t<T>>{},
                                std::forward<TC>(tl), std::forward<TC>(br));
  }
}
template <typename T, typename TX>
concept mut_box_pointer =
    bp::pointer_type<T> && mutable_bounding_box<bp::dereferenced_t<T>, TX>;

template <typename T, typename TV1, typename TV2>
concept mut_box_pair =
    (mut_box_pointer<T, TV1> || call::is_placeholder_v<TV1>)&&(
        mut_box_pointer<T, TV2> || call::is_placeholder_v<TV2>);

template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T, typename TTL,
          typename TBR>
constexpr void _set_xx_or_yy(T b, TV1 tl, TV2 br, TTL getset1, TBR getset2) {
  if constexpr (is_placeholder_v<TV1>) {
    _set_xx_or_yy(b, tl(getset1, *b), br, getset1, getset2);
  } else if constexpr (is_placeholder_v<TV2>) {
    _set_xx_or_yy(b, tl, br(getset2, *b), getset1, getset2);
  } else {
    getset1(*b, tl);
    getset2(*b, br);
  }
}

template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_xx(T b, TV1 lx, TV2 rx) {
  _set_xx_or_yy(b, lx, rx, call::l_x, call::r_x);
}
template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_yy(T b, TV1 ty, TV2 by) {
  _set_xx_or_yy(b, ty, by, call::t_y, call::b_y);
}
template <bounding_box TB, pixel_coord TC = default_pixel_coord>
constexpr auto move_tl_to(TB b, TC tl) {
  auto w = call::width(b);
  auto h = call::height(b);
  call::l_x(b, call::x_of(tl));
  call::t_y(b, call::y_of(tl));
  call::width(b, w);
  call::height(b, h);
  return b;
}

template <typename TX, mut_box_pointer<TX> T>
constexpr auto split_x(T b, TX x) {
  assert(b != nullptr);
  auto res = call::box_from_xyxy<bp::dereferenced_t<T>>(
      x, call::t_y(*b), call::r_x(*b), call::b_y(*b));
  call::r_x(*b, x);
  return res;
}
template <typename TY, mut_box_pointer<TY> T>
constexpr auto split_y(T b, TY y) {
  assert(b != nullptr);
  auto res = call::box_from_xyxy<bp::dereferenced_t<T>>(
      call::l_x(*b), y, call::r_x(*b), call::b_y(*b));
  call::b_y(*b, y);
  return res;
}

template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_left(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::width(b));
  auto org_lx = call::l_x(b);
  auto split_x = static_cast<decltype(org_lx)>(org_lx + v);
  call::set_xx(&b, split_x, keep_current);
  return call::box_from_xyxy<bp::dereferenced_t<T>>(org_lx, call::t_y(b),
                                                    split_x, call::b_y(b));
}
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_above(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::height(b));
  auto org_y = call::t_y(b);
  auto split_y = static_cast<decltype(org_y)>(org_y + v);
  call::set_yy(&b, split_y, keep_current);
  return call::box_from_xyxy<bp::dereferenced_t<T>>(call::l_x(b), org_y,
                                                    call::r_x(b), split_y);
}
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_right(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::width(b));
  call::width(b, call::width(b) - v);
  return call::box_from_xywh<bp::dereferenced_t<T>>(call::r_x(b), call::t_y(b),
                                                    v, call::height(b));
}
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_below(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::height(b));
  call::height(b, call::height(b) - v);
  return call::box_from_xywh<bp::dereferenced_t<T>>(call::l_x(b), call::b_y(b),
                                                    call::width(b), v);
}

constexpr bool valid_box(bounding_box auto const &b) {
  return (call::width(b) >= 0) && (call::height(b) >= 0);
}

template <typename TRes = void, bounding_box T1, bounding_box T2>
constexpr auto box_union(T1 const &b1, T2 const &b2) {
  assert(call::valid_box(b1));
  assert(call::valid_box(b2));
  using result_t =
      std::conditional_t<std::is_void_v<TRes>, std::common_type<T1, T2>,
                         std::type_identity<TRes>>::type;
  return call::box_from_xyxy<result_t>(std::min(call::l_x(b1), call::l_x(b2)),
                                       std::min(call::t_y(b1), call::t_y(b2)),
                                       std::max(call::r_x(b1), call::r_x(b2)),
                                       std::max(call::b_y(b1), call::b_y(b2)));
}

template <typename TRes = void, bounding_box T1, bounding_box T2>
constexpr auto box_intersection(T1 const &b1, T2 const &b2) {
  CGUI_ASSERT(call::valid_box(b1));
  CGUI_ASSERT(call::valid_box(b2));
  using result_t =
      std::conditional_t<std::is_void_v<TRes>, std::common_type<T1, T2>,
                         std::type_identity<TRes>>::type;
  return call::box_from_xyxy<result_t>(std::max(call::l_x(b1), call::l_x(b2)),
                                       std::max(call::t_y(b1), call::t_y(b2)),
                                       std::min(call::r_x(b1), call::r_x(b2)),
                                       std::min(call::b_y(b1), call::b_y(b2)));
}

constexpr auto nudge_left(pixel_coord auto c, auto &&val) {
  call::x_of(c, call::x_of(c) - val);
  return c;
}
constexpr auto nudge_right(pixel_coord auto c, auto &&val) {
  return nudge_left(c, -val);
}
constexpr auto nudge_up(pixel_coord auto c, auto &&val) {
  call::y_of(c, call::y_of(c) - val);
  return c;
}
constexpr auto nudge_down(pixel_coord auto c, auto &&val) {
  return nudge_up(c, -val);
}

constexpr auto nudge_left(bounding_box auto b, auto &&val) {
  call::set_xx(&b, call::l_x(b) - val, call::r_x(b) - val);
  return std::forward<decltype(b)>(b);
}
constexpr auto nudge_right(bounding_box auto b, auto &&val) {
  return nudge_left(b, -val);
}
constexpr auto nudge_up(bounding_box auto b, auto &&val) {
  call::set_yy(&b, call::t_y(b) - val, call::b_y(b) - val);
  return std::forward<decltype(b)>(b);
}
constexpr auto nudge_down(bounding_box auto b, auto &&val) {
  return nudge_up(b, -val);
}

template <typename T, typename TX>
concept range_condition = requires(T t, TX v) {
  { t(v, v, v) } -> std::convertible_to<bool>;
};

inline constexpr auto inside_open_range = [](auto &&c, auto &&min, auto &&max) {
  return (min < c) && (c < max);
};
inline constexpr auto inside_closed_range =
    [](auto &&c, auto &&min, auto &&max) { return (min <= c) && (c <= max); };
inline constexpr auto inside_semiopen_range =
    [](auto &&c, auto &&min, auto &&max) { return (min <= c) && (c < max); };

using inside_open_range_t = decltype(inside_open_range);
using inside_closed_range_t = decltype(inside_closed_range);
using inside_semiopen_range_t = decltype(inside_semiopen_range);

template <bounding_box TB, pixel_coord TC = default_pixel_coord,
          range_condition<decltype(call::x_of(std::declval<TC>()))> TRC =
              inside_semiopen_range_t>
constexpr bool hit_box(TB const &b, TC const &c, TRC &&inside_range = {}) {
  assert(call::valid_box(b));
  return inside_range(call::x_of(c), call::l_x(b), call::r_x(b)) &&
         inside_range(call::y_of(c), call::t_y(b), call::b_y(b));
};

template <bounding_box TB1, bounding_box TB2>
constexpr bool box_includes_box(TB1 const &outer, TB2 const &inner) {
  return call::hit_box(outer, call::top_left(inner), inside_closed_range) &&
         call::hit_box(outer, call::bottom_right(inner), inside_closed_range);
}

} // namespace call

constexpr bool empty_box(bounding_box auto const &b) {
  return call::width(b) == 0 || call::height(b) == 0;
}

template <bounding_box T, bounding_box T2> constexpr T copy_box(T2 &&b) {
  if constexpr (bp::cvref_type<T2, T>) {
    return std::forward<T2>(b);
  } else if constexpr (std::constructible_from<T, T2 &&>) {
    return T(std::forward<T2>(b));
  } else if constexpr (call::impl::has_from_xywh<T, decltype(call::l_x(b))>) {
    return call::box_from_xywh<T>(call::l_x(b), call::t_y(b), call::width(b),
                                  call::height(b));
  } else {
    return call::box_from_xyxy<T>(call::l_x(b), call::t_y(b), call::r_x(b),
                                  call::b_y(b));
  }
}

template <typename TRes = void, typename TB1, typename TB2>
constexpr auto box_add(TB1 const &b1, TB2 const &b2)
    -> decltype(call::box_union<TRes>(b1, b2)) {
  using result_t = decltype(call::box_union<TRes>(b1, b2));
  if (empty_box(b1)) {
    return copy_box<result_t>(b2);
  }
  if (empty_box(b2)) {
    return copy_box<result_t>(b1);
  }
  return call::box_union<TRes>(b1, b2);
}

template <typename>
concept render_args = true;

template <typename T>
concept state_marker = requires(T const &t) {
  { t.current_state() } -> bp::not_void;
};
template <typename T>
concept widget_states_aspect = requires(T const &t) {
  { call::state(t) } -> state_marker;
};

template <std::equality_comparable T, T... values> class widget_state_marker {
  T value_;

public:
  constexpr explicit(false) widget_state_marker(T val) : value_(val) {
    CGUI_ASSERT(((value_ == values) || ...));
  }
  constexpr widget_state_marker()
    requires(sizeof...(values) == 1)
      : value_(values...) {}
  constexpr T const &current_state() const noexcept { return value_; }
};
template <typename T> class widget_state_marker<T> {
public:
  static constexpr T current_state() noexcept { return T{}; }
};
template <typename T, T value> class widget_state_marker<T, value> {
public:
  constexpr widget_state_marker() noexcept = default;
  constexpr explicit(false) widget_state_marker(T v) noexcept {
    CGUI_ASSERT(v == value);
    unused(v);
  }
  static constexpr T current_state() noexcept { return T{}; }
};

using no_state_t = widget_state_marker<int>;

template <typename T, T... tS1, T... tS2>
constexpr bool operator==(widget_state_marker<T, tS1...> const &l,
                          widget_state_marker<T, tS2...> const &r) {
  return l.current_state() == r.current_state();
}

template <typename T, T... values> struct widget_states {
  static constexpr std::size_t size() { return sizeof...(values); }
};

template <typename> struct all_states_in_marker {};
template <typename T, T... values>
struct all_states_in_marker<widget_state_marker<T, values...>> {
  using type = widget_states<T, values...>;
};
template <typename T>
using all_states_in_marker_t = typename all_states_in_marker<T>::type;

template <typename T, T... values>
constexpr std::size_t state2index(widget_state_marker<T, values...> const &v) {
  if constexpr (sizeof...(values) < 2) {
    return 0;
  } else {
    auto const i = static_cast<std::size_t>(v.current_state());
    CGUI_ASSERT(i < sizeof...(values));
    return i;
  }
}

template <typename TWH = int, typename TState = no_state_t>
class widget_render_args : TState {
  TWH w_;
  TWH h_;

public:
  template <typename... Ts>
    requires(std::constructible_from<TState, Ts && ...>)
  constexpr widget_render_args(TWH w, TWH h, Ts &&...state_args)
      : TState(std::forward<Ts>(state_args)...), w_(w), h_(h) {}

  constexpr TWH width() const noexcept { return w_; }
  constexpr TWH height() const noexcept { return h_; }
  constexpr auto const &widget_state() const {
    return static_cast<TState const &>(*this);
  }
};
template <typename TWH, typename TState>
widget_render_args(TWH, TWH, TState &&)
    -> widget_render_args<TWH, std::remove_cvref_t<TState>>;
template <typename TWH> widget_render_args(TWH, TWH) -> widget_render_args<TWH>;

template <typename T>
concept canvas = requires(T const &tc) {
  { call::area(tc) } -> bounding_box;
};

template <typename T, typename TR, typename... Ts>
concept has_render = call::impl::has_render<T, TR, Ts...>;

constexpr auto center(bounding_box auto const &b) {
  auto tl = call::top_left(b);
  auto br = call::bottom_right(b);
  return default_pixel_coord{(x_of(br) - x_of(tl)) / 2,
                             (y_of(br) - y_of(tl)) / 2};
}

struct dummy_canvas {
  constexpr default_rect area() const { return {}; }
};

struct dummy_renderer {
  constexpr void draw_pixels(bounding_box auto const &,
                             pixel_draw_callback auto &&) {}
  constexpr void draw_alpha(bounding_box auto const &,
                            alpha_draw_callback auto &&) {}
  constexpr dummy_renderer with(colour auto &&) const { return {}; }
  constexpr dummy_renderer sub(bounding_box auto &&, colour auto &&) const {
    return {};
  }
  constexpr dummy_renderer sub(bounding_box auto &&) const { return {}; }
};

template <typename T, typename TRender = dummy_renderer>
concept widget_display = has_render<T, TRender>;
template <typename T, typename TRender = dummy_renderer>
concept widget_display_builder = builder<T> && requires(bp::as_forward<T> t) {
  { (*t).build() } -> widget_display<TRender>;
};
template <typename T, typename TRender = dummy_renderer>
concept widget_display_args =
    widget_display<T, TRender> ||
    widget_display<std::unwrap_ref_decay_t<T>, TRender> ||
    widget_display_builder<T, TRender>;

template <typename TRender> struct _widget_display_filter {
  constexpr void operator()(widget_display_args<TRender> auto &&) const {}
};
template <typename T, typename TRender = dummy_renderer>
concept widget_display_range = has_for_each<T, _widget_display_filter<TRender>>;

template <typename T, typename TRender = dummy_renderer,
          typename TArgs = widget_render_args<>>
concept display_component = has_render<T, TRender, TArgs>;
template <typename T, typename TRender = dummy_renderer,
          typename TArgs = widget_render_args<>,
          typename TSzT = widget_states<std::size_t, 0u>>
concept built_display_concept =
    builder<T, TSzT> &&
    display_component<build_result_t<T, TSzT>, TRender, TArgs>;
template <typename T, typename TRender = dummy_renderer,
          typename TArgs = widget_render_args<>,
          typename TSzT = widget_states<std::size_t, 0u>>
concept builder_display_args =
    display_component<T, TRender, TArgs> ||
    display_component<std::unwrap_ref_decay_t<T>, TRender, TArgs> ||
    built_display_concept<T, TRender, TArgs, TSzT>;

template <typename TRender, typename TArgs>
struct _display_component_range_concept_filter {
  template <typename T>
    requires(display_component<T, TRender, TArgs> ||
             display_component<std::remove_cvref_t<T>, TRender, TArgs>)
  constexpr void operator()(T &&) const {}
};
template <typename T, typename TRender = dummy_renderer,
          typename TArgs = widget_render_args<>>
concept display_component_range =
    has_for_each<T, _display_component_range_concept_filter<TRender, TArgs>>;

template <typename T, typename TRenderer = dummy_renderer>
concept font_glyph = requires(T const &ct, TRenderer &r) {
  call::base_to_top(ct);
  call::advance_x(ct);
  call::advance_y(ct);
  call::render(ct, r);
  call::pixel_area(ct);
};
template <typename T, typename TChar = char>
concept font_face = requires(bp::as_forward<T> t, TChar c) {
  call::glyph(*t, c);
  { *call::glyph(*t, c) } -> font_glyph;
  { call::full_height(*t) } -> std::convertible_to<long>;
  { call::ascender(*t) } -> std::convertible_to<long>;
};

struct position {
  long x;
  long y;
};
struct no_auto_cleanup_t {};
inline constexpr no_auto_cleanup_t no_auto_cleanup;
struct auto_cleanup_t {};
inline constexpr auto_cleanup_t auto_cleanup;

template <typename TQuit, typename... TArgs> struct cleanup_object_t {
  using value_type = std::tuple<TArgs...>;
  static constexpr bool _all_pointers =
      (std::is_pointer_v<TArgs> && ...) && sizeof...(TArgs);
  using wrapper_t =
      std::conditional_t<_all_pointers, value_type, std::optional<value_type>>;
  wrapper_t values;

private:
  constexpr value_type &_values() {
    if constexpr (_all_pointers) {
      return values;
    } else {
      assert(has_value());
      return *values;
    }
  }
  void cleanup() {
    if (has_value()) {
      std::apply(TQuit{}, _values());
    }
  }

public:
  constexpr cleanup_object_t() = default;
  constexpr explicit cleanup_object_t(TArgs... args)
    requires(sizeof...(TArgs) > 0 &&
             (std::is_copy_constructible_v<TArgs> && ...))
      : values(std::tuple<TArgs...>(std::move(args)...)) {}
  constexpr explicit(false) cleanup_object_t(std::nullptr_t)
    requires(_all_pointers)
      : values() {}
  constexpr explicit cleanup_object_t(no_auto_cleanup_t) : values() {}
  constexpr explicit cleanup_object_t(std::in_place_t, auto &&...args)
      : values(std::in_place, std::forward<decltype(args)>(args)...) {}
  constexpr cleanup_object_t(cleanup_object_t &&other) noexcept
      : values(std::exchange(other.values, wrapper_t())) {}
  constexpr cleanup_object_t &operator=(cleanup_object_t &&other) noexcept {
    std::swap(values, other.values);
    return *this;
  }
  constexpr ~cleanup_object_t() { cleanup(); }
  constexpr void reset() {
    cleanup();
    values = wrapper_t();
  }
  constexpr void reset(std::convertible_to<value_type> auto &&v) noexcept {
    cleanup();
    values = std::forward<decltype(v)>(v);
  }
  constexpr void reset(std::convertible_to<TArgs> auto &&...v) noexcept
    requires(sizeof...(v) > 1)
  {
    cleanup();
    values = value_type(std::forward<decltype(v)>(v)...);
  }
  constexpr bool has_value() const noexcept {
    if constexpr (_all_pointers) {
      return std::get<0>(values) != nullptr;
    } else {
      return values.has_value();
    }
  }
  constexpr operator bool() const noexcept { return has_value(); }
  template <typename = void>
    requires(sizeof...(TArgs) > 0)
  [[nodiscard]] constexpr auto &first_value() noexcept {
    return std::get<0>(_values());
  }
  template <typename = void>
    requires(sizeof...(TArgs) > 0)
  [[nodiscard]] constexpr auto const &first_value() const noexcept {
    return std::get<0>(values);
  }
  constexpr void swap(cleanup_object_t &r) noexcept {
    std::swap(values, r.values);
  }
  [[nodiscard]] constexpr auto release() noexcept {
    return std::exchange(values, wrapper_t{});
  }
  constexpr auto operator->() const
    requires(sizeof...(TArgs) == 1 && _all_pointers)
  {
    return first_value();
  }
};

namespace details {
template <typename TRatioFrom, typename TRatioTo>
constexpr bool convert_without_loss =
    TRatioFrom::num == TRatioTo::num && TRatioFrom::den <= TRatioTo::den;
}

template <typename TRatio, typename TRep = int> class font_point {
  TRep value_{};

public:
  constexpr font_point() = default;
  constexpr font_point(font_point const &) = default;
  constexpr font_point(font_point &&) noexcept = default;
  constexpr explicit font_point(TRep v) : value_(v) {}
  template <typename TRep2, typename TRatio2>
  constexpr explicit(!std::convertible_to<TRep2, TRep> ||
                     !details::convert_without_loss<TRatio2, TRatio>)
      font_point(font_point<TRatio2, TRep2> const &fp)
      : value_(static_cast<TRep>(fp.count())) {}

  constexpr font_point &operator=(font_point const &) = default;
  constexpr font_point &operator=(font_point &&) noexcept = default;

  template <typename TRep2, typename TRatio2>
    requires(std::convertible_to<TRep2, TRep> &&
             details::convert_without_loss<TRatio2, TRatio>)
  constexpr font_point &operator=(font_point<TRatio2, TRep2> const &fp) {
    using conv_ratio = std::ratio_divide<TRatio, TRatio2>;
    value_ =
        static_cast<TRep>((fp.count() * conv_ratio::num) / conv_ratio::den);
    return *this;
  }

  constexpr TRep count() const { return value_; }
};

using whole_point = font_point<std::ratio<1, 1>>;

constexpr default_colour_t to_default_colour(colour auto &&c) {
  return {call::red(c), call::green(c), call::blue(c), call::alpha(c)};
}
constexpr default_colour_t const &to_default_colour(default_colour_t const &c) {
  return c;
}
constexpr default_colour_t &to_default_colour(default_colour_t &c) { return c; }
constexpr default_colour_t const &&
to_default_colour(default_colour_t const &&c) {
  return std::move(c);
}
constexpr default_colour_t &&to_default_colour(default_colour_t &&c) {
  return std::move(c);
}

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_TYPES_HPP

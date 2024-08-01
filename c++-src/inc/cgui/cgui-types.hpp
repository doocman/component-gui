
#ifndef COMPONENT_GUI_CGUI_TYPES_HPP
#define COMPONENT_GUI_CGUI_TYPES_HPP

#include <cassert>
#include <concepts>
#include <optional>
#include <ranges>
#include <ratio>
#include <tuple>
#include <utility>

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui {
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

namespace extend {

#define CGUI_EXTRA_PARAMS
#define CGUI_EXTRA_ARGS
#define CGUI_EXTRA_ARGS_COMMA
#define CGUI_PIX_FUNC_IMPL(NAME, CONCEPT)                                      \
  namespace ns_lookup {                                                        \
  inline void NAME() {}                                                        \
  template <typename T>                                                        \
  concept member_##NAME = requires(T const &tc CGUI_EXTRA_PARAMS) {            \
    { tc.NAME(CGUI_EXTRA_ARGS) } -> CONCEPT;                                   \
  };                                                                           \
  template <typename T>                                                        \
  concept free_##NAME = requires(T const &tc CGUI_EXTRA_PARAMS) {              \
    { NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS) } -> CONCEPT;             \
  };                                                                           \
  template <typename T>                                                        \
  concept has_##NAME = member_##NAME<T> || free_##NAME<T>;                     \
  constexpr decltype(auto) do_##NAME(has_##NAME auto &&t CGUI_EXTRA_PARAMS) {  \
    using type = decltype(t);                                                  \
    if constexpr (member_##NAME<type>) {                                       \
      return std::forward<type>(t).NAME(CGUI_EXTRA_ARGS);                      \
    } else {                                                                   \
      static_assert(free_##NAME<type>);                                        \
      return NAME(std::forward<type>(t)                                        \
                      CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS);                  \
    }                                                                          \
  }                                                                            \
  }                                                                            \
  constexpr auto NAME(auto &&t) -> decltype(ns_lookup::do_##NAME(              \
                                    std::forward<decltype(t)>(t)))             \
    requires(                                                                  \
        requires() { ns_lookup::do_##NAME(std::forward<decltype(t)>(t)); })    \
  {                                                                            \
    return ns_lookup::do_##NAME(std::forward<decltype(t)>(t));                 \
  }

#define CGUI_PIX_FUNC_IMPL_GETSET(NAME, CONCEPT)                               \
  CGUI_PIX_FUNC_IMPL(NAME, CONCEPT)                                            \
  namespace ns_lookup {                                                        \
                                                                               \
  template <typename T, typename TVal>                                         \
  concept member_set_##NAME = requires(                                        \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { (*tc).NAME(*v); };       \
  template <typename T, typename TVal>                                         \
  concept member_mut_##NAME = requires(bp::as_forward<T> tc) {                 \
    { (*tc).NAME() } -> std::assignable_from<TVal>;                            \
  };                                                                           \
  template <typename T, typename TVal>                                         \
  concept free_set_##NAME = requires(                                          \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { NAME(*tc, *v); };        \
  template <typename T, typename TVal>                                         \
  concept free_mut_##NAME = requires(bp::as_forward<T> tc) {                   \
    { NAME(*tc) } -> std::assignable_from<TVal>;                               \
  };                                                                           \
  template <typename T, typename TV>                                           \
  concept has_set_##NAME =                                                     \
      member_set_##NAME<T, TV> || member_mut_##NAME<T, TV> ||                  \
      free_set_##NAME<T, TV> || free_mut_##NAME<T, TV>;                        \
                                                                               \
  template <typename TV, has_set_##NAME<TV> T>                                 \
  constexpr auto do_set_##NAME(T &&vorg, TV &&valorg) {                        \
    auto v = bp::as_forward<T>(std::forward<T>(vorg));                         \
    auto val = bp::as_forward<TV>(std::forward<TV>(valorg));                   \
    if constexpr (member_set_##NAME<T, TV>) {                                  \
      return (*v).NAME(*val);                                                  \
    } else if constexpr (free_set_##NAME<T, TV>) {                             \
      return NAME(*v, *val);                                                   \
    } else if constexpr (member_mut_##NAME<T, TV>) {                           \
      return (*v).NAME() = *val;                                               \
    } else {                                                                   \
      static_assert(free_mut_##NAME<T, TV>);                                   \
      return NAME(*v) = *val;                                                  \
    }                                                                          \
  }                                                                            \
  }                                                                            \
  constexpr auto NAME(auto &&t, auto &&v)                                      \
    requires(requires() {                                                      \
      ns_lookup::do_set_##NAME(std::forward<decltype(t)>(t),                   \
                               std::forward<decltype(v)>(v));                  \
    })                                                                         \
  {                                                                            \
    return ns_lookup::do_set_##NAME(std::forward<decltype(t)>(t),              \
                                    std::forward<decltype(v)>(v));             \
  }

CGUI_PIX_FUNC_IMPL_GETSET(x_of, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(y_of, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(tl_x, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(tl_y, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(br_x, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(br_y, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(width, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(height, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(red, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(green, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(blue, pixel_coord_value_cv_t)
CGUI_PIX_FUNC_IMPL_GETSET(alpha, pixel_coord_value_cv_t)

} // namespace extend

template <typename T>
concept pixel_coord = requires(T &&t) {
  { extend::x_of(t) } -> pixel_coord_value_cv_t;
  { extend::y_of(t) } -> pixel_coord_value_cv_t;
};

template <typename T, typename TVal>
concept pixel_coord_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      extend::x_of(*t, *v);
      extend::y_of(*t, *v);
    };
template <typename T, typename TVal>
concept pixel_coord_ref = requires(bp::as_forward<T> t) {
  { extend::x_of(*t) } -> std::assignable_from<TVal>;
  { extend::y_of(*t) } -> std::assignable_from<TVal>;
};
template <typename T, typename TVal>
concept pixel_coord_mut = pixel_coord_ref<T, TVal> || pixel_coord_set<T, TVal>;

template <typename T>
concept colour = requires(T &&t) {
  { extend::red(t) } -> pixel_coord_value_cv_t;
  { extend::blue(t) } -> pixel_coord_value_cv_t;
  { extend::green(t) } -> pixel_coord_value_cv_t;
  { extend::alpha(t) } -> pixel_coord_value_cv_t;
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

template <typename T>
struct basic_colour_t {
  T red, green, blue, alpha;
};

using default_colour_t = basic_colour_t<std::uint_least8_t>;

template <typename T>
constexpr T &red(basic_colour_t<T> &c) noexcept { return c.red; }
template <typename T>
constexpr T red(basic_colour_t<T> const &c) noexcept {
  return c.red;
}
template <typename T>
constexpr T &blue(basic_colour_t<T> &c) noexcept {
  return c.red;
}
template <typename T>
constexpr T blue(basic_colour_t<T> const &c) noexcept {
  return c.blue;
}
template <typename T>
constexpr T &green(basic_colour_t<T> &c) noexcept {
  return c.green;
}
template <typename T>
constexpr T green(basic_colour_t<T> const &c) noexcept {
  return c.green;
}
template <typename T>
constexpr T &alpha(basic_colour_t<T> &c) noexcept {
  return c.alpha;
}
template <typename T>
constexpr T alpha(basic_colour_t<T> const &c) noexcept {
  return c.alpha;
}

namespace extend {
CGUI_PIX_FUNC_IMPL(top_left, pixel_coord)
CGUI_PIX_FUNC_IMPL(bottom_right, pixel_coord)
} // namespace extend

template <typename T, typename TFrom>
concept is_mutable_by =
    std::is_lvalue_reference_v<T> && std::is_assignable_v<T, TFrom>;

template <typename T, typename TFrom>
concept mutable_pixel_coord_value =
    is_mutable_by<T, TFrom> && pixel_coord_value_cv_t<T>;

template <typename T>
concept bounding_box_xxyy = requires(T const &t) {
  { extend::tl_x(t) } -> pixel_coord_value_cv_t;
  { extend::tl_y(t) } -> pixel_coord_value_cv_t;
  { extend::br_x(t) } -> pixel_coord_value_cv_t;
  { extend::br_y(t) } -> pixel_coord_value_cv_t;
};
template <typename T, typename TFrom>
concept bounding_box_xxyy_mut = requires(bp::as_forward<T> t) {
  { extend::tl_x(*t) } -> mutable_pixel_coord_value<TFrom>;
  { extend::tl_y(*t) } -> mutable_pixel_coord_value<TFrom>;
  { extend::br_x(*t) } -> mutable_pixel_coord_value<TFrom>;
  { extend::br_y(*t) } -> mutable_pixel_coord_value<TFrom>;
};
template <typename T, typename TVal>
concept bounding_box_xxyy_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      extend::tl_x(*t, *v);
      extend::tl_y(*t, *v);
      extend::br_x(*t, *v);
      extend::br_y(*t, *v);
    };

template <typename T>
concept bounding_box_xwyh = requires(T const &t) {
  { extend::tl_x(t) } -> pixel_coord_value_cv_t;
  { extend::width(t) } -> pixel_coord_value_cv_t;
  { extend::tl_y(t) } -> pixel_coord_value_cv_t;
  { extend::height(t) } -> pixel_coord_value_cv_t;
};
template <typename T, typename TFrom>
concept bounding_box_xwyh_mut = requires(bp::as_forward<T> t) {
  { extend::tl_x(*t) } -> mutable_pixel_coord_value<TFrom>;
  { extend::width(*t) } -> mutable_pixel_coord_value<TFrom>;
  { extend::tl_y(*t) } -> mutable_pixel_coord_value<TFrom>;
  { extend::height(*t) } -> mutable_pixel_coord_value<TFrom>;
};
template <typename T, typename TVal>
concept bounding_box_xwyh_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      extend::tl_x(*t, *v);
      extend::width(*t, *v);
      extend::br_x(*t, *v);
      extend::height(*t, *v);
    };

template <typename T>
concept bounding_box_coord = requires(T const &t) {
  { extend::top_left(t) } -> pixel_coord;
  { extend::bottom_right(t) } -> pixel_coord;
};
template <typename T, typename TFrom>
concept bounding_box_coord_mut = requires(bp::as_forward<T> t) {
  { extend::top_left(*t) } -> pixel_coord_mut<TFrom>;
  { extend::bottom_right(*t) } -> pixel_coord_mut<TFrom>;
};
template <typename T, typename TVal>
concept bounding_box_coord_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      extend::top_left(*t, *v);
      extend::bottom_right(*t, *v);
    };

template <typename T>
concept bounding_box =
    bounding_box_coord<T> || bounding_box_xwyh<T> || bounding_box_xxyy<T>;
template <typename T, typename TFrom>
concept mutable_bounding_box =
    bounding_box_coord_mut<T, TFrom> || bounding_box_xwyh_mut<T, TFrom> ||
    bounding_box_xxyy_mut<T, TFrom> || bounding_box_coord_set<T, TFrom> ||
    bounding_box_xwyh_set<T, TFrom> || bounding_box_xxyy_set<T, TFrom>;

struct default_rect {
  default_pixel_coord tl;
  default_pixel_coord br;
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
concept renderer = true;
template <typename T>
concept renderer_member_context = requires(T &&t, int x) {
  { t.context(x, x, x, x) } -> renderer;
};
template <typename T>
concept renderer_free_context = requires(T &&t, int x) {
  { context(t, x, x, x, x) } -> renderer;
};
template <typename T>
concept renderer_context =
    renderer_free_context<T> || renderer_member_context<T>;

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

template <typename>
concept canvas = true;

namespace extend {
#undef CGUI_EXTRA_PARAMS
#undef CGUI_EXTRA_ARGS
#undef CGUI_EXTRA_ARGS_COMMA
#define CGUI_EXTRA_PARAMS , default_rect const &r
#define CGUI_EXTRA_ARGS r
#define CGUI_EXTRA_ARGS_COMMA ,

CGUI_PIX_FUNC_IMPL(context, renderer)

#undef CGUI_EXTRA_PARAMS
#undef CGUI_EXTRA_ARGS
#undef CGUI_EXTRA_ARGS_COMMA
#define CGUI_EXTRA_PARAMS
#define CGUI_EXTRA_ARGS
#define CGUI_EXTRA_ARGS_COMMA

} // namespace extend

struct xy2wh_t {
  static constexpr auto on_fetch(auto const &x1, auto const &x2) {
    return x2 - x1;
  }
  static constexpr auto on_assign(auto const &x1, auto const &w) {
    return x1 + w;
  }
};
struct wh2xy_t {
  static constexpr auto on_fetch(auto const &x1, auto const &w) {
    return x1 + w;
  }
  static constexpr auto on_assign(auto const &x1, auto const &x2) {
    return x2 - x1;
  }
};

inline constexpr xy2wh_t xy2wh;
inline constexpr wh2xy_t wh2xy;

template <typename T>
concept xxyy_xwyh_conv_policy =
    std::is_same_v<T, xy2wh_t> || std::is_same_v<T, wh2xy_t>;

template <typename TXY, typename TWH, xxyy_xwyh_conv_policy TPol>
class xxyy_xwyh_conv {
  TXY const *xy_{};
  TWH *wh_{};
  static_assert(!std::is_reference_v<TXY>);
  static_assert(!std::is_reference_v<TWH>);

public:
  using value_type = std::common_type_t<TXY, TWH>;
  constexpr xxyy_xwyh_conv(TXY const &xy, TWH &wh, TPol = {})
      : xy_(&xy), wh_(&wh) {}

  constexpr xxyy_xwyh_conv &operator=(value_type const &v) {
    *wh_ = TPol::on_assign(*xy_, v);
    return *this;
  }

  constexpr explicit(false) operator value_type() const {
    return TPol::on_fetch(*xy_, *wh_);
  }
  constexpr value_type value() const { return TPol::on_fetch(*xy_, *wh_); }
};

namespace call {
inline constexpr auto x_of = [](auto &&t, auto &&...vals) {
  return extend::x_of(std::forward<decltype(t)>(t),
                      std::forward<decltype(vals)>(vals)...);
};
inline constexpr auto y_of = [](auto &&t, auto &&...vals) {
  return extend::y_of(std::forward<decltype(t)>(t),
                      std::forward<decltype(vals)>(vals)...);
};
#define CGUI_CALL_IMPL(NAME, BASE_CONCEPT, MUT_BASE_CONCEPT)                   \
  constexpr decltype(auto) _fallback(auto const &b) const;                     \
  constexpr decltype(auto) _fallback_mut(auto &&b, auto &&v) const;            \
  constexpr decltype(auto) operator()(BASE_CONCEPT auto &&b) const {      \
    using fwd_t = decltype(b);                                                                           \
    auto bf = bp::as_forward<fwd_t>(std::forward<fwd_t>(b));                                                                           \
    if constexpr (requires() { extend::NAME(b); }) {                           \
      return extend::NAME(*bf);                                                  \
    } else {                                                                   \
      return _fallback(bf);                                                     \
    }                                                                          \
  }                                                                            \
  template <typename TVal, MUT_BASE_CONCEPT<TVal> T>                           \
  constexpr decltype(auto) operator()(T &&b, TVal &&v) const {                 \
    auto bf = bp::as_forward<T>(std::forward<T>(b));                           \
    auto vf = bp::as_forward<TVal>(std::forward<TVal>(v));                     \
    if constexpr (requires() { extend::NAME(*bf, *vf); }) {                    \
      return extend::NAME(*bf, *vf);                                           \
    } else if constexpr (requires() {                                          \
                           { extend::NAME(*bf) } -> is_mutable_by<TVal>;       \
                         }) {                                                  \
      return extend::NAME(*bf) = *vf;                                          \
    } else {                                                                   \
      return _fallback_mut(bf, vf);                                            \
    }                                                                          \
  }

struct tl_x_t {
  CGUI_CALL_IMPL(tl_x, bounding_box, mutable_bounding_box)
};
struct tl_y_t {
  CGUI_CALL_IMPL(tl_y, bounding_box, mutable_bounding_box)
};
struct br_x_t {
  CGUI_CALL_IMPL(br_x, bounding_box, mutable_bounding_box)
};
struct br_y_t {
  CGUI_CALL_IMPL(br_y, bounding_box, mutable_bounding_box)
};
struct width_t {
  CGUI_CALL_IMPL(width, bounding_box, mutable_bounding_box)
};
struct height_t {
  CGUI_CALL_IMPL(height, bounding_box, mutable_bounding_box)
};
struct top_left_t {
  CGUI_CALL_IMPL(top_left, bounding_box, mutable_bounding_box)
};
struct bottom_right_t {
  CGUI_CALL_IMPL(bottom_right, bounding_box, mutable_bounding_box)
};

inline constexpr tl_x_t tl_x;
inline constexpr tl_y_t tl_y;
inline constexpr br_x_t br_x;
inline constexpr br_y_t br_y;
inline constexpr width_t width;
inline constexpr height_t height;
inline constexpr top_left_t top_left;
inline constexpr bottom_right_t bottom_right;

constexpr decltype(auto) tl_x_t::_fallback(auto const &b) const {
  return x_of(extend::top_left(*b));
}
constexpr decltype(auto) tl_x_t::_fallback_mut(auto &&b, auto &&v) const {
  return x_of(extend::top_left(*b), *v);
}
constexpr decltype(auto) tl_y_t::_fallback(auto const &b) const {
  return y_of(extend::top_left(*b));
}
constexpr decltype(auto) tl_y_t::_fallback_mut(auto &&b, auto &&v) const {
  return y_of(extend::top_left(*b), *v);
}
constexpr decltype(auto) br_x_t::_fallback(auto const &b) const {
  if constexpr (requires() { extend::bottom_right(*b); }) {
    return x_of(extend::bottom_right(*b));
  } else {
    return tl_x(*b) + extend::width(*b);
  }
}
constexpr decltype(auto) br_x_t::_fallback_mut(auto &&b, auto &&v) const {
  if constexpr (requires() {
                  { extend::bottom_right(*b) } -> pixel_coord_mut<decltype(*v)>;
                }) {
    return x_of(extend::bottom_right(*b), *v);
  } else {
    return (xxyy_xwyh_conv(tl_x(*b), extend::width(*b), wh2xy) = *v).value();
  }
}
constexpr decltype(auto) br_y_t::_fallback(auto const &b) const {
  if constexpr (requires() { extend::bottom_right(*b); }) {
    return y_of(extend::bottom_right(*b));
  } else {
    return tl_y(*b) + extend::height(*b);
  }
}
constexpr decltype(auto) br_y_t::_fallback_mut(auto &&b, auto &&v) const {
  if constexpr (requires() {
                  { extend::bottom_right(*b) } -> pixel_coord_mut<decltype(*v)>;
                }) {
    return y_of(extend::bottom_right(*b), *v);
  } else {
    return (xxyy_xwyh_conv(tl_y(*b), extend::height(*b), wh2xy) = *v).value();
  }
}
constexpr decltype(auto) width_t::_fallback(auto const &b) const {
  return br_x(*b) - tl_x(*b);
}
constexpr decltype(auto) width_t::_fallback_mut(auto &&b, auto &&v) const {
  if constexpr (requires() {
                  extend::bottom_right(*b);
                  x_of(extend::top_left(*b));
                  x_of(extend::bottom_right(*b),
                       x_of(extend::top_left(*b)) + *v);
                }) {
    return x_of(extend::bottom_right(*b), x_of(extend::top_left(*b)) + *v);
  } else {
    return extend::br_x(*b, extend::tl_x(*b) + *v);
  }
}
constexpr decltype(auto) height_t::_fallback(auto const &b) const {
  return br_y(*b) - tl_y(*b);
}
constexpr decltype(auto) height_t::_fallback_mut(auto &&b, auto &&v) const {
  if constexpr (requires() {
                  extend::bottom_right(*b);
                  y_of(extend::top_left(*b));
                  y_of(extend::bottom_right(*b),
                       y_of(extend::top_left(*b)) + *v);
                }) {
    return y_of(extend::bottom_right(*b), y_of(extend::top_left(*b)) + *v);
  } else {
    return extend::br_y(*b, extend::tl_y(*b) + *v);
  }
}

template <typename T, typename TX, typename TY>
class tlbr_wh_conv {
  T val_;
public:
  constexpr tlbr_wh_conv(T&& v, TX, TY) : val_(std::forward<T>(v)) {}

  constexpr tlbr_wh_conv(tlbr_wh_conv&&) = delete;
  constexpr tlbr_wh_conv& operator=(tlbr_wh_conv&&) = delete;

  constexpr T&& ref() && {
    return std::forward<T>(val_);
  }
  constexpr tlbr_wh_conv force_copy() && {
    return {std::forward<T>(val_), TX{}, TY{}};
  }

  constexpr decltype(auto) x_of() const {
    return TX{}(val_);
  }
  constexpr decltype(auto) x_of(auto&& v) {
    return TX{}(val_, std::forward<decltype(v)>(v));
  }
  constexpr decltype(auto) y_of() const {
    return TY{}(val_);
  }
  constexpr decltype(auto) y_of(auto&& v) {
    return TY{}(val_, std::forward<decltype(v)>(v));
  }
};

template <typename T, typename TX, typename TY>
tlbr_wh_conv(T&&, TX, TY) -> tlbr_wh_conv<T, TX, TY>;

constexpr decltype(auto) top_left_t::_fallback(auto const &b) const {
  return tlbr_wh_conv(*b, tl_x, tl_y);
}
constexpr decltype(auto) top_left_t::_fallback_mut(auto &&b, auto&& v) const {
  auto val = _fallback(b);
  x_of(val, x_of(*v));
  y_of(val, y_of(*v));
  return val;
}
constexpr decltype(auto) bottom_right_t::_fallback(auto const &b) const {
  return tlbr_wh_conv(*b, br_x, br_y);
}
constexpr decltype(auto) bottom_right_t::_fallback_mut(auto &&b, auto&& v) const {
  auto val = _fallback(b);
  x_of(val, x_of(*v));
  y_of(val, y_of(*v));
  return val;
}

}; // namespace call

template <typename T> class not_null {
public:
  using pointer = T *;

private:
  pointer ptr_;

public:
  constexpr explicit(false) not_null(pointer p) noexcept : ptr_(p) {}
  not_null(std::nullptr_t) = delete;
  constexpr pointer ptr() const noexcept { return ptr_; }
  constexpr explicit(false) operator pointer() const noexcept { return ptr(); }
  constexpr T &operator*() const noexcept { return *ptr_; }
  constexpr pointer operator->() const noexcept { return ptr(); }
};

template <typename T> not_null(T *) -> not_null<T>;

static_assert(std::convertible_to<not_null<int>, int *>);
static_assert(std::convertible_to<int *, not_null<int>>);
static_assert(!std::convertible_to<std::nullptr_t, not_null<int>>);

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

} // namespace cgui

#undef CGUI_PIX_FUNC_IMPL
#undef CGUI_EXTRA_PARAMS
#undef CGUI_EXTRA_ARGS
#undef CGUI_EXTRA_ARGS_COMMA
#undef CGUI_CALL_IMPL

#endif // COMPONENT_GUI_CGUI_TYPES_HPP

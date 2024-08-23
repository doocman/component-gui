
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
#include <cgui/std-backport/type_traits.hpp>
#include <cgui/std-backport/utility.hpp>

#include <cgui/warnings.hpp>

namespace cgui {

template <typename T> class not_null {
public:
  // using pointer = T *;

private:
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

static_assert(std::convertible_to<not_null<int *>, int *>);
static_assert(std::convertible_to<int *, not_null<int *>>);
static_assert(!std::convertible_to<std::nullptr_t, not_null<int *>>);

template <typename> struct pixel_type;
template <typename> struct extend_api;
template <typename T> using extend_api_t = extend_api<std::remove_cvref_t<T>>;

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
  concept member_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                 \
    { tc.NAME(CGUI_EXTRA_ARGS) } -> CONCEPT;                                   \
  };                                                                           \
  template <typename T>                                                        \
  concept free_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                   \
    { NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS) } -> CONCEPT;             \
  };                                                                           \
  template <typename T>                                                        \
  concept static_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                 \
    {                                                                          \
      std::remove_cvref_t<T>::NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS)   \
    } -> CONCEPT;                                                              \
  };                                                                           \
  template <typename TExt, typename T>                                         \
  concept extend_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                 \
    { TExt::NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS) } -> CONCEPT;       \
  };                                                                           \
  template <typename T>                                                        \
  concept has_##NAME = member_##NAME<T> || static_##NAME<T> ||                 \
                       free_##NAME<T> || extend_##NAME<extend_api_t<T>, T>;    \
  constexpr decltype(auto) do_##NAME(has_##NAME auto &&t CGUI_EXTRA_PARAMS) {  \
    using type = decltype(t);                                                  \
    if constexpr (member_##NAME<type>) {                                       \
      return std::forward<type>(t).NAME(CGUI_EXTRA_ARGS);                      \
    } else if constexpr (static_##NAME<type>) {                                \
      return std::remove_cvref_t<type>::NAME(                                  \
          std::forward<type>(t) CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS);        \
    } else if constexpr (extend_##NAME<extend_api_t<type>, type>) {            \
      return extend_api_t<type>::NAME(                                         \
          std::forward<type>(t) CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS);        \
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
  concept static_set_##NAME =                                                  \
      requires(bp::as_forward<T> tc, bp::as_forward<TVal> v) {                 \
        std::remove_cvref_t<T>::NAME(*tc, *v);                                 \
      };                                                                       \
  template <typename T, typename TVal>                                         \
  concept free_set_##NAME = requires(                                          \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { NAME(*tc, *v); };        \
  template <typename TExt, typename T, typename TVal>                          \
  concept extend_set_##NAME = requires(                                        \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { TExt::NAME(*tc, *v); };  \
  template <typename T, typename TVal>                                         \
  concept mut_get_##NAME = has_##NAME<T> && requires(bp::as_forward<T> t) {    \
    { ns_lookup::do_##NAME(*t) } -> std::assignable_from<TVal>;                \
  };                                                                           \
  template <typename T, typename TV>                                           \
  concept has_set_##NAME =                                                     \
      member_set_##NAME<T, TV> || static_set_##NAME<T, TV> ||                  \
      free_set_##NAME<T, TV> || extend_set_##NAME<extend_api_t<T>, T, TV> ||   \
      mut_get_##NAME<T, TV>;                                                   \
                                                                               \
  template <typename TV, has_set_##NAME<TV> T>                                 \
  constexpr auto do_set_##NAME(T &&vorg, TV &&valorg) {                        \
    auto v = bp::as_forward<T>(std::forward<T>(vorg));                         \
    auto val = bp::as_forward<TV>(std::forward<TV>(valorg));                   \
    if constexpr (member_set_##NAME<T, TV>) {                                  \
      return (*v).NAME(*val);                                                  \
    } else if constexpr (static_set_##NAME<T, TV>) {                           \
      return std::remove_cvref_t<T>::NAME(*v, *val);                           \
    } else if constexpr (extend_set_##NAME<extend_api_t<T>, T, TV>) {          \
      return extend_api_t<T>::NAME(*v, *val);                                  \
    } else if constexpr (free_set_##NAME<T, TV>) {                             \
      return NAME(*v, *val);                                                   \
    } else {                                                                   \
      static_assert(mut_get_##NAME<T, TV>);                                    \
      return do_##NAME(*v) = *val;                                             \
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

template <typename T> struct basic_colour_t {
  T red, green, blue, alpha;
};

using default_colour_t = basic_colour_t<std::uint_least8_t>;

template <typename T> constexpr T &red(basic_colour_t<T> &c) noexcept {
  return c.red;
}
template <typename T> constexpr T red(basic_colour_t<T> const &c) noexcept {
  return c.red;
}
template <typename T> constexpr T &blue(basic_colour_t<T> &c) noexcept {
  return c.red;
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

template <typename> struct extend_t {};

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
      extend::tl_y(*t, *v);
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

template <typename>
concept text2render = true;
template <typename T>
concept pixel_drawer =
    std::invocable<T &&, default_pixel_coord, default_colour_t>;
struct dummy_pixel_drawer {
  constexpr void operator()(pixel_coord auto &&, colour auto &&) {}
};
static_assert(pixel_drawer<dummy_pixel_drawer>);

template <typename>
concept pixel_draw_callback = true;

struct dummy_pixel_draw_callback {};
static_assert(pixel_draw_callback<dummy_pixel_draw_callback>);

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

class keep_current_t {

public:
  constexpr decltype(auto) operator()(auto &&op, auto &&obj) const {
    return std::forward<decltype(op)>(op)(std::forward<decltype(obj)>(obj));
  }
};

inline constexpr keep_current_t keep_current;

namespace call {
template <typename> constexpr bool is_placeholder_impl = false;
template <> constexpr bool is_placeholder_impl<keep_current_t> = true;
template <typename T>
constexpr bool is_placeholder_v = is_placeholder_impl<std::remove_cvref_t<T>>;

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
  constexpr decltype(auto) operator()(BASE_CONCEPT auto &&b) const {           \
    using fwd_t = decltype(b);                                                 \
    auto bf = bp::as_forward<fwd_t>(std::forward<fwd_t>(b));                   \
    if constexpr (requires() { extend::NAME(b); }) {                           \
      return extend::NAME(*bf);                                                \
    } else {                                                                   \
      return _fallback(bf);                                                    \
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
    return extend::width(*b, *v - tl_x(*b));
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
    return extend::height(*b, *v - tl_y(*b));
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

template <typename T, typename TX, typename TY> class tlbr_wh_conv {
  T val_;

public:
  constexpr tlbr_wh_conv(T &&v, TX, TY) : val_(std::forward<T>(v)) {}

  constexpr tlbr_wh_conv(tlbr_wh_conv &&) = delete;
  constexpr tlbr_wh_conv &operator=(tlbr_wh_conv &&) = delete;

  constexpr T &&ref() && { return std::forward<T>(val_); }
  constexpr tlbr_wh_conv force_copy() && {
    return {std::forward<T>(val_), TX{}, TY{}};
  }

  constexpr decltype(auto) x_of() const { return TX{}(val_); }
  constexpr decltype(auto) x_of(auto &&v) {
    return TX{}(val_, std::forward<decltype(v)>(v));
  }
  constexpr decltype(auto) y_of() const { return TY{}(val_); }
  constexpr decltype(auto) y_of(auto &&v) {
    return TY{}(val_, std::forward<decltype(v)>(v));
  }
};

template <typename T, typename TX, typename TY>
tlbr_wh_conv(T &&, TX, TY) -> tlbr_wh_conv<T, TX, TY>;

constexpr decltype(auto) top_left_t::_fallback(auto const &b) const {
  return tlbr_wh_conv(*b, tl_x, tl_y);
}
constexpr decltype(auto) top_left_t::_fallback_mut(auto &&b, auto &&v) const {
  auto val = _fallback(b);
  x_of(val, x_of(*v));
  y_of(val, y_of(*v));
  return val;
}
constexpr decltype(auto) bottom_right_t::_fallback(auto const &b) const {
  return tlbr_wh_conv(*b, br_x, br_y);
}
constexpr decltype(auto) bottom_right_t::_fallback_mut(auto &&b,
                                                       auto &&v) const {
  auto val = _fallback(b);
  x_of(val, x_of(*v));
  y_of(val, y_of(*v));
  return val;
}

namespace impl {
[[maybe_unused]] inline void render_direct() {}
[[maybe_unused]] inline void draw_pixels() {}
[[maybe_unused]] inline void set_displayed() {}
[[maybe_unused]] inline void set_text() {}
[[maybe_unused]] inline void render_text() {}

template <typename T, typename TR, typename... Ts>
concept member_render_direct =
    requires(bp::as_forward<T> t, TR &r, Ts &&...args) {
      (*t).render_direct(r, std::forward<Ts>(args)...);
    };
template <typename T, typename TR, typename... Ts>
concept static_render_direct =
    requires(bp::as_forward<T> t, TR &r, Ts &&...args) {
      std::remove_cvref_t<T>::render_direct(*t, r, std::forward<Ts>(args)...);
    };
template <typename T, typename TR, typename... Ts>
concept free_render_direct =
    requires(bp::as_forward<T> t, TR &r, Ts &&...args) {
      render_direct(*t, r, std::forward<Ts>(args)...);
    };
template <typename T, typename TR, typename... Ts>
concept has_render_direct =
    renderer<TR> &&
    (member_render_direct<T, TR, Ts...> || free_render_direct<T, TR, Ts...> ||
     static_render_direct<T, TR, Ts...>);

inline constexpr auto do_render_direct =
    []<renderer TR, typename... Ts, has_render_direct<TR, Ts...> T>(
        T &&torg, TR &&rorg, Ts &&...args) {
      auto t = bp::as_forward(std::forward<T>(torg));
      auto r = bp::as_forward(std::forward<TR>(rorg));
      if constexpr (member_render_direct<T, TR, Ts...>) {
        return (*t).render_direct(*r, std::forward<Ts>(args)...);
      } else if constexpr (static_render_direct<T, TR, Ts...>) {
        return std::remove_cvref_t<T>::render_direct(*t, *r,
                                                     std::forward<Ts>(args)...);
      } else {
        static_assert(free_render_direct<T, TR, Ts...>);
        return render_direct(*t, *r, std::forward<Ts>(args)...);
      }
    };

template <typename T, typename TRect, typename TCP>
concept member_draw_pixels = requires(T &&t, TRect const &r, TCP &&cb) {
  std::forward<T>(t).draw_pixels(r, std::forward<TCP>(cb));
};
template <typename T, typename TRect, typename TCP>
concept static_draw_pixels = requires(T &&t, TRect const &r, TCP &&cb) {
  std::remove_cvref_t<T>::draw_pixels(std::forward<T>(t), r,
                                      std::forward<TCP>(cb));
};
template <typename T, typename TRect, typename TCP>
concept free_draw_pixels = requires(T &&t, TRect const &r, TCP &&cb) {
  draw_pixels(std::forward<T>(t), r, std::forward<TCP>(cb));
};
template <typename T, typename TRect, typename TCP>
concept has_draw_pixels =
    bounding_box<TRect> && pixel_draw_callback<TCP> &&
    (member_draw_pixels<T, TRect, TCP> || static_draw_pixels<T, TRect, TCP> ||
     free_draw_pixels<T, TRect, TCP>);

inline constexpr auto do_draw_pixels =
    []<pixel_draw_callback TCB, bounding_box TRect,
       has_draw_pixels<TRect, TCB> T>(T &&torg, TRect const &rorg,
                                      TCB &&cborg) {
      auto t = bp::as_forward(std::forward<T>(torg));
      auto r = bp::as_forward(rorg);
      auto cb = bp::as_forward(std::forward<TCB>(cborg));
      if constexpr (member_draw_pixels<T, TRect, TCB>) {
        return (*t).draw_pixels(*r, *cb);
      } else if constexpr (static_draw_pixels<T, TRect, TCB>) {
        return std::remove_cvref_t<T>::draw_pixels(*t, *r, *cb);
      } else {
        static_assert(free_draw_pixels<T, TRect, TCB>);
        return draw_pixels(*t, *r, *cb);
      }
    };

struct do_center {
  constexpr auto operator()(bounding_box auto const &b) const {
    auto tl = call::top_left(b);
    auto br = call::bottom_right(b);
    return default_pixel_coord{(x_of(br) - x_of(tl)) / 2,
                               (y_of(br) - y_of(tl)) / 2};
  };
};

template <typename T, typename... Ts>
concept member_set_displayed =
    requires(bp::as_forward<T> t, bp::as_forward<Ts>... vals) {
      (*t).set_displayed(*vals...);
    };
template <typename T, typename... Ts>
concept static_set_displayed =
    requires(bp::as_forward<T> t, bp::as_forward<Ts>... vals) {
      std::remove_cvref_t<T>::set_displayed(*t, *vals...);
    };
template <typename T, typename... Ts>
concept free_set_displayed =
    requires(bp::as_forward<T> t, bp::as_forward<Ts>... vals) {
      set_displayed(*t, *vals...);
    };
template <typename T, typename... Ts>
concept has_set_displayed =
    member_set_displayed<T, Ts...> || static_set_displayed<T, Ts...> ||
    free_set_displayed<T, Ts...>;

struct do_set_displayed {
  template <typename... Ts, has_set_displayed<Ts...> T>
  constexpr decltype(auto) operator()(T &&torg, Ts &&...vals) const {
    auto t = bp::as_forward(std::forward<T>(torg));
    if constexpr (member_set_displayed<T, Ts...>) {
      return (*t).set_displayed(std::forward<Ts>(vals)...);
    } else if constexpr (static_set_displayed<T, Ts...>) {
      return std::remove_cvref_t<T>::set_displayed(*t,
                                                   std::forward<Ts>(vals)...);
    } else {
      static_assert(free_set_displayed<T, Ts...>);
      return set_displayed(*t, std::forward<Ts>(vals)...);
    }
  }
};

template <typename T, typename TRend>
concept member_render_text =
    requires(bp::as_forward<T> t, bp::as_forward<TRend> rend, int w, int h) {
      (*t).render_text(*rend, w, h);
    };
template <typename T, typename TRend>
concept static_render_text =
    requires(bp::as_forward<T> t, bp::as_forward<TRend> rend, int w, int h) {
      std::remove_cvref_t<T>::render_text((*t), *rend, w, h);
    };
template <typename T, typename TRend>
concept free_render_text =
    requires(bp::as_forward<T> t, bp::as_forward<TRend> rend, int w, int h) {
      render_text((*t), *rend, w, h);
    };
template <typename T, typename TRend>
concept has_render_text = renderer<TRend> && (member_render_text<T, TRend> ||
                                              static_render_text<T, TRend> ||
                                              free_render_text<T, TRend>);

struct do_render_text {
  template <renderer TRend, has_render_text<TRend> T>
  constexpr auto operator()(T &&torg, TRend &&rorg, int w, int h) const {
    auto t = bp::as_forward<T>(std::forward<T>(torg));
    auto r = bp::as_forward(std::forward<TRend>(rorg));
    if constexpr (member_render_text<T, TRend>) {
      return (*t).render_text(*r, w, h);
    } else if constexpr (static_render_text<T, TRend>) {
      return std::remove_cvref_t<T>::render_text(*t, *r, w, h);
    } else {
      return render_text(*t, *r, w, h);
    }
  }
};

template <typename T, typename TStr>
concept member_set_text = requires(
    bp::as_forward<T> t, bp::as_forward<TStr> s) { (*t).set_text(*s); };
template <typename T, typename TStr>
concept static_set_text =
    requires(bp::as_forward<T> t, bp::as_forward<TStr> s) {
      std::remove_cvref_t<T>::set_text(*t, *s);
    };
template <typename T, typename TStr>
concept free_set_text =
    requires(bp::as_forward<T> t, bp::as_forward<TStr> s) { set_text(*t, *s); };
template <typename T, typename TStr>
concept has_set_text = member_set_text<T, TStr> || static_set_text<T, TStr> ||
                       free_set_text<T, TStr>;

struct do_set_text {
  template <typename TStr, has_set_text<TStr> T>
  constexpr auto operator()(T &&torg, TStr &&sorg) const {
    auto t = bp::as_forward(std::forward<T>(torg));
    auto s = bp::as_forward(std::forward<TStr>(sorg));
    if constexpr (member_set_text<T, TStr>) {
      return (*t).set_text(*s);
    } else if constexpr (static_set_text<T, TStr>) {
      return std::remove_cvref_t<T>::set_text(*t, *s);
    } else {
      return set_text(*t, *s);
    }
  }
};

template <typename T, typename TC>
concept has_from_xyxy = requires(bp::as_forward<TC> v) {
  { T::from_xyxy(*v, *v, *v, *v) } -> bounding_box;
};
template <typename T, typename TC>
concept has_from_xywh = requires(bp::as_forward<TC> v) {
  { T::from_xywh(*v, *v, *v, *v) } -> bounding_box;
};
template <typename T, typename TC>
concept has_from_tlbr_ils = requires(bp::as_forward<TC> v) {
  { T::from_tlbr({*v, *v}, {*v, *v}) } -> bounding_box;
};
template <typename T, typename TC>
concept has_from_tlbr = requires(bp::as_forward<TC> v) {
  { T::from_tlbr(*v, *v) } -> bounding_box;
};
template <typename T, typename TC>
concept has_bbox_init =
    has_from_xyxy<T, TC> || has_from_xywh<T, TC> ||
    has_from_tlbr<T, basic_default_pixel_coord<std::remove_cvref_t<TC>>> ||
    has_from_tlbr_ils<T, TC>;

struct do_from_xyxy {
  template <typename TXY, has_bbox_init<TXY> T>
  constexpr bounding_box auto operator()(T const &, TXY xl, TXY yt, TXY xr,
                                         TXY yb) const {
    if constexpr (has_from_xyxy<T, TXY>) {
      return T::from_xyxy(std::move(xl), std::move(yt), std::move(xr),
                          std::move(yb));
    } else if constexpr (has_from_xywh<T, TXY>) {
      return T::from_xywh(std::move(xl), std::move(yt), xr - xl, yb - yt);
    } else if constexpr (has_from_tlbr_ils<T, TXY>) {
      return T::from_tlbr({std::move(xl), std::move(yt)},
                          {std::move(xr), std::move(yb)});
    } else {
      return T::from_tlbr(
          basic_default_pixel_coord<TXY>(std::move(xl), std::move(yt)),
          basic_default_pixel_coord<TXY>(std::move(xr), std::move(yb)));
    }
  }
};
struct do_from_xywh {
  template <typename TXY, has_bbox_init<TXY> T>
  constexpr bounding_box auto operator()(T const &, TXY x, TXY y, TXY w,
                                         TXY h) const {
    if constexpr (has_from_xywh<T, TXY>) {
      return T::from_xywh(x, y, w, h);
    } else {
      return do_from_xyxy{}(T{}, x, y, x + w, y + h);
    }
  }
};
struct do_from_tlbr {
  template <pixel_coord TC, typename T>
    requires(has_from_tlbr<T, TC> ||
             has_bbox_init<T, decltype(call::x_of(std::declval<TC &&>()))>)
  constexpr bounding_box auto operator()(T const &, TC &&tl, TC &&br) const {
    if constexpr (has_from_tlbr<T, TC>) {
      return T::from_tlbr(std::forward<TC>(tl), std::forward<TC>(br));
    } else {
      return do_from_xyxy{}(T{}, call::x_of(tl), call::y_of(tl), call::x_of(br),
                            call::y_of(br));
    }
  }
};

} // namespace impl

inline constexpr auto render_direct = impl::do_render_direct;
inline constexpr auto draw_pixels = impl::do_draw_pixels;
inline constexpr auto center = impl::do_center{};
inline constexpr impl::do_set_displayed set_displayed;
inline constexpr impl::do_render_text render_text;
inline constexpr impl::do_set_text set_text;

inline constexpr auto red = [](colour auto &&c, auto &&...vs) -> auto && {
  return extend::red(std::forward<decltype(c)>(c),
                     std::forward<decltype(vs)>(vs)...);
};
inline constexpr auto green = [](colour auto &&c, auto &&...vs) -> auto && {
  return extend::green(std::forward<decltype(c)>(c),
                       std::forward<decltype(vs)>(vs)...);
};
inline constexpr auto blue = [](colour auto &&c, auto &&...vs) -> auto && {
  return extend::blue(std::forward<decltype(c)>(c),
                      std::forward<decltype(vs)>(vs)...);
};
inline constexpr auto alpha = [](colour auto &&c, auto &&...vs) -> auto && {
  return extend::alpha(std::forward<decltype(c)>(c),
                       std::forward<decltype(vs)>(vs)...);
};

template <typename T, typename TXY>
constexpr auto box_from_xyxy(TXY xl, TXY yt, TXY xr, TXY yb,
                             std::type_identity<T> = {}) {
  return impl::do_from_xyxy{}(extend_api_t<T>{}, xl, yt, xr, yb);
}
template <typename T, typename TXY>
constexpr auto box_from_xywh(TXY x, TXY y, TXY w, TXY h,
                             std::type_identity<T> = {}) {
  return impl::do_from_xywh{}(extend_api_t<T>{}, x, y, w, h);
}
template <typename T, typename TC>
constexpr auto box_from_tlbr(TC &&tl, TC &&br, std::type_identity<T> = {}) {
  return impl::do_from_tlbr{}(extend_api_t<T>{}, std::forward<TC>(tl),
                              std::forward<TC>(br));
}
template <typename T, typename TX>
concept mut_box_pointer =
    bp::pointer_type<T> && mutable_bounding_box<bp::pointer_reference_t<T>, TX>;

template <typename T, typename TV1, typename TV2>
concept mut_box_pair =
    (mut_box_pointer<T, TV1> || call::is_placeholder_v<TV1>) &&
    (mut_box_pointer<T, TV2> || call::is_placeholder_v<TV2>);

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
  _set_xx_or_yy(b, lx, rx, call::tl_x, call::br_x);
}
template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_yy(T b, TV1 ty, TV2 by) {
  _set_xx_or_yy(b, ty, by, call::tl_y, call::br_y);
}

template <typename TX, mut_box_pointer<TX> T>
constexpr auto split_x(T b, TX x) {
  assert(b != nullptr);
  auto res = call::box_from_xyxy<bp::pointer_reference_t<T>>(
      x, call::tl_y(*b), call::br_x(*b), call::br_y(*b));
  call::br_x(*b, x);
  return res;
}
template <typename TY, mut_box_pointer<TY> T>
constexpr auto split_y(T b, TY y) {
  assert(b != nullptr);
  auto res = call::box_from_xyxy<bp::pointer_reference_t<T>>(
      call::tl_x(*b), y, call::br_x(*b), call::br_y(*b));
  call::br_y(*b, y);
  return res;
}

template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_left(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::width(b));
  auto org_lx = call::tl_x(b);
  auto split_x = static_cast<decltype(org_lx)>(org_lx + v);
  call::set_xx(&b, split_x, keep_current);
  return call::box_from_xyxy<bp::pointer_reference_t<T>>(
      org_lx, call::tl_y(b), split_x, call::br_y(b));
}
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_above(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::height(b));
  auto org_y = call::tl_y(b);
  auto split_y = static_cast<decltype(org_y)>(org_y + v);
  call::set_yy(&b, split_y, keep_current);
  return call::box_from_xyxy<bp::pointer_reference_t<T>>(
      call::tl_x(b), org_y, call::br_x(b), split_y);
}

} // namespace call
template <typename T, typename TR>
concept has_render_direct = call::impl::has_render_direct<T, TR>;

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

#undef CGUI_PIX_FUNC_IMPL
#undef CGUI_EXTRA_PARAMS
#undef CGUI_EXTRA_ARGS
#undef CGUI_EXTRA_ARGS_COMMA
#undef CGUI_CALL_IMPL

#endif // COMPONENT_GUI_CGUI_TYPES_HPP

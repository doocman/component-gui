
#ifndef COMPONENT_GUI_CGUI_GEOMETRY_HPP
#define COMPONENT_GUI_CGUI_GEOMETRY_HPP

#include <concepts>
#include <ranges>
#include <type_traits>

#include <cgui/cgui-call.hpp>
#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/type_traits.hpp>
#include <cgui/warnings.hpp>

namespace cgui {

/// @brief Concept to check if a type T meets the range condition for values of
/// type TX. The range_condition should from a test value and min/max values
/// determine if the test-value is inside the range of min max. Implementations
/// are e.g. open-range, closed-range and semi-open (open-closed).
template <typename T, typename TX>
concept range_condition = requires(T t, TX v) {
  { t(v, v, v) } -> std::convertible_to<bool>;
};

/// @brief Trait to extract pixel type for a given type T.
template <typename> struct pixel_type;

/// @brief Concept to define valid pixel coordinate value types.
template <typename T>
concept pixel_coord_value_t = std::integral<T> || std::floating_point<T>;

/// @brief Concept to check if a type has a nested pixel_type.
template <typename T>
concept member_pixel_type = requires() { typename T::pixel_type; } &&
                            pixel_coord_value_t<typename T::pixel_type>;

/// @brief Specialization of pixel_type for types with a nested pixel_type.
template <member_pixel_type T> struct pixel_type<T> {
  using type = typename T::pixel_type;
};

/// @brief Fallback specialization for pixel_type for types without pixel_type
/// member that also have a const/volatile/reference decorator.
template <typename T>
  requires(!bp::pure_value<T> && !member_pixel_type<T>)
struct pixel_type<T> : pixel_type<std::remove_cvref_t<T>> {};

/// @brief Concept to check if a type has a valid pixel_type alias.
template <typename T>
concept has_pixel_type = requires() { typename pixel_type<T>::type; } &&
                         pixel_coord_value_t<typename pixel_type<T>::type>;

/// @brief Helper alias to retrieve the pixel_type for a type.
template <typename T> using pixel_type_t = typename pixel_type<T>::type;

/// @brief Concept to check if a type is a valid pixel coordinate type (ignoring
/// cv-ref).
template <typename T>
concept pixel_coord_value_cv_t = pixel_coord_value_t<std::remove_cvref_t<T>>;

/// @brief Concept to check if a type is a pixel coordinate.
template <typename T>
concept pixel_coord = requires(T &&t) {
  { call::x_of(t) } -> pixel_coord_value_cv_t;
  { call::y_of(t) } -> pixel_coord_value_cv_t;
};

/// @brief Concept for pixel coordinates that can be set to a value.
template <typename T, typename TVal>
concept pixel_coord_set =
    requires(bp::as_forward<T> t, bp::as_forward<TVal> v) {
      call::x_of(*t, *v);
      call::y_of(*t, *v);
    };

/// @brief Concept for pixel coordinates that support reference assignment.
template <typename T, typename TVal>
concept pixel_coord_ref = requires(bp::as_forward<T> t) {
  { call::x_of(*t) } -> std::assignable_from<TVal>;
  { call::y_of(*t) } -> std::assignable_from<TVal>;
};

/// @brief Concept for pixel coordinates that support mutation.
template <typename T, typename TVal>
concept pixel_coord_mut = pixel_coord_ref<T, TVal> || pixel_coord_set<T, TVal>;

/// @brief Basic structure for representing pixel coordinates.
template <typename T> struct basic_pixel_coord {
  T x; ///< X coordinate
  T y; ///< Y coordinate

  /// @brief Converts this coordinate to a non-const value of the same type.
  template <typename T2 = T>
    requires(!std::is_same_v<T2, std::remove_cvref_t<T2>>)
  constexpr explicit(false) operator std::remove_cvref_t<T>() const {
    return {.x = x, .y = y};
  }
};

using default_pixel_coord = basic_pixel_coord<int>;

/// @brief Retrieves the x-coordinate from a basic pixel coordinate.
template <typename T> constexpr T x_of(basic_pixel_coord<T> const &c) {
  return c.x;
}

/// @brief Retrieves the y-coordinate from a basic pixel coordinate.
template <typename T> constexpr T y_of(basic_pixel_coord<T> const &c) {
  return c.y;
}

/// @brief Returns a reference to the x-coordinate of a basic pixel coordinate.
template <typename T> constexpr T &x_of(basic_pixel_coord<T> &c) { return c.x; }

/// @brief Returns a reference to the y-coordinate of a default pixel
/// coordinate.
template <typename T> constexpr T &y_of(basic_pixel_coord<T> &c) { return c.y; }

/// @brief Concept to check if a type is mutable by TFrom and is a valid pixel
/// coordinate value.
template <typename T, typename TFrom>
concept mutable_pixel_coord_value =
    bp::is_mutable_by<T, TFrom> && pixel_coord_value_cv_t<T>;

/// Maps a coordinate to a new coordinate through function f.
/// \tparam T Resulting type.
/// \tparam U
/// \tparam F
/// \param u
/// \param f
/// \return mapped coordinate of type T.
template <pixel_coord T, pixel_coord U, typename F>
constexpr T map_coord(U const &u, F &&f) {
  return {f(call::x_of(u)), f(call::y_of(u))};
}

/// @brief Structure representing a default rectangular bounding box.
struct default_rect {
  default_pixel_coord tl; ///< Top-left coordinate
  default_pixel_coord br; ///< Bottom-right coordinate

  /// @brief Creates a default_rect from given coordinates.
  /// @param x1 Left x-coordinate.
  /// @param y1 Top y-coordinate.
  /// @param x2 Right x-coordinate.
  /// @param y2 Bottom y-coordinate.
  static constexpr default_rect from_xyxy(int x1, int y1, int x2, int y2) {
    return {{x1, y1}, {x2, y2}};
  }
};

/// @brief Returns the top-left coordinate of a rectangle.
constexpr default_pixel_coord top_left(default_rect const &r) noexcept {
  return r.tl;
}

/// @brief Returns a reference to the top-left coordinate of a rectangle.
constexpr default_pixel_coord &top_left(default_rect &r) noexcept {
  return r.tl;
}

/// @brief Returns the bottom-right coordinate of a rectangle.
constexpr default_pixel_coord bottom_right(default_rect const &r) noexcept {
  return r.br;
}

/// @brief Returns a reference to the bottom-right coordinate of a rectangle.
constexpr default_pixel_coord &bottom_right(default_rect &r) noexcept {
  return r.br;
}

/// @brief Concept for types that represent a size with width and height.
template <typename T>
concept size_wh = requires(T const &t) {
  { call::width(t) } -> bp::not_void;
  { call::height(t) } -> bp::not_void;
};

/// @brief Basic structure for representing size with width and height.
template <typename T = int> struct basic_size_wh {
  T w;
  T h;

  /// @brief Retrieves the width of the size. Best invoked with call::width()
  static constexpr auto &&width(bp::cvref_type<basic_size_wh> auto &&wh) {
    return std::forward<decltype(wh)>(wh).w;
  }
  /// @brief Retrieves the height of the size. Best invoked with call::height()
  static constexpr auto &&height(bp::cvref_type<basic_size_wh> auto &&wh) {
    return std::forward<decltype(wh)>(wh).h;
  }
};
using default_size_wh = basic_size_wh<int>;

/// @brief Concept for bounding box types.
template <typename T>
concept bounding_box = requires(T const &t) {
  { call::l_x(t) } -> bp::not_void;
  { call::t_y(t) } -> bp::not_void;
  { call::r_x(t) } -> bp::not_void;
  { call::b_y(t) } -> bp::not_void;
  { call::width(t) } -> bp::not_void;
  { call::height(t) } -> bp::not_void;
};

/// @brief Concept for mutable bounding box types.
template <typename T, typename TFrom>
concept mutable_bounding_box =
    requires(bp::as_forward<T> t, bp::as_forward<TFrom> v) {
      call::l_x(*t, *v);
      call::t_y(*t, *v);
      call::r_x(*t, *v);
      call::b_y(*t, *v);
      call::width(*t, *v);
      call::height(*t, *v);
    };

/// Generates a lazy view of all (integer) pointer between left and right x of
/// b.
constexpr auto x_view(bounding_box auto &&b) {
  return std::views::iota(call::l_x(b), call::r_x(b));
}

/// Generates a lazy view of all (integer) pointer between top and bottom y of
/// b.
constexpr auto y_view(bounding_box auto &&b) {
  return std::views::iota(call::t_y(b), call::b_y(b));
}

/// Concept to check that a type is a pointer to a mutable bounding box.
template <typename T, typename TX>
concept mut_box_pointer =
    bp::pointer_type<T> && mutable_bounding_box<bp::dereferenced_t<T>, TX>;

/// Specialised concept, requires that T is a pointer to a mutable box that can
/// mutate from TVs or, if TVs is a placeholder, from the evaluation of TVs for
/// all TVs.
template <typename T, typename... TVs>
concept mut_box_pair =
    ((mut_box_pointer<T, TVs> || is_placeholder_v<TVs>) && ...);

/// @cond
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
    has_from_tlbr<T, basic_pixel_coord<std::remove_cvref_t<TC>>> ||
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
          basic_pixel_coord<TXY>(std::move(xl), std::move(yt)),
          basic_pixel_coord<TXY>(std::move(xr), std::move(yb)));
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

template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T, typename TTL,
          typename TBR>
constexpr void set_xx_or_yy(T b, TV1 tl, TV2 br, TTL getset1, TBR getset2) {
  if constexpr (is_placeholder_v<TV1>) {
    impl::set_xx_or_yy(b, tl(getset1, *b), br, getset1, getset2);
  } else if constexpr (is_placeholder_v<TV2>) {
    impl::set_xx_or_yy(b, tl, br(getset2, *b), getset1, getset2);
  } else {
    getset1(*b, tl);
    getset2(*b, br);
  }
}

}; // namespace impl
/// @endcond

/// Creates a box (presumably of type T) from two XY coordinates.
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

/// Creates a box (presumably of type T) from a top-left coordinate + width and
/// height.
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

/// Creates a box (presumably of type T) from two coordinate types.
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

/// Set the pointer b to have the x-values lx-rx. Use "keep_current" to only
/// change a single value. May use placeholders.
template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_xx(T b, TV1 lx, TV2 rx) {
  impl::set_xx_or_yy(b, lx, rx, call::l_x, call::r_x);
}

/// Set the pointer b to have the y-values ty-by. Use "keep_current" to only
/// change a single value. May use placeholders.
template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_yy(T b, TV1 ty, TV2 by) {
  impl::set_xx_or_yy(b, ty, by, call::t_y, call::b_y);
}

/// Create a bounding box that has the same dimensions as b, but with its top
/// left corner at tl.
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

/// Split the bounding box pointed to by T at x. b keeps the left part while the
/// returned area is the right part. Use "trim_from_*" to use width/height
/// arithmetics instead.
template <typename TX, mut_box_pointer<TX> T>
constexpr auto split_x(T b, TX x) {
  CGUI_ASSERT(b != nullptr);
  auto res = box_from_xyxy<bp::dereferenced_t<T>>(x, call::t_y(*b),
                                                  call::r_x(*b), call::b_y(*b));
  call::r_x(*b, x);
  return res;
}

/// Split the bounding box pointed to by T at y. b keeps the upper part while
/// the returned area is the lower part. Use "trim_from_*" to use width/height
/// arithmetics instead.
template <typename TY, mut_box_pointer<TY> T>
constexpr auto split_y(T b, TY y) {
  CGUI_ASSERT(b != nullptr);
  auto res = box_from_xyxy<bp::dereferenced_t<T>>(call::l_x(*b), y,
                                                  call::r_x(*b), call::b_y(*b));
  call::b_y(*b, y);
  return res;
}

/// Cut away a part v from the left side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_left(T bptr, TV v) {
  CGUI_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  CGUI_ASSERT(v <= call::width(b));
  auto org_lx = call::l_x(b);
  auto split_x = static_cast<decltype(org_lx)>(org_lx + v);
  set_xx(&b, split_x, keep_current);
  return box_from_xyxy<bp::dereferenced_t<T>>(org_lx, call::t_y(b), split_x,
                                              call::b_y(b));
}

/// Cut away a part v from the top side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_above(T bptr, TV v) {
  CGUI_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  CGUI_ASSERT(v <= call::height(b));
  auto org_y = call::t_y(b);
  auto split_y = static_cast<decltype(org_y)>(org_y + v);
  set_yy(&b, split_y, keep_current);
  return box_from_xyxy<bp::dereferenced_t<T>>(call::l_x(b), org_y, call::r_x(b),
                                              split_y);
}

/// Cut away a part v from the right side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_right(T bptr, TV v) {
  CGUI_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  CGUI_ASSERT(v <= call::width(b));
  call::width(b, call::width(b) - v);
  return box_from_xywh<bp::dereferenced_t<T>>(call::r_x(b), call::t_y(b), v,
                                              call::height(b));
}

/// Cut away a part v from the lower side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_below(T bptr, TV v) {
  CGUI_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  CGUI_ASSERT(v <= call::height(b));
  call::height(b, call::height(b) - v);
  return box_from_xywh<bp::dereferenced_t<T>>(call::l_x(b), call::b_y(b),
                                              call::width(b), v);
}

/// Returns true if width and height are non-negative.
constexpr bool valid_box(bounding_box auto const &b) {
  return (call::width(b) >= 0) && (call::height(b) >= 0);
}

/// Creates a larger box that includes the smaller boxes. Does not check for
/// empty boxes. Use box_add when the boxes may be empty or non-valid.
template <typename TRes = void, bounding_box T1, bounding_box T2>
constexpr auto box_union(T1 const &b1, T2 const &b2) {
  CGUI_ASSERT(valid_box(b1));
  CGUI_ASSERT(valid_box(b2));
  using result_t =
      std::conditional_t<std::is_void_v<TRes>, std::common_type<T1, T2>,
                         std::type_identity<TRes>>::type;
  return box_from_xyxy<result_t>(std::min(call::l_x(b1), call::l_x(b2)),
                                 std::min(call::t_y(b1), call::t_y(b2)),
                                 std::max(call::r_x(b1), call::r_x(b2)),
                                 std::max(call::b_y(b1), call::b_y(b2)));
}

/// Creates a larger box that is the intersection of both b1 and b2.
template <typename TRes = void, bounding_box T1, bounding_box T2>
constexpr auto box_intersection(T1 const &b1, T2 const &b2) {
  CGUI_ASSERT(valid_box(b1));
  CGUI_ASSERT(valid_box(b2));
  using result_t = typename std::conditional_t<std::is_void_v<TRes>,
                                               std::common_type<T1, T2>,
                                               std::type_identity<TRes>>::type;
  auto lx = std::max(call::l_x(b1), call::l_x(b2));
  auto ty = std::max(call::t_y(b1), call::t_y(b2));
  auto rx = std::min(call::r_x(b1), call::r_x(b2));
  auto by = std::min(call::b_y(b1), call::b_y(b2));
  return box_from_xyxy<result_t>(lx, ty, std::max(rx, lx), std::max(by, ty));
}

/// Creates a new pixel_coord that has moved left by val.
constexpr auto nudge_left(pixel_coord auto c, auto &&val) {
  call::x_of(c, call::x_of(c) - val);
  return c;
}
/// Creates a new pixel_coord that has moved right by val.
constexpr auto nudge_right(pixel_coord auto c, auto &&val) {
  return nudge_left(c, -val);
}
/// Creates a new pixel_coord that has moved up by val.
constexpr auto nudge_up(pixel_coord auto c, auto &&val) {
  call::y_of(c, call::y_of(c) - val);
  return c;
}
/// Creates a new pixel_coord that has moved down by val.
constexpr auto nudge_down(pixel_coord auto c, auto &&val) {
  return nudge_up(c, -val);
}

/// Creates a new box that has moved left by val.
constexpr auto nudge_left(bounding_box auto b, auto &&val) {
  set_xx(&b, call::l_x(b) - val, call::r_x(b) - val);
  return std::forward<decltype(b)>(b);
}
/// Creates a new box that has moved right by val.
constexpr auto nudge_right(bounding_box auto b, auto &&val) {
  return nudge_left(b, -val);
}
/// Creates a new box that has moved up by val.
constexpr auto nudge_up(bounding_box auto b, auto &&val) {
  set_yy(&b, call::t_y(b) - val, call::b_y(b) - val);
  return std::forward<decltype(b)>(b);
}
/// Creates a new box that has moved down by val.
constexpr auto nudge_down(bounding_box auto b, auto &&val) {
  return nudge_up(b, -val);
}

/// Range checker that models the open range min < c < max.
inline constexpr auto inside_open_range = [](auto &&c, auto &&min, auto &&max) {
  return (min < c) && (c < max);
};
/// Range checker that models the closed range min <= c <= max.
inline constexpr auto inside_closed_range =
    [](auto &&c, auto &&min, auto &&max) { return (min <= c) && (c <= max); };
/// Range checker that models the closed-open range min <= c < max.
inline constexpr auto inside_semiopen_range =
    [](auto &&c, auto &&min, auto &&max) { return (min <= c) && (c < max); };

using inside_open_range_t = decltype(inside_open_range);
using inside_closed_range_t = decltype(inside_closed_range);
using inside_semiopen_range_t = decltype(inside_semiopen_range);

/// Check if coordinate c is inside box b, by the range checking policy
/// inside_range.
template <bounding_box TB, pixel_coord TC = default_pixel_coord,
          range_condition<decltype(call::x_of(std::declval<TC>()))> TRC =
              inside_semiopen_range_t>
constexpr bool hit_box(TB const &b, TC const &c, TRC &&inside_range = {}) {
  CGUI_ASSERT(valid_box(b));
  return inside_range(call::x_of(c), call::l_x(b), call::r_x(b)) &&
         inside_range(call::y_of(c), call::t_y(b), call::b_y(b));
}

/// True if all corners of inner is inside the outer box.
template <bounding_box TB1, bounding_box TB2>
constexpr bool box_includes_box(TB1 const &outer, TB2 const &inner) {
  return hit_box(outer, call::top_left(inner), inside_closed_range) &&
         hit_box(outer, call::bottom_right(inner), inside_closed_range);
}

/// Returns true if the box is empty.
constexpr bool empty_box(bounding_box auto const &b) {
  return call::width(b) == 0 || call::height(b) == 0;
}

/// Copies a box of type T2 into a box of type T.
template <bounding_box T, bounding_box T2> constexpr T copy_box(T2 &&b) {
  if constexpr (bp::cvref_type<T2, T>) {
    return std::forward<T2>(b);
  } else if constexpr (std::constructible_from<T, T2 &&>) {
    return T(std::forward<T2>(b));
  } else if constexpr (impl::has_from_xywh<T, decltype(call::l_x(b))>) {
    return box_from_xywh<T>(call::l_x(b), call::t_y(b), call::width(b),
                            call::height(b));
  } else {
    return box_from_xyxy<T>(call::l_x(b), call::t_y(b), call::r_x(b),
                            call::b_y(b));
  }
}

/// Creates a box of type T by applying map_f on b.
/// \tparam T Return type
/// \tparam T2 Incoming box type
/// \param b box to morph
/// \param map_f function f(x)->x that maps all coordinates and sizes. The
/// map-function should be linear, as it does not know if it is called with the
/// top-left + bottom-right coordinates or with the top-left + width & height
/// coordinates.
/// \return new morphed box.
template <bounding_box T, bounding_box T2>
constexpr T map_box(T2 const &b, auto &&map_f) {
  if constexpr (impl::has_from_xywh<T, decltype(call::l_x(b))>) {
    return box_from_xywh<T>(map_f(call::l_x(b)), map_f(call::t_y(b)),
                            map_f(call::width(b)), map_f(call::height(b)));
  } else {
    return box_from_xyxy<T>(map_f(call::l_x(b)), map_f(call::t_y(b)),
                            map_f(call::r_x(b)), map_f(call::b_y(b)));
  }
}

/// Version of box_union that supports empty boxes.
template <typename TRes = void, typename TB1, typename TB2>
constexpr auto box_add(TB1 const &b1,
                       TB2 const &b2) -> decltype(box_union<TRes>(b1, b2)) {
  using result_t = decltype(box_union<TRes>(b1, b2));
  if (empty_box(b1)) {
    return copy_box<result_t>(b2);
  }
  if (empty_box(b2)) {
    return copy_box<result_t>(b1);
  }
  return box_union<TRes>(b1, b2);
}

template <typename T>
concept has_pixel_scale = requires(T const &t) {
  { call::pixel_scale(t) } -> std::convertible_to<double>;
};

struct pixel_size_tag {};
struct point_size_tag {};
constexpr auto convert(pixel_size_tag, point_size_tag, auto &&in,
                       has_pixel_scale auto &&scaler) {
  return in / call::pixel_scale(scaler);
}
constexpr auto convert(point_size_tag, pixel_size_tag, auto &&in,
                       has_pixel_scale auto &&scaler) {
  return in * call::pixel_scale(scaler);
}
template <bp::empty_type Tag, typename In, has_pixel_scale Scaler>
constexpr auto convert(Tag, Tag, In &&in, Scaler const &) {
  return in;
}
struct dummy_scaler {
  static constexpr int pixel_scale() { return 1; }
};

template <typename T, typename To, typename ValT = int,
          typename Scaler = dummy_scaler>
concept can_convert_tag =
    bp::empty_type<T> && bp::empty_type<To> &&
    requires(T t, To to, ValT &&v, Scaler const &s) { convert(t, to, v, s); };
template <typename T, typename To, typename ValT = int,
          typename Scaler = dummy_scaler>
concept bidirection_convert_tag = can_convert_tag<T, To, ValT, Scaler> &&
                                  can_convert_tag<To, T, ValT, Scaler>;

template <typename T, typename ValT = int, typename Scaler = dummy_scaler>
concept pixelpoint_tag =
    bidirection_convert_tag<T, pixel_size_tag, ValT, Scaler> &&
    bidirection_convert_tag<T, point_size_tag, ValT, Scaler>;

template <pixelpoint_tag From, pixelpoint_tag To, has_pixel_scale S>
struct pixelpoint_converter {
  S &&s_;
  constexpr auto operator()(auto &&in) const {
    return convert(From{}, To{}, in, s_);
  }
};

template <typename T>
concept has_tag_t = requires() { typename std::remove_cvref_t<T>::tag_t; };

template <typename T> using tag_t_of = typename std::remove_cvref_t<T>::tag_t;

template <typename T>
concept size_tagged = has_tag_t<T> && pixelpoint_tag<tag_t_of<T>>;

template <typename T>
concept scalar = std::is_integral_v<T> || std::is_floating_point_v<T>;

template <typename T>
concept is_geometric = bounding_box<T> || pixel_coord<T>;
template <typename T, typename U>
concept same_geometry_as =
    is_geometric<T> && is_geometric<U> && bounding_box<T> == bounding_box<U> &&
    pixel_coord<T> == pixel_coord<U>;

template <pixelpoint_tag SizeTag, typename T> class pixelpoint_unit {
  T value_;

  template <pixelpoint_tag ST2, typename U, has_pixel_scale S>
  static constexpr T conv(pixelpoint_unit<ST2, U> const &v, S const &scaler) {
    if constexpr (std::is_same_v<ST2, SizeTag> &&
                  std::constructible_from<T, U>) {
      return T(v.value());
    } else {
      auto conv = pixelpoint_converter<ST2, SizeTag, S const &>(scaler);
      if constexpr (bounding_box<T>) {
        return map_box<T>(v.value(), conv);
      } else if constexpr (pixel_coord<T>) {
        return map_coord<T>(v.value(), conv);
      } else {
        static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
        return conv(v.value());
      }
    }
  }

public:
  constexpr T &value() noexcept { return value_; }
  constexpr T const &value() const noexcept { return value_; }

#define CGUI_PIXELPOINT_WRAP_RETURN_(X) pixelpoint_unit<SizeTag, X>
#define CGUI_PIXELPOINT_IMPL_CONSTRAINT_(X) call::impl::has_##X<U, Ts...>
#define CGUI_PIXELPOINT_CALL_CONSTRAINT_(X)                                    \
  requires(bp::as_forward<U> u, bp::as_forward<Ts>... args) {                  \
    call::X(*u, *args...);                                                     \
  }

#define CGUI_FWD_CALL_(X, RETURN_TYPE, CONSTRAINT)                             \
  template <bp::cvref_type<T> U, typename... Ts>                               \
    requires(CONSTRAINT(X))                                                    \
  static constexpr auto X(U &&u, Ts &&...args)                                 \
      -> RETURN_TYPE(                                                          \
          decltype(call::X(std::forward<U>(u), std::forward<Ts>(args)...))) {  \
    return call::X(std::forward<U>(u), std::forward<Ts>(args)...);             \
  }

  constexpr pixelpoint_unit(pixelpoint_unit const &) noexcept(
      std::is_nothrow_copy_constructible_v<T>) = default;
  constexpr pixelpoint_unit &operator=(pixelpoint_unit const &) noexcept(
      std::is_nothrow_copy_assignable_v<T>) = default;
  constexpr pixelpoint_unit(pixelpoint_unit &&) noexcept(
      std::is_nothrow_move_constructible_v<T>) = default;
  constexpr pixelpoint_unit &operator=(pixelpoint_unit &&) noexcept(
      std::is_nothrow_move_assignable_v<T>) = default;

  template <typename ST2, typename T2, has_pixel_scale S>
    requires(same_geometry_as<T, T2> || (scalar<T> && scalar<T2>))
  constexpr pixelpoint_unit(pixelpoint_unit<ST2, T2> const &v, S const &s)
      : value_(conv(v, s)) {}

  template <typename T2>
    requires(std::constructible_from<T, T2>)
  constexpr explicit(!std::convertible_to<T2, T>)
      pixelpoint_unit(pixelpoint_unit<SizeTag, T2> v)
      : value_(v.value()) {}

  template <typename T2>
    requires(std::constructible_from<T, T2>)
  constexpr explicit pixelpoint_unit(T2 &&v) : value_(std::forward<T2>(v)) {}
  template <typename T2>
    requires(std::constructible_from<T, T2>)
  constexpr pixelpoint_unit(SizeTag, T2 &&v) : value_(std::forward<T2>(v)) {}

  static constexpr decltype(auto) x_of(bp::cvref_type<T> auto &&v,
                                       auto &&...args)
    requires(requires() { call::x_of(v, args...); })
  {}

  CGUI_FWD_CALL_(x_of, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(y_of, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(l_x, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(r_x, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(t_y, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(b_y, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(top_left, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(bottom_right, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(height, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)
  CGUI_FWD_CALL_(width, CGUI_PIXELPOINT_WRAP_RETURN_,
                 CGUI_PIXELPOINT_IMPL_CONSTRAINT_)

  using tag_t = SizeTag;

#undef CGUI_PIXELPOINT_WRAP_RETURN_
#undef CGUI_FWD_CALL_
#undef CGUI_PIXELPOINT_IMPL_CONSTRAINT_
#undef CGUI_PIXELPOINT_CALL_CONSTRAINT_
};

template <typename SizeTag, typename T>
pixelpoint_unit(SizeTag,
                T &&) -> pixelpoint_unit<SizeTag, std::remove_cvref_t<T>>;

} // namespace cgui

#endif


#ifndef COMPONENT_GUI_CGUI_GEOMETRY_HPP
#define COMPONENT_GUI_CGUI_GEOMETRY_HPP

#include <concepts>

#include <cgui/cgui-call.hpp>

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
template <typename T> constexpr int x_of(basic_pixel_coord<T> const &c) {
  return c.x;
}

/// @brief Retrieves the y-coordinate from a basic pixel coordinate.
template <typename T> constexpr int y_of(basic_pixel_coord<T> const &c) {
  return c.y;
}

/// @brief Returns a reference to the x-coordinate of a basic pixel coordinate.
template <typename T> constexpr int &x_of(basic_pixel_coord<T> &c) {
  return c.x;
}

/// @brief Returns a reference to the y-coordinate of a default pixel
/// coordinate.
template <typename T> constexpr int &y_of(basic_pixel_coord<T> &c) {
  return c.y;
}

/// @brief Concept to check if a type is mutable by TFrom and is a valid pixel
/// coordinate value.
template <typename T, typename TFrom>
concept mutable_pixel_coord_value =
    bp::is_mutable_by<T, TFrom> && pixel_coord_value_cv_t<T>;

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
  assert(b != nullptr);
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
  assert(b != nullptr);
  auto res = box_from_xyxy<bp::dereferenced_t<T>>(call::l_x(*b), y,
                                                  call::r_x(*b), call::b_y(*b));
  call::b_y(*b, y);
  return res;
}

/// Cut away a part v from the left side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_left(T bptr, TV v) {
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::width(b));
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
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::height(b));
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
  assert(bptr != nullptr);
  auto &b = *bptr;
  assert(v <= call::width(b));
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

/// Version of box_union that supports empty boxes.
template <typename TRes = void, typename TB1, typename TB2>
constexpr auto box_add(TB1 const &b1, TB2 const &b2)
    -> decltype(box_union<TRes>(b1, b2)) {
  using result_t = decltype(box_union<TRes>(b1, b2));
  if (empty_box(b1)) {
    return copy_box<result_t>(b2);
  }
  if (empty_box(b2)) {
    return copy_box<result_t>(b1);
  }
  return box_union<TRes>(b1, b2);
}

} // namespace cgui

#endif


#ifndef COMPONENT_GUI_ASP_GEOMETRY_HPP
#define COMPONENT_GUI_ASP_GEOMETRY_HPP

#include <cmath>
#include <concepts>
#include <ranges>
#include <type_traits>

#include <asp/import/mp-units.hpp>

#include <asp/call.hpp>
#include <asp/std-backport/concepts.hpp>
#include <asp/std-backport/limits.hpp>
#include <asp/std-backport/type_traits.hpp>
#include <asp/warnings.hpp>

namespace asp {
ASP_EXPORT_BEGIN
template <typename T>
concept has_rep = requires() { typename std::remove_cvref_t<T>::rep; };
template <typename T>
using representation_of_t = typename std::remove_cvref_t<T>::rep;

template <typename T>
concept has_width_height = requires(T const &t) {
  { call::width(t) } -> bp::not_void;
  { call::height(t) } -> bp::not_void;
};
/// @brief Concept for bounding box types.
template <typename T>
concept bounding_box = has_width_height<T> && requires(T const &t) {
  { call::l_x(t) } -> bp::not_void;
  { call::t_y(t) } -> bp::not_void;
  { call::r_x(t) } -> bp::not_void;
  { call::b_y(t) } -> bp::not_void;
};
template <typename T>
concept two_dimensional_coordinate = requires(T const &t) {
  { call::x_of(t) } -> bp::not_void;
  { call::y_of(t) } -> bp::not_void;
};
template <typename T>
concept has_unit = requires() { std::remove_cvref_t<T>::unit; };
template <typename T, auto U>
concept has_unit_of = has_unit<T> && std::remove_cvref_t<T>::unit == U;
template <typename T>
concept is_any_quantity = mp_units::Quantity<std::remove_cvref_t<T>>;
template <typename T>
concept is_any_quantity_point = mp_units::QuantityPoint<std::remove_cvref_t<T>>;
template <typename Q, auto R>
concept is_quantity =
    is_any_quantity<Q> && mp_units::Reference<decltype(R)> &&
    mp_units::QuantityOf<std::remove_cvref_t<Q>, get_quantity_spec(R)> &&
    has_unit_of<Q, get_unit(R)>;
template <typename QP, auto R>
concept is_quantity_point =
    is_any_quantity_point<QP> && mp_units::Reference<decltype(R)> &&
    mp_units::QuantityPointOf<std::remove_cvref_t<QP>, get_quantity_spec(R)> &&
    has_unit_of<QP, get_unit(R)>;

template <typename T>
concept two_dimensional_point = two_dimensional_coordinate<T> && has_unit<T> && has_rep<T> && 
  requires(T const& t) {
	  { call::x_of(t) } -> is_any_quantity_point;
	  { call::y_of(t) } -> is_any_quantity_point;
  };
template <typename T>
concept two_dimensional_delta = two_dimensional_coordinate<T> && has_unit<T> && 
  requires(T const& t) {
	  { call::x_of(t) } -> is_any_quantity;
	  { call::y_of(t) } -> is_any_quantity;
  };

template <has_unit T>
inline constexpr auto unit_of_type = std::remove_cvref_t<T>::unit;

template <typename T, typename U>
concept same_unit_as =
    has_unit<T> && has_unit<U> && unit_of_type<T> == unit_of_type<U>;
	
template <typename T, auto R>
concept two_dimensional_delta_of = two_dimensional_delta<T> && unit_of_type<T> == R;

template <typename...>
struct common_unit_base {};
template <has_unit T, same_unit_as<T>... Ts>
struct common_unit_base<T, Ts...> {
  static constexpr mp_units::Unit auto unit = unit_of_type<T>;
};

template <typename... Ts>
struct common_rep_base : common_unit_base<Ts...> {};
template <has_rep T, has_rep... Ts>
requires (std::same_as<representation_of_t<T>, representation_of_t<Ts>> &&...)
struct common_rep_base<T, Ts...> : common_unit_base<T, Ts...> {
  using rep = representation_of_t<T>;
};

template <typename... Ts>
struct common_aliases_base : common_rep_base<Ts...> {};

template <is_any_quantity_point QP>
constexpr QP center_between(QP lhs, QP rhs) {
  if constexpr (std::floating_point<typename QP::rep>) {
    auto sum_quantity_from_zero = lhs.quantity_from(QP::point_origin) +
                                  rhs.quantity_from(QP::point_origin);
    return QP(sum_quantity_from_zero / 2, QP::point_origin);
  } else {
    static_assert(std::integral<typename QP::rep>);
    return lhs + (rhs - lhs) / 2;
  }
}
template <two_dimensional_coordinate C>
constexpr C center_between(C lhs, C rhs) {
  return C(center_between(call::x_of(lhs), call::x_of(rhs)),
           center_between(call::y_of(lhs), call::y_of(rhs)));
}

template <typename T>
concept has_reference_member = requires() { T::reference; };
template <typename T>
concept has_unit_reference =
    has_reference_member<T> && mp_units::Reference<decltype(T::reference)>;
struct no_unit_reference {
  friend constexpr bool
  operator==(const no_unit_reference &,
             const no_unit_reference &) noexcept = default;
};

template <typename T> constexpr auto get_unit_or_no_unit_reference() {
  if constexpr (has_unit<T>) {
    return T::unit;
  } else {
    return no_unit_reference{};
  }
}

/// @brief Basic structure for representing screen coordinates.
template <typename TX, typename TY> struct xy_pair : common_aliases_base<TX, TY> {
  using x_t = TX;
  using y_t = TY;
  TX x{};
  TY y{};
  
  constexpr xy_pair(std::convertible_to<TX> auto&& x_in, std::convertible_to<TY> auto&& y_in)
  : x(std::forward<decltype(x_in)>(x_in)), y(std::forward<decltype(y_in)>(y_in)) {}
  constexpr xy_pair() = default;
  template <std::convertible_to<TX> XIn, std::convertible_to<TY> YIn>
  requires(!std::convertible_to<XIn, TY> && std::convertible_to<YIn, TX>)
  constexpr xy_pair(YIn&& y_in, XIn&& x_in)
   : x(std::forward<XIn>(x_in)), y(std::forward<YIn>(y_in)) {}
};

template <mp_units::Unit auto R, typename Rep>
using basic_coordinate =
    xy_pair<mp_units::quantity_point<
                mp_units::isq::width[R],
                default_point_origin(mp_units::isq::width[R]), Rep>,
            mp_units::quantity_point<
                mp_units::isq::height[R],
                default_point_origin(mp_units::isq::height[R]), Rep>>;

template <mp_units::Unit auto R, typename Rep>
using basic_coordinate_delta = xy_pair<mp_units::quantity<mp_units::isq::width[R], Rep>, mp_units::quantity<mp_units::isq::height[R], Rep>>;

template <typename X1, typename Y1, std::equality_comparable_with<X1> X2,
          std::equality_comparable_with<Y1> Y2>
constexpr bool operator==(xy_pair<X1, Y1> const &l, xy_pair<X2, Y2> const &r) {
  return (l.x == r.x) && (l.y == r.y);
}

template <typename... Ts>
concept all_multipliable = requires(Ts&&... ts) {
	(ts * ...);
};
template <typename T>
concept squarable = all_multipliable<T, T>;
template <typename... Ts>
concept all_summable = requires(Ts&&... ts) {
	(ts + ...);
};
template <typename Num, typename Den>
concept dividable_with = requires(Num n, Den d) {
	n / d;
};

template <typename... Ts>
requires all_multipliable<Ts...>
using product_result_t = decltype((std::declval<Ts>() * ...));
template <typename T>
requires all_multipliable<T, T>
using square_result_t = product_result_t<T, T>;
template <typename Num, typename Den>
requires dividable_with<Num, Den>
using divide_result_t = decltype(std::declval<Num>() / std::declval<Den>());

template <typename... Ts>
concept all_sum_squarable = (squarable<Ts> &&...) && 
all_summable<square_result_t<Ts>...>;

template <typename... Ts>
requires all_summable<Ts...>
using sum_result_t = decltype((std::declval<Ts>() + ...));
template <typename... Ts>
using sum_square_result_t = sum_result_t<square_result_t<Ts>...>;

template <typename X, typename Y>
requires(all_sum_squarable<X, Y>)
constexpr sum_square_result_t<X, Y> distance_square(xy_pair<X, Y> const& v) {
  return call::x_of(v) * call::x_of(v) + call::y_of(v) * call::y_of(v);
}
template <typename X, typename Y>
constexpr decltype(sqrt(std::declval<sum_square_result_t<X, Y>>())) length(xy_pair<X, Y> const& v) {
  using std::sqrt;
  return sqrt(distance_square(v));
}

template <typename X, typename Y, typename Den>
requires (dividable_with<X, Den> && dividable_with<Y, Den>)
constexpr xy_pair<divide_result_t<X, Den>, divide_result_t<Y, Den>> operator/(xy_pair<X, Y> const& xy, Den const& den) {
	return {xy.x  / den, xy.y / den};
}

template <typename X1, typename Y1, std::totally_ordered_with<X1> X2,
          std::equality_comparable_with<Y1> Y2>
constexpr bool operator==(xy_pair<X1, Y1> const &l, xy_pair<X2, Y2> const &r) {
  auto xcmp = l.x <=> r.x;
  if (xcmp == 0) {
    return l.y <=> r.y;
  } else {
    return xcmp;
  }
}

template <typename X1, typename Y1, typename X2, typename Y2>
requires(bp::subtractable_with<X1, X2> && bp::subtractable_with<Y1, Y2>)
constexpr xy_pair<bp::subtract_result_t<X1 const&, X2 const&>, bp::subtract_result_t<Y1 const&, Y2 const&>>
operator-(xy_pair<X1, Y1> const& lhs, xy_pair<X2, Y2> const& rhs) {
	return {lhs.x - rhs.x, lhs.y - rhs.y};
}

template <typename TX, typename TY>
xy_pair(TX &&, TY &&)
    -> xy_pair<std::remove_cvref_t<TX>, std::remove_cvref_t<TY>>;

/// @brief Retrieves the x-coordinate from a basic pixel coordinate.
template <typename TX, typename TY>
constexpr TX const &x_of(xy_pair<TX, TY> const &c) {
  return c.x;
}

/// @brief Retrieves the y-coordinate from a basic pixel coordinate.
template <typename TX, typename TY>
constexpr TY const &y_of(xy_pair<TX, TY> const &c) {
  return c.y;
}

/// @brief Returns a reference to the x-coordinate of a basic pixel coordinate.
template <typename TX, typename TY> constexpr TX &x_of(xy_pair<TX, TY> &c) {
  return c.x;
}

/// @brief Returns a reference to the y-coordinate of a default pixel
/// coordinate.
template <typename TX, typename TY> constexpr TY &y_of(xy_pair<TX, TY> &c) {
  return c.y;
}

ASP_EXPORT_END
#if 0
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

/// @brief Concept for readable position.
template <typename T>
concept has_position = requires(bp::as_forward<T> t) {
  { call::position(*t) } -> two_dimensional_coordinate;
};

/// @brief Concept to check if a type is mutable by TFrom and is a valid pixel
/// coordinate value.
template <typename T, typename TFrom>
concept mutable_pixel_coord_value =
    bp::is_mutable_by<T, TFrom>;

/// Maps a coordinate to a new coordinate through function f.
/// \tparam T Resulting type.
/// \tparam U
/// \tparam F
/// \param u
/// \param f
/// \return mapped coordinate of type T.
template <two_dimensional_coordinate T, two_dimensional_coordinate U, typename F>
constexpr T map_coord(U const &u, F &&f) {
  return {f(call::x_of(u)), f(call::y_of(u))};
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

/// @brief Adds two coordinate vectors through elementwise addition.
/// @tparam Ret Specifies the return type. Defaults to `void`, which resolves
///             to the common type of the two input types unless explicitly
///             provided.
/// @tparam P1 The type of the first input coordinate vector.
/// @tparam P2 The type of the second input coordinate vector.
/// @tparam RetType The resolved return type, either explicitly provided by
/// `Ret`
///                 or deduced as the common type of P1 and P2.
/// @param p1 The first coordinate vector.
/// @param p2 The second coordinate vector.
/// @return The result of adding the two coordinate vectors elementwise.
///         If both input vectors support operator+, it uses that operator;
///         otherwise, individual components are summed.
/// @note The function ensures compatibility with vectors that define an `x_of`
///       and `y_of` accessor instead of operator+.
template <typename Ret = void, two_dimensional_coordinate P1, two_dimensional_coordinate P2,
          two_dimensional_coordinate RetType = typename std::conditional_t<
              std::is_same_v<void, Ret>, std::common_type<P1, P2>,
              std::type_identity<Ret>>::type>
constexpr RetType add(P1 const &p1, P2 const &p2) {
  if constexpr (requires() {
                  { p1 + p2 } -> std::convertible_to<RetType>;
                }) {
    return p1 + p2;
  } else {
    auto x = call::x_of(p1) + call::x_of(p2).quantity_from_zero();
    auto y = call::y_of(p1) + call::y_of(p2).quantity_from_zero();
    return RetType(x, y);
  }
}

/// @brief Subtracts two coordinate vectors through elementwise addition.
/// @tparam Ret Specifies the return type. Defaults to `void`, which resolves
///             to the common type of the two input types unless explicitly
///             provided.
/// @tparam P1 The type of the first input coordinate vector.
/// @tparam P2 The type of the second input coordinate vector.
/// @tparam RetType The resolved return type, either explicitly provided by
/// `Ret`
///                 or deduced as the common type of P1 and P2.
/// @param p1 The first coordinate vector.
/// @param p2 The second coordinate vector.
/// @return The result of adding the two coordinate vectors elementwise.
///         If both input vectors support operator-, it uses that operator;
///         otherwise, individual components are summed.
/// @note The function ensures compatibility with vectors that define an `x_of`
///       and `y_of` accessor instead of operator+.
template <typename Ret = void, two_dimensional_coordinate P1, two_dimensional_coordinate P2,
          two_dimensional_coordinate RetType = typename std::conditional_t<
              std::is_same_v<void, Ret>, std::common_type<P1, P2>,
              std::type_identity<Ret>>::type>
constexpr RetType sub(P1 const &p1, P2 const &p2) {
  if constexpr (requires() {
                  { p1 - p2 } -> std::convertible_to<RetType>;
                }) {
    return p1 - p2;
  } else {
    auto x = call::x_of(p1) - call::x_of(p2);
    auto y = call::y_of(p1) - call::y_of(p2);
    return RetType(x, y);
  }
}

/// @brief Divides the components of a coordinate vector by a scalar.
/// @tparam P The type of the input coordinate vector.
/// @tparam Div The type of the divisor, which must be compatible with the
/// division operation.
/// @param p The coordinate vector to be divided.
/// @param d The scalar divisor.
/// @return A coordinate vector resulting from dividing each component of `p` by
/// `d`.
///         If the vector type supports operator/, it uses that operator;
///         otherwise, the division is performed component-wise using `x_of` and
///         `y_of`.
/// @note The function ensures compatibility with coordinate types that may not
/// natively
///       support operator/ by falling back to elementwise division.
template <two_dimensional_coordinate P, typename Div>
  requires(
      requires(P p, Div d) { p / d; } ||
      requires(P p, Div d) {
        call::x_of(p) / d;
        call::y_of(p) / d;
      })
constexpr P divide(P const &p, Div d) {
  if constexpr (requires() { p / d; }) {
    return p / d;
  } else {
    auto x = call::x_of(p) / d;
    auto y = call::y_of(p) / d;
    return P(x, y);
  }
}

/// @brief Multiplies the components of a vector by a scalar.
/// @tparam P The type of input vector
/// @tparam F The type of the factor.
/// @param p The vector to be multiplied.
/// @param f The scalar factor.
/// @return Vector with all elements multiplied with f.
template <two_dimensional_coordinate P, typename F>
  requires(
      requires(P p, F f) { p * f; } ||
      requires(P p, F f) {
        call::x_of(p) * f;
        call::y_of(p) * f;
      })
constexpr P multiply(P const &p, F const &f) {
  if constexpr (requires() { p * f; }) {
    return p * f;
  } else {
    auto x = call::x_of(p) * f;
    auto y = call::y_of(p) * f;
    return P(x, y);
  }
}

/// @brief Generate a new point right between two other points. All points must
/// share the same unit.
/// @tparam Ret Specifies the return type. Defaults to `void`, which resolves
///             to the common type of the two input types unless explicitly
///             provided.
/// @tparam P1 The type of the first input coordinate vector.
/// @tparam P2 The type of the second input coordinate vector.
/// @tparam RetType The resolved return type, either explicitly provided by
/// `Ret`
///                 or deduced as the common type of P1 and P2.
/// @param p1 The first coordinate vector.
/// @param p2 The second coordinate vector.
/// @return A coordinate vector pointing to the center of the input vectors.
template <typename Ret = void, two_dimensional_coordinate P1, two_dimensional_coordinate P2,
          two_dimensional_coordinate RetType = typename std::conditional_t<
              std::is_same_v<void, Ret>, std::common_type<P1, P2>,
              std::type_identity<Ret>>::type>
constexpr RetType center_between(P1 const &p1, P2 const &p2) {
  //return divide(p1 + sub(p1, p2), 2);
  return p1 + (p1 - p2) / 2;
}

template <typename T>
basic_size_wh(T &&, T &&) -> basic_size_wh<std::remove_cvref_t<T>>;

using default_size_wh = basic_size_wh<int>;

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
struct pixel_size_tag {};
struct point_size_tag {};

template <typename T, typename V = int>
concept pixelpoint_scale = requires(T const &t, V const &v) {
  { v * t } -> std::convertible_to<std::remove_cvref_t<V>>;
  { v / t } -> std::convertible_to<std::remove_cvref_t<V>>;
};

template <typename T, pixelpoint_scale<T> S>
constexpr auto convert(pixel_size_tag, point_size_tag, T &&in, S &&scaler) {
  auto res = in / scaler;
  using in_t = std::remove_cvref_t<T>;
  if constexpr (std::is_integral_v<in_t> &&
                !std::is_integral_v<decltype(res)>) {
    return static_cast<in_t>(std::lround(res));
  } else {
    return res;
  }
}
template <typename T, pixelpoint_scale<T> S>
constexpr auto convert(point_size_tag, pixel_size_tag, T &&in, S &&scaler) {
  auto res = in * scaler;
  using in_t = std::remove_cvref_t<T>;
  if constexpr (std::is_integral_v<in_t> &&
                !std::is_integral_v<decltype(res)>) {
    return static_cast<in_t>(std::lround(res));
  } else {
    return res;
  }
}
template <bp::empty_type Tag, typename In, pixelpoint_scale<In> Scaler>
constexpr auto convert(Tag, Tag, In &&in, Scaler const &) {
  return in;
}

template <typename T, typename To, typename ValT = int, typename Scaler = int>
concept can_convert_tag =
    bp::empty_type<T> && bp::empty_type<To> &&
    requires(T t, To to, ValT &&v, Scaler const &s) { convert(t, to, v, s); };
template <typename T, typename To, typename ValT = int, typename Scaler = int>
concept bidirection_convert_tag = can_convert_tag<T, To, ValT, Scaler> &&
                                  can_convert_tag<To, T, ValT, Scaler>;

template <typename T, typename ValT = int, typename Scaler = int>
concept pixelpoint_tag =
    bidirection_convert_tag<T, pixel_size_tag, ValT, Scaler> &&
    bidirection_convert_tag<T, point_size_tag, ValT, Scaler>;

template <pixelpoint_tag From, pixelpoint_tag To, pixelpoint_scale<int> S>
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
template <typename T, typename Tag>
concept size_tagged_with = size_tagged<T> && std::is_same_v<Tag, tag_t_of<T>>;

template <typename T>
concept scalar = std::is_integral_v<T> || std::is_floating_point_v<T>;


template <typename T>
concept is_pixel_sized =
    size_tagged<T> && std::is_same_v<tag_t_of<T>, pixel_size_tag>;
template <typename T>
concept is_point_sized =
    size_tagged<T> && std::is_same_v<tag_t_of<T>, point_size_tag>;

template <typename T>
concept pixel_rect = bounding_box<T> && is_pixel_sized<T>;
template <typename T>
concept point_rect = bounding_box<T> && is_point_sized<T>;
template <typename T>
concept pixel_or_point_rect_basic = pixel_rect<T> || point_rect<T>;
template <typename T>
concept pixel_or_point_rect = pixel_or_point_rect_basic<T> || requires(T &&t) {
  { t.convert() } -> pixel_or_point_rect_basic;
};

template <typename T>
concept pixel_size_wh = size_wh<T> && is_pixel_sized<T>;
template <typename T>
concept point_size_wh = size_wh<T> && is_point_sized<T>;
template <typename T>
concept pixel_or_point_size_wh_basic = pixel_size_wh<T> || point_size_wh<T>;
template <typename T>
concept pixel_or_point_size_wh =
    pixel_or_point_size_wh_basic<T> || requires(T &&t) {
      { t.convert() } -> pixel_or_point_size_wh_basic;
    };

template <pixelpoint_tag SizeTag, typename T> class pixelpoint_unit {
  using this_t = pixelpoint_unit;
  T value_{};

  template <typename U, typename C = std::identity>
  static constexpr T conv_value(U &&v, C &&converter = {}) {
    if constexpr (bounding_box<T>) {
      return map_box<T>(v, converter);
    } else if constexpr (pixel_coord<T>) {
      return map_coord<T>(v, converter);
    } else {
      static_assert(std::is_integral_v<T> || std::is_floating_point_v<T>);
      return converter(v);
    }
  }

  template <pixelpoint_tag ST2, typename U, pixelpoint_scale S>
  static constexpr T conv_t(pixelpoint_unit<ST2, U> const &v, S const &scaler) {
    if constexpr (std::is_same_v<ST2, SizeTag> &&
                  std::constructible_from<T, U>) {
      return T(v.value());
    } else {
      auto conv = pixelpoint_converter<ST2, SizeTag, S const &>(scaler);
      return conv_value(v.value(), conv);
    }
  }

public:
  using value_type = T;
  constexpr T &value() noexcept { return value_; }
  constexpr T const &value() const noexcept { return value_; }

  constexpr pixelpoint_unit<SizeTag, std::remove_cvref_t<T>>
  remove_ref() const {
    return {SizeTag{}, value()};
  }

  constexpr pixelpoint_unit() noexcept(
      std::is_nothrow_default_constructible_v<T>) = default;
  constexpr pixelpoint_unit(pixelpoint_unit const &) noexcept(
      std::is_nothrow_copy_constructible_v<T>) = default;
  constexpr pixelpoint_unit &operator=(pixelpoint_unit const &) noexcept(
      std::is_nothrow_copy_assignable_v<T>) = default;
  constexpr pixelpoint_unit(pixelpoint_unit &&) noexcept(
      std::is_nothrow_move_constructible_v<T>) = default;
  constexpr pixelpoint_unit &operator=(pixelpoint_unit &&) noexcept(
      std::is_nothrow_move_assignable_v<T>) = default;

  template <typename ST2, typename T2, pixelpoint_scale S>
    requires(same_geometry_as<T, T2> || (scalar<T> && scalar<T2>))
  constexpr pixelpoint_unit(pixelpoint_unit<ST2, T2> const &v, S const &s)
      : value_(conv_t(v, s)) {}

  template <typename T2, typename... Ts>
    requires(std::constructible_from<T, T2, Ts...>)
  constexpr explicit(!std::convertible_to<T2, T> && sizeof...(Ts) == 0)
      pixelpoint_unit(pixelpoint_unit<SizeTag, T2> const &v,
                      pixelpoint_unit<SizeTag, Ts> const &...vs)
      : value_(v.value(), vs.value()...) {}

  template <typename T2 = T, typename... Ts>
    requires(std::constructible_from<T, T2, Ts...>)
  constexpr explicit(sizeof...(Ts) == 0) pixelpoint_unit(T2 &&v, Ts &&...args)
      : value_(std::forward<T2>(v), std::forward<Ts>(args)...) {}

  template <typename T2>
    requires(!std::constructible_from<T, T2> && same_unit_geometry_as<T2, T>)
  constexpr explicit pixelpoint_unit(T2 &&v) : value_(conv_value(v)) {}

  template <typename T2 = T>
    requires(std::constructible_from<T, T2>)
  constexpr pixelpoint_unit(SizeTag, T2 &&v) : value_(std::forward<T2>(v)) {}

  static constexpr decltype(auto) x_of(bp::cvref_type<T> auto &&v,
                                       auto &&...args)
    requires(requires() { call::x_of(v, args...); })
  {}

#define ASP_FWD_GETSET_(X)                                                     \
  template <bp::cvref_type<this_t> U,                                          \
            typename OpRes = decltype(call::X(std::declval<U &&>().value())),  \
            typename OpResClean = std::remove_cvref_t<OpRes>>                  \
    requires(!std::is_void_v<OpRes> &&                                         \
             std::constructible_from<OpResClean, OpRes>)                       \
  static constexpr pixelpoint_unit<SizeTag, OpResClean> X(U &&u) {             \
    return {SizeTag{}, call::X(std::forward<U>(u).value())};                   \
  }                                                                            \
  template <bp::cvref_type<this_t> U, size_tagged_with<SizeTag> Arg>           \
    requires(requires(bp::as_forward<U> u, bp::as_forward<Arg> a) {            \
      call::X((*u).value(), (*a).value());                                     \
    })                                                                         \
  static constexpr void X(U &&u, Arg &&a) {                                    \
    call::X(std::forward<U>(u).value(), std::forward<Arg>(a).value());         \
  }
  ASP_FWD_GETSET_(x_of)
  ASP_FWD_GETSET_(y_of)
  ASP_FWD_GETSET_(r_x)
  ASP_FWD_GETSET_(l_x)
  ASP_FWD_GETSET_(t_y)
  ASP_FWD_GETSET_(b_y)
  ASP_FWD_GETSET_(width)
  ASP_FWD_GETSET_(height)
  ASP_FWD_GETSET_(top_left)
  ASP_FWD_GETSET_(bottom_right)

  using tag_t = SizeTag;
#undef ASP_FWD_GETSET_

  constexpr pixelpoint_unit &operator++()
    requires(bp::value_incrementable<T>)
  {
    ++value();
    return *this;
  }
  constexpr pixelpoint_unit &operator--()
    requires(bp::value_decrementable<T>)
  {
    --value();
    return *this;
  }
  constexpr pixelpoint_unit operator++(int)
    requires(bp::value_incrementable<T> && std::is_copy_constructible_v<T>)
  {
    auto r = *this;
    ++value();
    return r;
  }
  constexpr pixelpoint_unit operator--(int)
    requires(bp::value_decrementable<T> && std::is_copy_constructible_v<T>)
  {
    auto r = *this;
    --value();
    return r;
  }
};

template <pixelpoint_tag Tag, typename OldTag, typename T, pixelpoint_scale S>
// requires(!std::is_reference_v<T>)
constexpr pixelpoint_unit<Tag, T>
convert_pixelpoint(pixelpoint_unit<OldTag, T> const &t, S &&s) {
  static_assert(!std::is_reference_v<T>);
  return {t, std::forward<S>(s)};
}

template <pixelpoint_tag Tag, size_tagged T>
using convert_pixelpoint_t =
    decltype(convert_pixelpoint<Tag>(std::declval<T>(), int{}));

template <pixelpoint_tag Tag, typename T>
  requires(!size_tagged<T> || size_tagged_with<T, Tag>)
constexpr auto _wrap_with_pixelpoint(T &&t) {
  if constexpr (size_tagged<T>) {
    return std::forward<T>(t);
  } else {
    return pixelpoint_unit(Tag{}, std::forward<T>(t));
  }
}
} // namespace asp
namespace std {
template <typename ST, typename T, typename U>
  requires(requires() { typename common_type<T, U>::type; })
struct common_type<::asp::pixelpoint_unit<ST, T>,
                   ::asp::pixelpoint_unit<ST, U>> {
  using type = ::asp::pixelpoint_unit<ST, common_type_t<T, U>>;
};
template <typename ST, typename T, typename U>
  requires(!::asp::bp::is_low_high_placeholder<U> &&
           requires() { typename common_type<T, U>::type; })
struct common_type<::asp::pixelpoint_unit<ST, T>, U> {
  using type = ::asp::pixelpoint_unit<ST, common_type_t<T, U>>;
};
template <typename ST, typename T, typename U>
  requires(!::asp::bp::is_low_high_placeholder<U> &&
           requires() { typename common_type<T, U>::type; })
struct common_type<U, ::asp::pixelpoint_unit<ST, T>> {
  using type = ::asp::pixelpoint_unit<ST, common_type_t<T, U>>;
};
template <typename ST, typename T>
  requires(numeric_limits<T>::is_specialized)
struct numeric_limits<::asp::pixelpoint_unit<ST, T>>
    : private numeric_limits<T> {
  using _base = numeric_limits<T>;
  using _this_t = ::asp::pixelpoint_unit<ST, T>;
  static constexpr bool is_specialized = true;
  static constexpr _this_t min() { return {ST{}, numeric_limits<T>::min()}; }
  static constexpr _this_t lowest() { return {ST{}, numeric_limits<T>::min()}; }
  static constexpr _this_t max() { return {ST{}, numeric_limits<T>::max()}; }
  static constexpr _this_t epsilon() { return {ST{}, _base::epsilon()}; }
  static constexpr _this_t round_error() {
    return {ST{}, _base::round_error()};
  }
  static constexpr _this_t infinity() { return {ST{}, _base::infinity()}; }
  static constexpr _this_t quiet_NaN() { return {ST{}, _base::quiet_NaN()}; }
  static constexpr _this_t signaling_NaN() {
    return {ST{}, _base::signaling_NaN()};
  }
  static constexpr _this_t denorm_min() { return {ST{}, _base::denorm_min()}; }
  using _base::digits;
  using _base::digits10;
  using _base::has_denorm;
  using _base::has_denorm_loss;
  using _base::has_infinity;
  using _base::has_quiet_NaN;
  using _base::has_signaling_NaN;
  using _base::is_bounded;
  using _base::is_exact;
  using _base::is_iec559;
  using _base::is_integer;
  using _base::is_modulo;
  using _base::is_signed;
  using _base::max_exponent;
  using _base::max_exponent10;
  using _base::min_exponent;
  using _base::min_exponent10;
  using _base::round_style;
  using _base::tinyness_before;
  using _base::traps;
};
template <typename St, typename T>
struct iterator_traits<::asp::pixelpoint_unit<St, T>> {
  using difference_type = T;
};
} // namespace std
namespace asp {

template <typename SizeTag, typename T>
struct extend_api<pixelpoint_unit<SizeTag, T>> {
  using this_t = pixelpoint_unit<SizeTag, T>;

#define ASP_BOX_INIT_FWD_(X)                                                   \
  template <typename... Ts,                                                    \
            typename TXY =                                                     \
                std::common_type_t<decltype(_wrap_with_pixelpoint<SizeTag>(    \
                    std::declval<Ts>()))...>>                                  \
    requires(impl::has_bbox_init<T, typename TXY::value_type> ||               \
             impl::has_bbox_init<extend_api_t<T>, typename TXY::value_type>)   \
  static constexpr auto from_##X(Ts &&...args)                                 \
      ->pixelpoint_unit<SizeTag,                                               \
                        std::remove_cvref_t<decltype(box_from_##X<T>(          \
                            TXY(std::forward<Ts>(args)).value()...))>> {       \
    return {SizeTag{},                                                         \
            box_from_##X<T, typename TXY::value_type>(                         \
                _wrap_with_pixelpoint<SizeTag>(std::forward<Ts>(args))         \
                    .value()...)};                                             \
  }

  ASP_BOX_INIT_FWD_(xyxy)
  ASP_BOX_INIT_FWD_(xywh)
  ASP_BOX_INIT_FWD_(tlbr)
#undef ASP_BOX_INIT_FWD_
};

template <typename SizeTag, typename T, typename U>
  requires(bp::weakly_comparable_with<T const &, U const &>)
constexpr bool operator==(pixelpoint_unit<SizeTag, T> const &l,
                          pixelpoint_unit<SizeTag, U> const &r) noexcept {
  return l.value() == r.value();
}
template <typename SizeTag, typename T, typename U>
  requires(bp::weakly_totally_ordered_with<T, U>)
constexpr auto operator<=>(pixelpoint_unit<SizeTag, T> const &l,
                           pixelpoint_unit<SizeTag, U> const &r) noexcept {
  return l.value() <=> r.value();
}

template <typename SizeTag, typename T, typename U,
          typename R = decltype(std::declval<T const &>() +
                                std::declval<U const &>())>
constexpr pixelpoint_unit<SizeTag, R>
operator+(pixelpoint_unit<SizeTag, T> const &l,
          pixelpoint_unit<SizeTag, U> const &r) {
  return pixelpoint_unit<SizeTag, R>(l.value() + r.value());
}
template <typename SizeTag, typename T, typename U>
  requires(requires(T &t, U const &r) { t += r; })
constexpr pixelpoint_unit<SizeTag, T> &
operator+=(pixelpoint_unit<SizeTag, T> &l,
           pixelpoint_unit<SizeTag, U> const &r) {
  l.value() += r.value();
  return l;
}

template <typename SizeTag, typename T, typename U,
          typename R = decltype(std::declval<T const &>() -
                                std::declval<U const &>())>
constexpr pixelpoint_unit<SizeTag, R>
operator-(pixelpoint_unit<SizeTag, T> const &l,
          pixelpoint_unit<SizeTag, U> const &r) {
  return pixelpoint_unit<SizeTag, R>(l.value() - r.value());
}
template <typename SizeTag, typename T, typename U>
  requires(requires(T &t, U const &r) { t -= r; })
constexpr pixelpoint_unit<SizeTag, T> &
operator-=(pixelpoint_unit<SizeTag, T> &l,
           pixelpoint_unit<SizeTag, U> const &r) {
  l.value() -= r.value();
  return l;
}

template <typename SizeTag, typename T, typename U,
          typename R = decltype(std::declval<T const &>() *
                                std::declval<U const &>())>
constexpr pixelpoint_unit<SizeTag, R>
operator*(pixelpoint_unit<SizeTag, T> const &l, U const &r) {
  return pixelpoint_unit<SizeTag, R>(l.value() * r);
}
template <typename SizeTag, typename T, typename U,
          typename R = decltype(std::declval<U const &>() *
                                std::declval<T const &>())>
constexpr pixelpoint_unit<SizeTag, R>
operator*(U const &l, pixelpoint_unit<SizeTag, T> const &r) {
  return pixelpoint_unit<SizeTag, R>(l * r.value());
}
template <typename SizeTag, typename T, typename U,
          typename R = decltype(std::declval<T const &>() /
                                std::declval<U const &>())>
constexpr pixelpoint_unit<SizeTag, R>
operator/(pixelpoint_unit<SizeTag, T> const &l, U const &r) {
  return pixelpoint_unit<SizeTag, R>(l.value() / r);
}
template <typename ST, typename T>
  requires(requires(T const &t) {
    { -t } -> bp::not_void;
  })
constexpr auto operator-(pixelpoint_unit<ST, T> const &o) -> pixelpoint_unit<
    ST, std::remove_cvref_t<decltype(-std::declval<T const &>())>> {
  return {ST{}, -o.value()};
}
template <typename ST, typename T>
  requires(requires(T const &t) {
    { +t } -> bp::not_void;
  })
constexpr auto operator+(pixelpoint_unit<ST, T> const &o) -> pixelpoint_unit<
    ST, std::remove_cvref_t<decltype(-std::declval<T const &>())>> {
  return {ST{}, -o.value()};
}

template <typename SizeTag, typename T>
pixelpoint_unit(SizeTag, T &&)
    -> pixelpoint_unit<SizeTag, std::remove_cvref_t<T>>;

template <typename T> using pixel_unit_t = pixelpoint_unit<pixel_size_tag, T>;
template <typename T> using point_unit_t = pixelpoint_unit<point_size_tag, T>;

template <typename T, typename U = std::unwrap_ref_decay_t<T>>
constexpr pixel_unit_t<U> pixel_unit(T &&in) {
  return pixel_unit_t<U>(std::forward<T>(in));
}
template <typename T, typename U = std::unwrap_ref_decay_t<T>>
constexpr point_unit_t<U> point_unit(T &&in) {
  return point_unit_t<U>(std::forward<T>(in));
}

template <typename T> constexpr auto strip_unit(T const &t) {
  if constexpr(std::integral<T> || std::floating_point<T>) {
    return t;
  }else if constexpr (size_tagged<T>) {
    return t.value();
  } else if constexpr (mp_units::Quantity<T>) {
    return t.numerical_value_in(T::unit);
  } else if constexpr (mp_units::QuantityPoint<T>) {
    return t.quantity_from_zero().numerical_value_in(T::unit);
  } else {
    static_assert(std::integral<T>);
  }
}

/// @brief Concept for readable position declared in point units.
template <typename T>
concept has_point_position = requires(bp::as_forward<T> t) {
  { call::position(*t) } -> point_coordinate;
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
template <bounding_box TB, pixel_coord TC>
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
  ASP_ASSERT(b != nullptr);
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
  ASP_ASSERT(b != nullptr);
  auto res = box_from_xyxy<bp::dereferenced_t<T>>(call::l_x(*b), y,
                                                  call::r_x(*b), call::b_y(*b));
  call::b_y(*b, y);
  return res;
}

/// Cut away a part v from the left side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_left(T bptr, TV v) {
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::width(b));
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
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::height(b));
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
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::width(b));
  call::width(b, call::width(b) - v);
  return box_from_xywh<bp::dereferenced_t<T>>(call::r_x(b), call::t_y(b), v,
                                              call::height(b));
}

/// Cut away a part v from the lower side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_below(T bptr, TV v) {
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::height(b));
  call::height(b, call::height(b) - v);
  return box_from_xywh<bp::dereferenced_t<T>>(call::l_x(b), call::b_y(b),
                                              call::width(b), v);
}

/// Creates a larger box that includes the smaller boxes. Does not check for
/// empty boxes. Use box_add when the boxes may be empty or non-valid.
template <typename TRes = void, bounding_box T1, same_unit_geometry_as<T1> T2>
constexpr auto box_union(T1 const &b1, T2 const &b2) {
  ASP_ASSERT(valid_box(b1));
  ASP_ASSERT(valid_box(b2));
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
  requires(same_unit_as<T1, T2> &&
           (same_unit_as<TRes, T1> || std::is_void_v<TRes>))
constexpr auto box_intersection(T1 const &b1, T2 const &b2) {
  ASP_ASSERT(valid_box(b1));
  ASP_ASSERT(valid_box(b2));
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
constexpr auto nudge_left(pixel_coord auto c,
                          same_unit_as<decltype(c)> auto &&val) {
  call::x_of(c, call::x_of(c) - val);
  return c;
}
/// Creates a new pixel_coord that has moved right by val.
constexpr auto nudge_right(pixel_coord auto c,
                           same_unit_as<decltype(c)> auto &&val) {
  return nudge_left(c, -val);
}
/// Creates a new pixel_coord that has moved up by val.
constexpr auto nudge_up(pixel_coord auto c,
                        same_unit_as<decltype(c)> auto &&val) {
  call::y_of(c, call::y_of(c) - val);
  return c;
}
/// Creates a new pixel_coord that has moved down by val.
constexpr auto nudge_down(pixel_coord auto c,
                          same_unit_as<decltype(c)> auto &&val) {
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

/// True if all corners of inner is inside the outer box.
template <bounding_box TB1, same_unit_geometry_as<TB1> TB2>
  requires(same_unit_as<TB1, TB2>)
constexpr bool box_includes_box(TB1 const &outer, TB2 const &inner) {
  if constexpr (size_tagged<TB1>) {
    return hit_box(outer, TB2::top_left(inner), inside_closed_range) &&
           hit_box(outer, TB2::bottom_right(inner), inside_closed_range);
  } else {
    return hit_box(outer, call::top_left(inner), inside_closed_range) &&
           hit_box(outer, call::bottom_right(inner), inside_closed_range);
  }
}

/// Returns true if the box is empty.
constexpr bool empty_box(bounding_box auto const &b) {
  return call::width(b) == bp::default_init_valued ||
         call::height(b) == bp::default_init_valued;
}

/// Calculates the length-squared of a coordinate vector
/// \tparam T Type of coordinate
/// \param p Coordinate vector
/// \return Scalar that is p^T p
template <pixel_coord T> constexpr auto length_sqr(T const &p) {
  if constexpr (size_tagged<T>) {
    // We currently don't support 'unit to the power of ...'.
    return length_sqr(p.value());
  } else {
    return square_value(call::x_of(p)) + square_value(call::y_of(p));
  }
}

/// Calculates the length-squared of a coordinate vector
/// \tparam T Type of coordinate
/// \param p Coordinate vector
/// \return Scalar that is p^T p
template <pixel_coord T> constexpr auto length(T const &p) {
  if constexpr (size_tagged<T>) {
    return T{std::sqrt(length_sqr(p.value()))};
  } else {
    return std::sqrt(length_sqr(p));
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

template <pixelpoint_tag SizeTag, typename T, typename Scale = double>
class autoconverting_pixelpoint_unit {
  Scale s_;
  T v_;

public:
  constexpr autoconverting_pixelpoint_unit(pixelpoint_unit<SizeTag, T> const &v,
                                           pixelpoint_scale auto &&s)
      : s_(s), v_(v.value()) {}
  template <typename T2, pixelpoint_scale S>
    requires(std::constructible_from<T, T2>)
  constexpr autoconverting_pixelpoint_unit(SizeTag, T2 const &v, S &&s)
      : s_(s), v_(v) {}

  constexpr Scale const &pixel_scale() const noexcept { return s_; }

  template <typename ST2 = SizeTag, typename T2 = T>
  constexpr pixelpoint_unit<ST2, T2> convert() const {
    if constexpr (std::is_same_v<SizeTag, ST2>) {
      return pixelpoint_unit<ST2, T2>(v_);
    } else {
      // return convert_to(pixelpoint_unit<SizeTag, T>(v_));
      return pixelpoint_unit<ST2, T2>(pixelpoint_unit<SizeTag, T>(v_), s_);
      // return pixelpoint_unit<ST2, T2>(v_, s_);
    }
  }

  template <typename ST2, typename T2>
  constexpr explicit(false) operator pixelpoint_unit<ST2, T2>() const {
    return convert<ST2, T2>();
  }
};

template <typename ST, typename T, pixelpoint_scale S>
autoconverting_pixelpoint_unit(pixelpoint_unit<ST, T> const &, S &&s)
    -> autoconverting_pixelpoint_unit<ST, T, std::remove_cvref_t<S>>;
template <pixelpoint_tag ST, typename T, pixelpoint_scale S>
autoconverting_pixelpoint_unit(ST, T &&, S &&)
    -> autoconverting_pixelpoint_unit<ST, std::remove_cvref_t<T>,
                                      std::remove_cvref_t<S>>;

template <pixelpoint_tag Tag, typename TagOrg, typename T, typename S>
constexpr pixelpoint_unit<Tag, T>
convert_to(autoconverting_pixelpoint_unit<TagOrg, T, S> const &pu) {
  return pu;
}
#endif
} // namespace asp

#endif


#ifndef COMPONENT_GUI_ASP_CALL_HPP
#define COMPONENT_GUI_ASP_CALL_HPP

#include "asp/import/stl.hpp"
#include "asp/export/asp_export.hpp"

#if __has_include("dooc/named_args_tuple.hpp")
#include "dooc/named_args_tuple.hpp"
#define ASP_HAS_NAMED_ARGS 1
#endif

#include "asp/std-backport/concepts.hpp"
#include "asp/std-backport/functional.hpp"
#include "asp/std-backport/tuple.hpp"
#include "asp/std-backport/utility.hpp"

#define ASP_CALL_CONCEPT(NAME) \
[[maybe_unused]] inline void NAME() {}                                       \
template <typename T, typename... Ts>                                        \
concept member_##NAME =                                                      \
requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
(*t).NAME(*args...);                                                   \
};                                                                       \
template <typename T, typename... Ts>                                        \
concept static_##NAME =                                                      \
requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
std::remove_cvref_t<T>::NAME(*t, *args...);                            \
};                                                                       \
template <typename T, typename... Ts>                                        \
concept extend_##NAME =                                                      \
requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
extend_api_t<T>::NAME(*t, *args...);                                   \
};                                                                       \
template <typename T, typename... Ts>                                        \
concept free_##NAME =                                                        \
requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
NAME(*t, *args...);                                                    \
};                                                                       \
template <typename T, typename... Ts>                                        \
concept has_##NAME = member_##NAME<T, Ts...> || static_##NAME<T, Ts...> ||   \
extend_##NAME<T, Ts...> || free_##NAME<T, Ts...>;       \

#define ASP_SIMPLE_CALL(NAME)                                                 \
  ASP_CALL_CONCEPT(NAME)\
  struct _do_##NAME {                                                       \
    template <typename... Ts, has_##NAME<Ts...> T>                             \
    static constexpr decltype(auto) call(T &&torg, Ts &&...args) {             \
      auto t = bp::as_forward<T>(torg);                                        \
      if constexpr (member_##NAME<T, Ts...>) {                                 \
        return (*t).NAME(std::forward<Ts>(args)...);                           \
      } else if constexpr (static_##NAME<T, Ts...>) {                          \
        return std::remove_cvref_t<T>::NAME(*t, std::forward<Ts>(args)...);    \
      } else if constexpr (extend_##NAME<T, Ts...>) {                          \
        return extend_api_t<T>::NAME(*t, std::forward<Ts>(args)...);           \
      } else {                                                                 \
        static_assert(free_##NAME<T, Ts...>);                                  \
        return NAME(*t, std::forward<Ts>(args)...);                            \
      }                                                                        \
    }                                                                          \
    template <typename... Ts, has_##NAME<Ts...> T>                             \
    constexpr decltype(auto) operator()(T &&t, Ts &&...args) const {           \
      return call(std::forward<T>(t), std::forward<Ts>(args)...);              \
    }                                                                          \
  };
#define ASP_CALL_CONCEPT_GETSET(NAME)                                          \
  ASP_CALL_CONCEPT(set_##NAME)                                                       \
  ASP_SIMPLE_CALL(get_##NAME) \
  template <typename T, typename TVal>                                         \
  concept has_get_##NAME##_assignable = has_assignable_get<T, _do_get_##NAME, TVal>;   \
  template <typename T, typename Val> \
  concept can_mutate_##NAME = has_set_##NAME<T, Val> || has_get_##NAME##_assignable<T, Val>;\
  struct _do_set_##NAME {                                                      \
    template <typename TObj, typename TVal>                                    \
      requires(can_mutate_##NAME<TObj, TVal>)                            \
    static constexpr void call(TObj &&o, TVal &&v) {                 \
      auto of = bp::as_forward<TObj>(o);                                       \
      auto vf = bp::as_forward<TVal>(v);                                       \
      if constexpr (member_set_##NAME<TObj, TVal>) {                                  \
        (*of).set_##NAME(*vf);                                     \
      } else if constexpr (static_set_##NAME<TObj, TVal>) {                                  \
        std::remove_cvref_t<TObj>::set_##NAME(*of, *vf);                                     \
      } else if constexpr (extend_set_##NAME<TObj, TVal>) {                                  \
        extend_api_t<TObj>::set_##NAME(*of, *vf);                                     \
      } else if constexpr (free_set_##NAME<TObj, TVal>) {                                  \
        set_##NAME(*of, *vf);                                     \
      } else {                                                                 \
        static_assert(has_get_##NAME##_assignable<TObj, TVal>);                    \
        _do_get_##NAME::call(*of) = *vf;                                    \
      }                                                                        \
    }                                                                          \
    template <typename... Ts>                                                  \
      requires(requires(Ts &&...ts) { call(std::forward<Ts>(ts)...); })        \
    constexpr decltype(auto) operator()(Ts &&...ts) const {                    \
      return call(std::forward<Ts>(ts)...);                                    \
    }                                                                          \
  };

#define ASP_CALL_BBOX_MEMBER(NAME, CONCEPT, MUTCONCEPT)                        \
  static constexpr decltype(auto) _fallback(auto const &b);                    \
  static constexpr decltype(auto) _fallback_mut(auto &&b, auto &&v);           \
  template <typename T>                                                        \
    requires(CONCEPT<T> || has_##NAME<T>)                                      \
  static constexpr decltype(auto) call(T &&b) {                                \
    using fwd_t = decltype(b);                                                 \
    auto bf = bp::as_forward<fwd_t>(std::forward<fwd_t>(b));                   \
    if constexpr (has_##NAME<fwd_t>) {                                         \
      return _do_##NAME::call(*bf);                                            \
    } else {                                                                   \
      return _fallback(bf);                                                    \
    }                                                                          \
  }                                                                            \
  template <typename TVal, typename T>                                         \
    requires(MUTCONCEPT<T, TVal> || has_##NAME<T, TVal>)                       \
  static constexpr decltype(auto) call(T &&b, TVal &&v) {                      \
    auto bf = bp::as_forward<T>(std::forward<T>(b));                           \
    auto vf = bp::as_forward<TVal>(std::forward<TVal>(v));                     \
    if constexpr (has_##NAME<T, TVal>) {                                       \
      return _do_##NAME::call(*bf, *vf);                                       \
    } else if constexpr (has_assignable_get<T, _do_##NAME, TVal>) {            \
      return _do_##NAME::call(*bf) = *vf;                                      \
    } else {                                                                   \
      return _fallback_mut(bf, vf);                                            \
    }                                                                          \
  }                                                                            \
  constexpr decltype(auto) operator()(auto &&b, auto &&...vs) const            \
    requires(requires() {                                                      \
      call(std::forward<decltype(b)>(b), std::forward<decltype(vs)>(vs)...);   \
    })                                                                         \
  {                                                                            \
    return call(std::forward<decltype(b)>(b),                                  \
                std::forward<decltype(vs)>(vs)...);                            \
  }

/// Primary ASP namespace
namespace asp {
ASP_EXPORT_BEGIN
template <typename T>
concept is_scalar = std::totally_ordered<T> && bp::has_arithmetic_operators<T>;

/// @brief Class used as a placeholder when no special functionality is
/// expected.
struct empty_placeholder_t {};

/// @brief Used in arguments when types needs to be deduced as part of a
/// function signature
///
/// @see sub_accessor_t
template <typename...> struct arguments_marker_t {};

template <typename... Ts>
inline constexpr arguments_marker_t<Ts...> arguments_marker;

/// @brief Customisation point. Users may specialise this class to provide an
/// API for new types.
///
/// @see asp/sdl.hpp for an example.
/// @tparam T type to provide interface for.
template <typename T> struct extend_api {};

/// @brief tyoe alias that removes extents not relevant for the extend_api
template <typename T> using extend_api_t = extend_api<std::remove_cvref_t<T>>;

/// @brief Conversion policy for transforming XY coordinates to Width and
/// Height.
///
/// The `xy2wh_t` structure provides static methods for converting XY
/// coordinates to width and height. This is achieved through a fetch and assign
/// operation using the difference or sum of two coordinates.
struct xy2wh_t {
  /// Calculates width/height from two X/Y coordinates.
  static constexpr auto on_fetch(auto const &x1, auto const &x2) {
    return x2 - x1;
  }
  /// Assigns ending X/Y based on starting X/Y and width/height.
  static constexpr auto on_assign(auto const &x1, auto const &w) {
    return x1 + w;
  }
};

/// @brief Conversion policy for transforming Width and Height to XY
/// coordinates.
///
/// The `wh2xy_t` structure provides static methods for converting width and
/// height to XY coordinates. This is achieved by adding width to an X
/// coordinate or assigning it as a difference.
struct wh2xy_t {
  /// Calculates the ending X/Y coordinate based on the starting X/Y and
  /// width/height.
  static constexpr auto on_fetch(auto const &x1, auto const &w) {
    return x1 + w;
  }
  /// Assigns the width/height by subtracting ending X/Y from the starting X/Y
  /// coordinate.
  static constexpr auto on_assign(auto const &x1, auto const &x2) {
    return x2 - x1;
  }
};

inline constexpr xy2wh_t xy2wh; ///< Instance of the XY to WH conversion policy.
inline constexpr wh2xy_t wh2xy; ///< Instance of the WH to XY conversion policy.

/// @brief Concept enforcing a valid conversion policy for coordinate
/// transformations.
template <typename T>
concept xxyy_xwyh_conv_policy =
    std::is_same_v<T, xy2wh_t> || std::is_same_v<T, wh2xy_t>;

/// @brief Provides a conversion interface between two coordinate systems.
///
/// This template class uses a conversion policy to convert between two
/// coordinate types.
///
/// @tparam TStartCoord The start coordinate type.
/// @tparam TTo The output coordinate type.
/// @tparam TPol The conversion policy type (either `xy2wh_t` or `wh2xy_t`).
template <typename TStartCoord, typename TTo, xxyy_xwyh_conv_policy TPol>
class xxyy_xwyh_conv {
  TStartCoord const *tl_{};
  TTo *br_{};
  static_assert(!std::is_reference_v<TStartCoord>);
  static_assert(!std::is_reference_v<TTo>);

public:
  using value_type = std::common_type_t<TStartCoord, TTo>;

  /// @brief Constructor that initializes the XY and WH values.
  ///
  /// @param tl Reference to the top left coordinate value.
  /// @param br Reference to the mutable bottom right value.
  /// @param policy.
  constexpr xxyy_xwyh_conv(TStartCoord const &tl, TTo &br, TPol = {})
      : tl_(&tl), br_(&br) {}

  /// @brief Assigns a value after converting based on the conversion policy.
  ///
  /// @param v The value to assign.
  /// @return Reference to the updated *this object.
  constexpr xxyy_xwyh_conv &operator=(value_type const &v) {
    *br_ = TPol::on_assign(*tl_, v);
    return *this;
  }

  /// @brief Implicit conversion operator to the common value type.
  ///
  /// @return The converted value from XY/WH policy.
  constexpr explicit(false) operator value_type() const {
    return TPol::on_fetch(*tl_, *br_);
  }
  constexpr value_type value() const { return TPol::on_fetch(*tl_, *br_); }
};

/// @brief Placeholder in various setters
///
/// Used to indicate that a certain value (like an x/y or width/height) are to
/// remain the same during an operation.
class keep_current_t {

public:
  constexpr decltype(auto) operator()(auto &&op, auto &&obj) const {
    return std::forward<decltype(op)>(op)(std::forward<decltype(obj)>(obj));
  }
};

inline constexpr keep_current_t
    keep_current; ///< Instance of the keep_current_t placeholder.

template <typename T, typename TOp, typename TVal>
concept has_assignable_get =
    requires(bp::as_forward<T> t, bp::as_forward<TOp> op) {
      { (*op)(*t) } -> bp::is_mutable_by<TVal>;
    };
template <typename> constexpr bool is_placeholder_impl = false;
template <> constexpr bool is_placeholder_impl<keep_current_t> = true;
template <typename T>
constexpr bool is_placeholder_v = is_placeholder_impl<std::remove_cvref_t<T>>;
ASP_EXPORT_END

/// @brief Generalized object API invocation namespace.
///
/// Namespace to make robust calls to various methods associated with a type
/// (much like std::ranges::begin). It checks for member functions and static
/// member functions, free functions and static extend_api_t<T> functions taking
/// the object as the first parameter. It may also even resolve to other
/// functions if it gives the same result (for example fill may fall back to
/// calling draw_pixels if no appropriate fill-function exists for the object)
namespace call {

/// @cond
namespace impl {
ASP_SIMPLE_CALL(apply_to)
ASP_SIMPLE_CALL(for_each)
ASP_SIMPLE_CALL(build)
ASP_SIMPLE_CALL(size_of)
ASP_SIMPLE_CALL(pixel_scale)
ASP_SIMPLE_CALL(draw_pixels)
ASP_SIMPLE_CALL(draw_alpha)
ASP_SIMPLE_CALL(fill)
ASP_SIMPLE_CALL(advance_x)
ASP_SIMPLE_CALL(advance_y)
ASP_SIMPLE_CALL(pixel_area)
ASP_SIMPLE_CALL(point_area)
ASP_SIMPLE_CALL(full_height)
ASP_SIMPLE_CALL(ascender)
ASP_SIMPLE_CALL(base_to_top)
ASP_SIMPLE_CALL(position)
ASP_SIMPLE_CALL(move_event)
ASP_SIMPLE_CALL(time_stamp)
ASP_SIMPLE_CALL(delta_x)
ASP_SIMPLE_CALL(delta_y)
ASP_SIMPLE_CALL(scale_x)
ASP_SIMPLE_CALL(scale_y)
ASP_SIMPLE_CALL(zoom_factor)
ASP_SIMPLE_CALL(raw_key)
ASP_SIMPLE_CALL(finger_index)
ASP_SIMPLE_CALL(handle)
ASP_SIMPLE_CALL(intrinsic_min_size)
ASP_SIMPLE_CALL(set_state)
ASP_SIMPLE_CALL(state)
ASP_SIMPLE_CALL(mouse_button)
ASP_SIMPLE_CALL(bitmap_top)
ASP_SIMPLE_CALL(event_type)
ASP_SIMPLE_CALL(render)
ASP_SIMPLE_CALL(set_displayed)
ASP_SIMPLE_CALL(set_text)
ASP_SIMPLE_CALL(render_text)
ASP_SIMPLE_CALL(area)
ASP_SIMPLE_CALL(widget_id)
ASP_SIMPLE_CALL(glyph)
ASP_SIMPLE_CALL(text_colour)
ASP_SIMPLE_CALL(colour)
ASP_SIMPLE_CALL(execute)
ASP_SIMPLE_CALL(initial_render_cache)
ASP_SIMPLE_CALL(executing_renderer)
ASP_SIMPLE_CALL(to_pixel)
ASP_SIMPLE_CALL(find_sub)
ASP_SIMPLE_CALL(find_sub_id)
ASP_SIMPLE_CALL(find_sub_at_location)
ASP_SIMPLE_CALL(sub_accessor)

template <typename T>
concept is_tuple_like_hack =
    bp::has_tuple_size<T> && (std::tuple_size_v<std::remove_cvref_t<T>> == 0 ||
                              requires(T &&t) { std::get<0>(t); });

struct do_apply_to {
  template <typename T, typename TCB>
    requires(has_apply_to<T, TCB> ||
             (is_tuple_like_hack<T> &&
              requires(bp::as_forward<T> t, bp::as_forward<TCB> cb) {
                std::apply(*cb, *t);
              }))
  static constexpr decltype(auto) call(T &&t, TCB &&cb) {
    auto tf = bp::as_forward<T>(t);
    auto cf = bp::as_forward<TCB>(cb);
    if constexpr (has_apply_to<T, TCB>) {
      return _do_apply_to{}(*tf, *cf);
    } else {
      return std::apply(*cf, *tf);
    }
  }
  template <typename... Ts>
    requires(requires(bp::as_forward<Ts>... vs) { call(*vs...); })
  constexpr decltype(auto) operator()(Ts &&...vs) const {
    return call(std::forward<Ts>(vs)...);
  }
};

#if defined(ASP_HAS_NAMED_ARGS)
template <typename> struct named_tuple_tags {};
template <typename... Ts, dooc::template_string... tags>
struct named_tuple_tags<dooc::named_tuple<dooc::named_arg_t<tags, Ts>...>> {
  using type = dooc::template_string_list_t<tags...>;
};
template <typename T>
using named_tuple_tags_t =
    typename named_tuple_tags<std::remove_cvref_t<T>>::type;
#endif

struct do_for_each {
  template <typename T, typename TCB>
    requires(has_for_each<T, TCB> || std::ranges::input_range<T> ||
             requires(bp::as_forward<T> t) { do_apply_to{}(*t, bp::no_op); }
#if defined(ASP_HAS_NAMED_ARGS)
             || dooc::named_tuple_like<T>
#endif
             )
  constexpr void operator()(T &&t, TCB &&cb) const {
    auto tf = bp::as_forward<T>(t);
    auto cb_gen = [cbf = bp::as_forward<TCB>(cb)]<typename U, typename INT>(
                      U &&in, INT &&index) {
      bp::invoke_arg1_or_arg1_2(*cbf, std::forward<U>(in),
                                std::forward<INT>(index));
    };
    if constexpr (has_for_each<T, decltype(cb_gen)>) {
      _do_for_each{}(*tf, cb_gen);
    } else if constexpr (std::ranges::input_range<T>) {
      std::ranges::for_each(
          *tf, [&cb_gen, i = std::ptrdiff_t{}]<typename U>(U &&v) mutable {
            cb_gen(std::forward<U>(v), i);
            ++i;
          });
#ifdef ASP_HAS_NAMED_ARGS
    } else if constexpr (dooc::named_tuple_like<T>) {
      dooc::tuple_for_each(
          [&cb_gen]<typename U>(auto const &tag, U &&v) {
            using tags = named_tuple_tags_t<T>;
            cb_gen(std::forward<U>(v), dooc::find_string(tag, tags{}));
          },
          std::forward<T>(t));
#endif
    } else {
      do_apply_to{}(*tf, [&cb_gen](auto &&...vals) {
        bp::run_for_each(cb_gen, std::forward<decltype(vals)>(vals)...);
      });
    }
  }
};

struct do_build {
  template <typename T, typename... Args>
    requires(has_build<T, Args...> || has_build<T>)
  constexpr decltype(auto) operator()(T &&t, Args &&...args) const {
    auto tf = bp::as_forward<T>(t);
    if constexpr (has_build<T, Args...>) {
      return _do_build::call(*tf, std::forward<Args>(args)...);
    } else {
      return _do_build::call(*tf);
    }
  }
};

ASP_CALL_CONCEPT_GETSET(x)
ASP_CALL_CONCEPT_GETSET(y)
ASP_CALL_CONCEPT_GETSET(red)
ASP_CALL_CONCEPT_GETSET(green)
ASP_CALL_CONCEPT_GETSET(blue)
ASP_CALL_CONCEPT_GETSET(alpha)

ASP_SIMPLE_CALL(l_x)
ASP_SIMPLE_CALL(t_y)
ASP_SIMPLE_CALL(r_x)
ASP_SIMPLE_CALL(b_y)
ASP_SIMPLE_CALL(width)
ASP_SIMPLE_CALL(height)
ASP_SIMPLE_CALL(top_left)
ASP_SIMPLE_CALL(bottom_right)

template <typename T, typename TVal>
concept pixel_coord_mut =
   can_mutate_x<T, TVal> && can_mutate_y<T, TVal>;
template <typename T, typename Ts>
concept has_mut_top_left = has_top_left<T> && requires(bp::as_forward<T> t) {
  { _do_top_left::call(*t) } -> pixel_coord_mut<Ts>;
};
template <typename T, typename Ts>
concept has_mut_bot_right = has_top_left<T> && requires(bp::as_forward<T> t) {
  { _do_bottom_right::call(*t) } -> pixel_coord_mut<Ts>;
};
template <typename T, typename... Ts>
concept has_any_tlx = has_l_x<T, Ts...> || has_top_left<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_tlx =
    has_any_tlx<T, TVal> || has_assignable_get<T, _do_l_x, TVal> ||
    has_mut_top_left<T, TVal>;

template <typename T, typename... Ts>
concept has_any_tly = has_t_y<T, Ts...> || has_top_left<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_tly =
    has_any_tly<T, TVal> || has_assignable_get<T, _do_t_y, TVal> ||
    has_mut_top_left<T, TVal>;

template <typename T, typename... Ts>
concept has_any_brx =
    has_r_x<T, Ts...> || (has_width<T, Ts...> && has_l_x<T>) ||
    has_bottom_right<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_brx =
    has_any_brx<T, TVal> || has_assignable_get<T, _do_r_x, TVal> ||
    has_assignable_get<T, _do_width, TVal> || has_mut_bot_right<T, TVal>;

template <typename T, typename... Ts>
concept has_any_bry =
    has_b_y<T, Ts...> || (has_height<T, Ts...> && has_t_y<T>) ||
    has_bottom_right<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_bry =
    has_any_bry<T, TVal> || has_assignable_get<T, _do_b_y, TVal> ||
    has_assignable_get<T, _do_height, TVal> || has_mut_bot_right<T, TVal>;

template <typename T, typename... Ts>
concept has_any_tl = has_any_tlx<T, Ts...> && has_any_tly<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_tl = has_any_mut_tlx<T, TVal> && has_any_mut_tly<T, TVal>;
template <typename T, typename... Ts>
concept has_any_br = has_any_brx<T, Ts...> && has_any_bry<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_br = has_any_mut_brx<T, TVal> && has_any_mut_bry<T, TVal>;

struct l_x_t {
  ASP_CALL_BBOX_MEMBER(l_x, has_any_tlx, has_any_mut_tlx)
};
struct t_y_t {
  ASP_CALL_BBOX_MEMBER(t_y, has_any_tly, has_any_mut_tly)
};
struct r_x_t {
  ASP_CALL_BBOX_MEMBER(r_x, has_any_brx, has_any_mut_brx)
};
struct b_y_t {
  ASP_CALL_BBOX_MEMBER(b_y, has_any_bry, has_any_mut_bry)
};
struct width_t {
  ASP_CALL_BBOX_MEMBER(width, has_any_brx, has_any_mut_brx)
};
struct height_t {
  ASP_CALL_BBOX_MEMBER(height, has_any_bry, has_any_mut_bry)
};
struct top_left_t {
  ASP_CALL_BBOX_MEMBER(top_left, has_any_tl, has_any_mut_tl)
};
struct bottom_right_t {
  ASP_CALL_BBOX_MEMBER(bottom_right, has_any_br, has_any_mut_br)
};

constexpr decltype(auto) l_x_t::_fallback(auto const &b) {
  return _do_get_x::call(_do_top_left::call(*b));
}
constexpr decltype(auto) l_x_t::_fallback_mut(auto &&b, auto &&v) {
  return _do_set_x::call(_do_top_left::call(*b), *v);
}
constexpr decltype(auto) t_y_t::_fallback(auto const &b) {
  return _do_get_y::call(_do_top_left::call(*b));
}
constexpr decltype(auto) t_y_t::_fallback_mut(auto &&b, auto &&v) {
  return _do_set_y::call(_do_top_left::call(*b), *v);
}
constexpr decltype(auto) r_x_t::_fallback(auto const &b) {
  if constexpr (requires() { _do_bottom_right::call(*b); }) {
    return _do_get_x::call(_do_bottom_right::call(*b));
  } else {
    return l_x_t::call(*b) + _do_width::call(*b);
  }
}
constexpr decltype(auto) r_x_t::_fallback_mut(auto &&b, auto &&v) {
  if constexpr (requires() {
                  {
                    _do_bottom_right::call(*b)
                  } -> pixel_coord_mut<decltype(*v)>;
                }) {
    return _do_set_x::call(_do_bottom_right::call(*b), *v);
  } else {
    return width_t::call(*b, *v - l_x_t{}(*b));
  }
}
constexpr decltype(auto) b_y_t::_fallback(auto const &b) {
  if constexpr (requires() { _do_bottom_right::call(*b); }) {
    return _do_get_y::call(_do_bottom_right::call(*b));
  } else {
    return t_y_t{}(*b) + _do_height::call(*b);
  }
}
constexpr decltype(auto) b_y_t::_fallback_mut(auto &&b, auto &&v) {
  if constexpr (requires() {
                  {
                    _do_bottom_right::call(*b)
                  } -> pixel_coord_mut<decltype(*v)>;
                }) {
    return _do_set_y::call(_do_bottom_right::call(*b), *v);
  } else {
    return height_t::call(*b, *v - t_y_t{}(*b));
  }
}
constexpr decltype(auto) width_t::_fallback(auto const &b) {
  return r_x_t{}(*b) - l_x_t{}(*b);
}
constexpr decltype(auto) width_t::_fallback_mut(auto &&b, auto &&v) {
  if constexpr (requires() {
                  _do_bottom_right::call(*b);
                  _do_get_x::call(_do_top_left::call(*b));
                  _do_set_x::call(_do_bottom_right::call(*b),
                                     _do_get_x::call(_do_top_left::call(*b)) +
                                         *v);
                }) {
    return _do_set_x::call(_do_bottom_right::call(*b),
                              _do_get_x::call(_do_top_left::call(*b)) + *v);
  } else {
    return r_x_t::call(*b, _do_l_x::call(*b) + *v);
  }
}
constexpr decltype(auto) height_t::_fallback(auto const &b) {
  return b_y_t{}(*b) - t_y_t{}(*b);
}
constexpr decltype(auto) height_t::_fallback_mut(auto &&b, auto &&v) {
  if constexpr (requires() {
                  _do_bottom_right::call(*b);
                  _do_get_y::call(_do_top_left::call(*b));
                  _do_set_y::call(_do_bottom_right::call(*b),
                                     _do_get_y::call(_do_top_left::call(*b)) +
                                         *v);
                }) {
    return _do_set_y::call(_do_bottom_right::call(*b),
                              _do_get_y::call(_do_top_left::call(*b)) + *v);
  } else {
    return b_y_t::call(*b, _do_t_y::call(*b) + *v);
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

  constexpr decltype(auto) set_x() const { return TX{}(val_); }
  constexpr decltype(auto) set_x(auto &&v)
    requires(std::invocable<TX, T &, decltype(v)>)
  {
    return TX{}(val_, std::forward<decltype(v)>(v));
  }
  constexpr decltype(auto) set_y() const { return TY{}(val_); }
  constexpr decltype(auto) set_y(auto &&v)
    requires(std::invocable<TY, T &, decltype(v)>)
  {
    return TY{}(val_, std::forward<decltype(v)>(v));
  }
};

template <typename T, typename TX, typename TY>
tlbr_wh_conv(T &&, TX, TY) -> tlbr_wh_conv<T, TX, TY>;

template <typename TX, typename TY> class fallback_coordinate {
  TX x_;
  TY y_;

public:
  constexpr fallback_coordinate(TX x, TY y) : x_(x), y_(y) {}
  constexpr TX const &get_x() const noexcept { return x_; }
  constexpr TX const &get_y() const noexcept { return y_; }
};

constexpr decltype(auto) top_left_t::_fallback(auto const &b) {
  return fallback_coordinate(l_x_t::call(*b), t_y_t::call(*b));
}
constexpr decltype(auto) top_left_t::_fallback_mut(auto &&b, auto &&v) {
  l_x_t::call(*b, _do_get_x::call(*v));
  t_y_t::call(*b, _do_get_y::call(*v));
}
constexpr decltype(auto) bottom_right_t::_fallback(auto const &b) {
  return fallback_coordinate(r_x_t::call(*b), b_y_t::call(*b));
}
constexpr decltype(auto) bottom_right_t::_fallback_mut(auto &&b, auto &&v) {
  r_x_t::call(*b, _do_get_x::call(*v));
  b_y_t::call(*b, _do_get_y::call(*v));
}

struct do_move_event {
  template <typename T, typename Pos>
    requires(
        requires(bp::as_forward<T> t, Pos const &p) {
          _do_move_event::call(*t, p);
        } ||
        requires(bp::as_forward<T> t) {
          { _do_position::call(*t) } -> pixel_coord_mut<Pos>;
        })
  static constexpr std::convertible_to<std::remove_cvref_t<T>> auto
  call(T &&t, Pos const &p) {
    auto tf = bp::as_forward<T>(t);
    if constexpr (requires() { _do_move_event::call(*tf, p); }) {
      return _do_move_event::call(*tf, p);
    } else {
      auto res = *tf;
      decltype(auto) tp = _do_position::call(res);
      _do_set_x::call(tp, _do_get_x::call(p));
      _do_set_y::call(tp, _do_get_y::call(p));
      return res;
    }
  }
  constexpr auto operator()(auto &&v, auto const &p) const
    requires(requires() { call(std::forward<decltype(v)>(v), p); })
  {
    return call(std::forward<decltype(v)>(v), p);
  }
};

} // namespace impl
/// @endcond

ASP_EXPORT_BEGIN
/// Calls apply_to or fallbacks to std::apply if it seems appropriate. Also
/// dooc::apply may be called if dooc-np is included in the translation unit.
inline constexpr impl::do_apply_to apply_to;
/// Calls for_each or fallbacks to apply_to together with bp::for_each
inline constexpr impl::do_for_each for_each;
inline constexpr impl::do_build build;
inline constexpr impl::_do_set_red red;
inline constexpr impl::_do_set_green green;
inline constexpr impl::_do_set_blue blue;
inline constexpr impl::_do_set_alpha alpha;
inline constexpr impl::_do_set_x set_x;
inline constexpr impl::_do_get_x get_x;
inline constexpr impl::_do_set_y set_y;
inline constexpr impl::_do_get_y get_y;
inline constexpr impl::_do_size_of size_of;

/// Function like object that calls pixel_scale for a type. The function takes
/// an object that implements pixel_scale.
///
/// \return a value that either can be
/// multiplied with a point value to get a pixel value or used as a divisor on a
/// pixel value to get a point value.
inline constexpr impl::_do_pixel_scale pixel_scale;
inline constexpr impl::_do_draw_pixels draw_pixels;
inline constexpr impl::_do_draw_alpha draw_alpha;
inline constexpr impl::_do_advance_x advance_x;
inline constexpr impl::_do_advance_y advance_y;
inline constexpr impl::_do_full_height full_height;
inline constexpr impl::_do_ascender ascender;
inline constexpr impl::_do_base_to_top base_to_top;
inline constexpr impl::_do_bitmap_top bitmap_top;
inline constexpr impl::_do_set_state set_state;
inline constexpr impl::_do_state state;
inline constexpr impl::_do_handle handle;
inline constexpr impl::_do_intrinsic_min_size intrinsic_min_size;
inline constexpr impl::_do_position position;
inline constexpr impl::do_move_event move_event;
using position_t = decltype(position);
inline constexpr impl::_do_time_stamp time_stamp;
using time_stamp_t = decltype(time_stamp);
inline constexpr impl::_do_delta_x delta_x;
inline constexpr impl::_do_delta_y delta_y;
inline constexpr impl::_do_scale_x scale_x;
inline constexpr impl::_do_scale_y scale_y;
inline constexpr impl::_do_zoom_factor zoom_factor;
inline constexpr impl::_do_raw_key raw_key;
inline constexpr impl::_do_finger_index finger_index;

/// Calls fill or fall backs to calling draw_pixels to fill the area in
/// software.
inline constexpr impl::_do_fill fill;
inline constexpr impl::_do_area area;
inline constexpr impl::_do_widget_id widget_id;
inline constexpr impl::_do_render render;
inline constexpr impl::_do_glyph glyph;
inline constexpr impl::_do_set_displayed set_displayed;
inline constexpr impl::_do_text_colour text_colour;
inline constexpr impl::_do_colour colour;
inline constexpr impl::_do_execute execute;
inline constexpr impl::_do_initial_render_cache initial_render_cache;
inline constexpr impl::_do_executing_renderer executing_renderer;
inline constexpr impl::_do_to_pixel to_pixel;

/// Get or set left x (x = 0). May use the other geometric functions to achieve
/// the desired result. Width and right x are undetermined after using this to
/// set the value.
inline constexpr impl::l_x_t l_x;
inline constexpr impl::l_x_t set_left_x;
inline constexpr impl::l_x_t get_left_x;
/// Get or set top y (y = 0). May use the other geometric functions to achieve
/// the desired result. Height and bottom y are undetermined after using this to
/// set the value.
inline constexpr impl::t_y_t t_y;
inline constexpr impl::t_y_t set_top_y;
inline constexpr impl::t_y_t get_top_y;
/// Get or set right x. May use the other geometric functions to achieve the
/// desired result. This should never change the left x.
inline constexpr impl::r_x_t r_x;
inline constexpr impl::r_x_t set_right_x;
inline constexpr impl::r_x_t get_right_x;
/// Get or set bottom y. May use the other geometric functions to achieve the
/// desired result. This should never change the top y.
inline constexpr impl::b_y_t b_y;
inline constexpr impl::b_y_t set_bottom_y;
inline constexpr impl::b_y_t get_bottom_y;
/// Get or set width. May use the other geometric functions to achieve the
/// desired result. This should never change the left x.
inline constexpr impl::width_t width;
inline constexpr impl::width_t set_width;
inline constexpr impl::width_t get_width;
/// Get or set height. May use the other geometric functions to achieve the
/// desired result. This should never change the top y.
inline constexpr impl::height_t height;
inline constexpr impl::height_t set_height;
inline constexpr impl::height_t get_height;
/// Get or set left x (x = 0) and top y (y = 0) as a coordinate type. May use
/// the other geometric functions to achieve the desired result. Right/Bottom xy
/// and width/height are undetermined after using this to set the value.
inline constexpr impl::top_left_t top_left;
/// Get or set right x and bottom y. May use the other geometric functions to
/// achieve the desired result. This should never change the left x nor the top
/// y.
inline constexpr impl::bottom_right_t bottom_right;

/// @brief Determines the result type of calling an instance of type `T` with
/// arguments `Ts&&...`, preserving const, volatile, and reference qualifiers.
/// @tparam T Callable type to be invoked.
/// @tparam Ts Argument types to be passed to `T`.
/// @returns The type resulting from calling `T` with `Ts&&...`, maintaining
/// const, volatile, and reference qualifiers.
template <auto T, typename... Ts>
using call_result_cvref_t = decltype(T(std::declval<Ts &&>()...));

/// @brief Determines the result type of calling an instance of type `T` with
/// arguments `Ts&&...`, with const, volatile, and reference qualifiers removed
/// from the result type.
/// @tparam T Callable type to be invoked.
/// @tparam Ts Argument types to be passed to `T`.
/// @returns The type resulting from calling `T` with `Ts&&...`, with qualifiers
/// removed.
template <auto T, typename... Ts>
using call_result_t = std::remove_cvref_t<call_result_cvref_t<T, Ts...>>;
ASP_EXPORT_END
} // namespace call
} // namespace asp

#undef ASP_PIX_FUNC_IMPL
#undef ASP_EXTRA_PARAMS
#undef ASP_EXTRA_ARGS
#undef ASP_EXTRA_ARGS_COMMA
#undef ASP_SIMPLE_CALL
#undef ASP_CALL_BBOX_MEMBER
#endif

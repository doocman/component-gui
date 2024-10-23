
#ifndef COMPONENT_GUI_CGUI_CALL_HPP
#define COMPONENT_GUI_CGUI_CALL_HPP

#include <concepts>
#include <type_traits>
#include <utility>

#if __has_include("dooc/named_args_tuple.hpp")
#include <dooc/named_args_tuple.hpp>
#define CGUI_HAS_NAMED_ARGS 1
#endif

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/utility.hpp>

#define CGUI_CALL_CONCEPT(NAME)                                                \
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
  struct _do_##NAME {                                                          \
                                                                               \
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
    constexpr decltype(auto) operator()(T && t, Ts &&...args) const {          \
      return call(std::forward<T>(t), std::forward<Ts>(args)...);              \
    }                                                                          \
  };
#define CGUI_CALL_CONCEPT_GETSET(NAME)                                         \
  CGUI_CALL_CONCEPT(NAME)                                                      \
  template <typename T, typename TVal>                                         \
  concept has_##NAME##_assignable = has_assignable_get<T, _do_##NAME, TVal>;   \
  struct _do_set_##NAME {                                                      \
    template <typename TObj, typename TVal>                                    \
      requires(has_##NAME<TObj &&, TVal &&> ||                                 \
               has_##NAME##_assignable<TObj, TVal>)                            \
    static constexpr decltype(auto) call(TObj &&o, TVal &&v) {                 \
      auto of = bp::as_forward<TObj>(o);                                       \
      auto vf = bp::as_forward<TVal>(v);                                       \
      if constexpr (has_##NAME<TObj, TVal>) {                                  \
        return _do_##NAME::call(*of, *vf);                                     \
      } else {                                                                 \
        static_assert(has_##NAME##_assignable<TObj, TVal>);                    \
        return _do_##NAME::call(*of) = *vf;                                    \
      }                                                                        \
    }                                                                          \
    static constexpr decltype(auto) call(has_##NAME auto &&t) {                \
      return _do_##NAME::call(std::forward<decltype(t)>(t));                   \
    }                                                                          \
    template <typename... Ts>                                                  \
      requires(requires(Ts &&...ts) { call(std::forward<Ts>(ts)...); })        \
    constexpr decltype(auto) operator()(Ts &&...ts) const {                    \
      return call(std::forward<Ts>(ts)...);                                    \
    }                                                                          \
  };

#define CGUI_CALL_BBOX_MEMBER(NAME, CONCEPT, MUTCONCEPT)                       \
  static constexpr decltype(auto) _fallback(auto const &b);                    \
  static constexpr decltype(auto) _fallback_mut(auto &&b, auto &&v);           \
  static constexpr decltype(auto) call(CONCEPT auto &&b) {                     \
    using fwd_t = decltype(b);                                                 \
    auto bf = bp::as_forward<fwd_t>(std::forward<fwd_t>(b));                   \
    if constexpr (has_##NAME<fwd_t>) {                                         \
      return _do_##NAME::call(*bf);                                            \
    } else {                                                                   \
      return _fallback(bf);                                                    \
    }                                                                          \
  }                                                                            \
  template <typename TVal, MUTCONCEPT<TVal> T>                                 \
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

/// Primary CGUI namespace
namespace cgui {

/// @brief Used in arguments when types needs to be deduced as part of a function signature
///
/// @see sub_accessor_t
template <typename...>
struct arguments_marker_t {};

template <typename... Ts>
inline constexpr arguments_marker_t<Ts...> arguments_marker;

/// @brief Customisation point. Users may specialise this class to provide an
/// API for new types.
///
/// @see cgui/sdl.hpp for an example.
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
CGUI_CALL_CONCEPT(apply_to);
CGUI_CALL_CONCEPT(for_each);
CGUI_CALL_CONCEPT(build);
CGUI_CALL_CONCEPT(size_of);
CGUI_CALL_CONCEPT(draw_pixels)
CGUI_CALL_CONCEPT(draw_alpha);
CGUI_CALL_CONCEPT(fill);
CGUI_CALL_CONCEPT(advance_x);
CGUI_CALL_CONCEPT(advance_y);
CGUI_CALL_CONCEPT(pixel_area);
CGUI_CALL_CONCEPT(full_height);
CGUI_CALL_CONCEPT(ascender);
CGUI_CALL_CONCEPT(base_to_top);
CGUI_CALL_CONCEPT(position);
CGUI_CALL_CONCEPT(handle);
CGUI_CALL_CONCEPT(set_state);
CGUI_CALL_CONCEPT(state);
CGUI_CALL_CONCEPT(mouse_button);
CGUI_CALL_CONCEPT(bitmap_top);
CGUI_CALL_CONCEPT(event_type)
CGUI_CALL_CONCEPT(render)
CGUI_CALL_CONCEPT(set_displayed)
CGUI_CALL_CONCEPT(set_text)
CGUI_CALL_CONCEPT(render_text)
CGUI_CALL_CONCEPT(area)
CGUI_CALL_CONCEPT(glyph)
CGUI_CALL_CONCEPT(text_colour)
CGUI_CALL_CONCEPT(find_sub)
CGUI_CALL_CONCEPT(find_sub_at_location)
CGUI_CALL_CONCEPT(sub_accessor)

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
  static constexpr decltype(auto) call(T && t, TCB && cb) {
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
  constexpr decltype(auto) operator()(Ts&&... vs) const {
    return call(std::forward<Ts>(vs)...);
  }
};

#if defined(CGUI_HAS_NAMED_ARGS)
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
#if defined(CGUI_HAS_NAMED_ARGS)
             || dooc::named_tuple_like<T>
#endif
             )
  constexpr void operator()(T &&t, TCB &&cb) const {
    auto tf = bp::as_forward<T>(t);
    // auto cbf = bp::as_forward<TCB>(cb);
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
#ifdef CGUI_HAS_NAMED_ARGS
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

CGUI_CALL_CONCEPT_GETSET(x_of)
CGUI_CALL_CONCEPT_GETSET(y_of)
CGUI_CALL_CONCEPT_GETSET(red)
CGUI_CALL_CONCEPT_GETSET(green)
CGUI_CALL_CONCEPT_GETSET(blue)
CGUI_CALL_CONCEPT_GETSET(alpha)

CGUI_CALL_CONCEPT(l_x)
CGUI_CALL_CONCEPT(t_y)
CGUI_CALL_CONCEPT(r_x)
CGUI_CALL_CONCEPT(b_y)
CGUI_CALL_CONCEPT(width)
CGUI_CALL_CONCEPT(height)
CGUI_CALL_CONCEPT(top_left)
CGUI_CALL_CONCEPT(bottom_right)

template <typename T, typename TVal>
concept pixel_coord_mut =
    (has_assignable_get<T, _do_x_of, TVal> || has_x_of<T, TVal>)&&(
        has_assignable_get<T, _do_y_of, TVal> || has_y_of<T, TVal>);
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
    has_r_x<T, Ts...> || has_width<T, Ts...> || has_bottom_right<T, Ts...>;
template <typename T, typename TVal>
concept has_any_mut_brx =
    has_any_brx<T, TVal> || has_assignable_get<T, _do_r_x, TVal> ||
    has_assignable_get<T, _do_width, TVal> || has_mut_bot_right<T, TVal>;

template <typename T, typename... Ts>
concept has_any_bry =
    has_b_y<T, Ts...> || has_height<T, Ts...> || has_bottom_right<T, Ts...>;
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
  CGUI_CALL_BBOX_MEMBER(l_x, has_any_tlx, has_any_mut_tlx)
};
struct t_y_t {
  CGUI_CALL_BBOX_MEMBER(t_y, has_any_tly, has_any_mut_tly)
};
struct r_x_t {
  CGUI_CALL_BBOX_MEMBER(r_x, has_any_brx, has_any_mut_brx)
};
struct b_y_t {
  CGUI_CALL_BBOX_MEMBER(b_y, has_any_bry, has_any_mut_bry)
};
struct width_t {
  CGUI_CALL_BBOX_MEMBER(width, has_any_brx, has_any_mut_brx)
};
struct height_t {
  CGUI_CALL_BBOX_MEMBER(height, has_any_bry, has_any_mut_bry)
};
struct top_left_t {
  CGUI_CALL_BBOX_MEMBER(top_left, has_any_tl, has_any_mut_tl)
};
struct bottom_right_t {
  CGUI_CALL_BBOX_MEMBER(bottom_right, has_any_br, has_any_mut_br)
};

constexpr decltype(auto) l_x_t::_fallback(auto const &b) {
  return _do_x_of::call(_do_top_left::call(*b));
}
constexpr decltype(auto) l_x_t::_fallback_mut(auto &&b, auto &&v) {
  return _do_set_x_of::call(_do_top_left::call(*b), *v);
}
constexpr decltype(auto) t_y_t::_fallback(auto const &b) {
  return _do_y_of::call(_do_top_left::call(*b));
}
constexpr decltype(auto) t_y_t::_fallback_mut(auto &&b, auto &&v) {
  return _do_set_y_of::call(_do_top_left::call(*b), *v);
}
constexpr decltype(auto) r_x_t::_fallback(auto const &b) {
  if constexpr (requires() { _do_bottom_right::call(*b); }) {
    return _do_x_of::call(_do_bottom_right::call(*b));
  } else {
    return l_x_t{}(*b) + _do_width::call(*b);
  }
}
constexpr decltype(auto) r_x_t::_fallback_mut(auto &&b, auto &&v) {
  if constexpr (requires() {
                  {
                    _do_bottom_right::call(*b)
                  } -> pixel_coord_mut<decltype(*v)>;
                }) {
    return _do_set_x_of::call(_do_bottom_right::call(*b), *v);
  } else {
    return width_t::call(*b, *v - l_x_t{}(*b));
  }
}
constexpr decltype(auto) b_y_t::_fallback(auto const &b) {
  if constexpr (requires() { _do_bottom_right::call(*b); }) {
    return _do_y_of::call(_do_bottom_right::call(*b));
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
    return _do_set_y_of::call(_do_bottom_right::call(*b), *v);
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
                  _do_x_of::call(_do_top_left::call(*b));
                  _do_set_x_of::call(_do_bottom_right::call(*b),
                                     _do_x_of::call(_do_top_left::call(*b)) +
                                         *v);
                }) {
    return _do_set_x_of::call(_do_bottom_right::call(*b),
                              _do_x_of::call(_do_top_left::call(*b)) + *v);
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
                  _do_y_of::call(_do_top_left::call(*b));
                  _do_set_y_of::call(_do_bottom_right::call(*b),
                                     _do_y_of::call(_do_top_left::call(*b)) +
                                         *v);
                }) {
    return _do_set_y_of::call(_do_bottom_right::call(*b),
                              _do_y_of::call(_do_top_left::call(*b)) + *v);
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

  constexpr decltype(auto) x_of() const { return TX{}(val_); }
  constexpr decltype(auto) x_of(auto &&v)
    requires(std::invocable<TX, T &, decltype(v)>)
  {
    return TX{}(val_, std::forward<decltype(v)>(v));
  }
  constexpr decltype(auto) y_of() const { return TY{}(val_); }
  constexpr decltype(auto) y_of(auto &&v)
    requires(std::invocable<TY, T &, decltype(v)>)
  {
    return TY{}(val_, std::forward<decltype(v)>(v));
  }
};

template <typename T, typename TX, typename TY>
tlbr_wh_conv(T &&, TX, TY) -> tlbr_wh_conv<T, TX, TY>;

constexpr decltype(auto) top_left_t::_fallback(auto const &b) {
  return tlbr_wh_conv(*b, l_x_t{}, t_y_t{});
}
constexpr decltype(auto) top_left_t::_fallback_mut(auto &&b, auto &&v) {
  auto val = _fallback(b);
  _do_set_x_of::call(val, _do_x_of::call(*v));
  _do_set_y_of::call(val, _do_y_of::call(*v));
  return val;
}
constexpr decltype(auto) bottom_right_t::_fallback(auto const &b) {
  return tlbr_wh_conv(*b, r_x_t{}, b_y_t{});
}
constexpr decltype(auto) bottom_right_t::_fallback_mut(auto &&b, auto &&v) {
  auto val = _fallback(b);
  _do_set_x_of::call(val, _do_x_of::call(*v));
  _do_set_y_of::call(val, _do_y_of::call(*v));
  return val;
}
} // namespace impl
/// @endcond

/// Calls apply_to or fallbacks to std::apply if it seems appropriate. Also
/// dooc::apply may be called if dooc-np is included in the translation unit.
inline constexpr impl::do_apply_to apply_to;
/// Calls for_each or fallbacks to apply_to together with bp::for_each
inline constexpr impl::do_for_each for_each;
inline constexpr impl::_do_build build;
inline constexpr impl::_do_set_red red;
inline constexpr impl::_do_set_green green;
inline constexpr impl::_do_set_blue blue;
inline constexpr impl::_do_set_alpha alpha;
inline constexpr impl::_do_set_x_of x_of;
inline constexpr impl::_do_set_y_of y_of;
inline constexpr impl::_do_size_of size_of;
inline constexpr impl::_do_draw_pixels draw_pixels;
inline constexpr impl::_do_draw_alpha draw_alpha;
inline constexpr impl::_do_advance_x advance_x;
inline constexpr impl::_do_advance_y advance_y;
inline constexpr impl::_do_pixel_area pixel_area;
inline constexpr impl::_do_full_height full_height;
inline constexpr impl::_do_ascender ascender;
inline constexpr impl::_do_base_to_top base_to_top;
inline constexpr impl::_do_bitmap_top bitmap_top;
inline constexpr impl::_do_set_state set_state;
inline constexpr impl::_do_state state;
inline constexpr impl::_do_handle handle;
inline constexpr impl::_do_position position;

/// Calls fill or fall backs to calling draw_pixels to fill the area in
/// software.
inline constexpr impl::_do_fill fill;
inline constexpr impl::_do_area area;
inline constexpr impl::_do_render render;
inline constexpr impl::_do_glyph glyph;
inline constexpr impl::_do_set_displayed set_displayed;
inline constexpr impl::_do_text_colour text_colour;

/// Get or set left x (x = 0). May use the other geometric functions to achieve
/// the desired result. Width and right x are undetermined after using this to
/// set the value.
inline constexpr impl::l_x_t l_x;
/// Get or set top y (y = 0). May use the other geometric functions to achieve
/// the desired result. Height and bottom y are undetermined after using this to
/// set the value.
inline constexpr impl::t_y_t t_y;
/// Get or set right x. May use the other geometric functions to achieve the
/// desired result. This should never change the left x.
inline constexpr impl::r_x_t r_x;
/// Get or set bottom y. May use the other geometric functions to achieve the
/// desired result. This should never change the top y.
inline constexpr impl::b_y_t b_y;
/// Get or set width. May use the other geometric functions to achieve the
/// desired result. This should never change the left x.
inline constexpr impl::width_t width;
/// Get or set height. May use the other geometric functions to achieve the
/// desired result. This should never change the top y.
inline constexpr impl::height_t height;
/// Get or set left x (x = 0) and top y (y = 0) as a coordinate type. May use
/// the other geometric functions to achieve the desired result. Right/Bottom xy
/// and width/height are undetermined after using this to set the value.
inline constexpr impl::top_left_t top_left;
/// Get or set right x and bottom y. May use the other geometric functions to
/// achieve the desired result. This should never change the left x nor the top
/// y.
inline constexpr impl::bottom_right_t bottom_right;
} // namespace call
} // namespace cgui

#undef CGUI_PIX_FUNC_IMPL
#undef CGUI_EXTRA_PARAMS
#undef CGUI_EXTRA_ARGS
#undef CGUI_EXTRA_ARGS_COMMA
#undef CGUI_CALL_CONCEPT
#undef CGUI_CALL_BBOX_MEMBER
#endif

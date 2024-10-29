
#ifndef COMPONENT_GUI_LIMITS_HPP
#define COMPONENT_GUI_LIMITS_HPP

#include <concepts>
#include <limits>

namespace cgui::bp {
/// @brief Concept to check if a type has numeric limits defined for min and
/// max.
template <typename T>
concept _has_numeric_limits_minmax = requires() {
  std::numeric_limits<T>::min();
  std::numeric_limits<T>::max();
};

/// @brief Struct to obtain the minimum possible value of a numeric type.
struct lowest_possible_t {
  /// @brief Conversion operator to get the minimum value of a type.
  /// @tparam T Type which must satisfy _has_numeric_limits_minmax.
  /// @return The minimum value of type T.
  template <_has_numeric_limits_minmax T>
  constexpr operator T() const noexcept {
    return std::numeric_limits<T>::min();
  }
};

/// @brief Struct to obtain the maximum possible value of a numeric type.
struct highest_possible_t {
  /// @brief Conversion operator to get the maximum value of a type.
  /// @tparam T Type which must satisfy _has_numeric_limits_minmax.
  /// @return The maximum value of type T.
  template <_has_numeric_limits_minmax T>
  constexpr operator T() const noexcept {
    return std::numeric_limits<T>::max();
  }
};

/// Concept to identify the limits placeholders.
/// \tparam T
template <typename T>
concept is_low_high_placeholder =
    cvref_type<T, lowest_possible_t> || cvref_type<T, highest_possible_t>;

/// Checks that T is a placeholder and U has the numeric_limits that the
/// placeholder can convert to.
/// \tparam T
/// \tparam U
template <typename T, typename U>
concept low_high_operand =
    is_low_high_placeholder<T> && _has_numeric_limits_minmax<U>;

/// Equality operator for placeholder (right)
/// \tparam LT
/// \tparam RT
/// \param l
/// \param r
/// \return l == numeric_limits::max()
template <typename LT, low_high_operand<LT> RT>
constexpr bool operator==(LT const &l, RT const &r) {
  return l == static_cast<LT>(r);
}

/// Spaceship operator for placeholder (right)
/// \tparam LT
/// \tparam RT
/// \param l
/// \param r
/// \return l <=> numeric_limits::max()
template <typename LT, low_high_operand<LT> RT>
constexpr bool operator<=>(LT const &l, RT const &r) {
  return l <=> static_cast<LT>(r);
}

/// Equality operator for placeholder (left)
/// \tparam LT
/// \tparam RT
/// \param l
/// \param r
/// \return numeric_limits::max() == r
template <typename RT, low_high_operand<RT> LT>
constexpr bool operator==(LT const &l, RT const &r) {
  return static_cast<RT>(l) == r;
}

/// Spaceship operator for placeholder (left)
/// \tparam LT
/// \tparam RT
/// \param l
/// \param r
/// \return numeric_limits::max() <=> r
template <typename RT, low_high_operand<RT> LT>
constexpr bool operator<=>(LT const &l, RT const &r) {
  return static_cast<RT>(l) <=> r;
}

/// @brief Global constant instance for the minimum possible value.
inline constexpr lowest_possible_t lowest_possible;

/// @brief Global constant instance for the maximum possible value.
inline constexpr highest_possible_t highest_possible;
} // namespace cgui::bp

#endif // COMPONENT_GUI_LIMITS_HPP

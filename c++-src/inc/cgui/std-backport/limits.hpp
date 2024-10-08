
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

/// @brief Global constant instance for the minimum possible value.
inline constexpr lowest_possible_t lowest_possible;

/// @brief Global constant instance for the maximum possible value.
inline constexpr highest_possible_t highest_possible;
} // namespace cgui::bp

#endif // COMPONENT_GUI_LIMITS_HPP

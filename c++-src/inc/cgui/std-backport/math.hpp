
#ifndef COMPONENT_GUI_CGUI_STD_BACKPORT_MATH_HPP
#define COMPONENT_GUI_CGUI_STD_BACKPORT_MATH_HPP

#include <concepts>

namespace cgui::bp {
constexpr auto is_power_of_2(std::integral auto v) {
  return (v != 0) && !(v & (v - 1));
}
} // namespace cgui::bp

#endif

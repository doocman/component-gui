
#ifndef COMPONENT_GUI_ASP_STD_BACKPORT_MATH_HPP
#define COMPONENT_GUI_ASP_STD_BACKPORT_MATH_HPP

#include <asp/import/stl.hpp>

namespace asp::bp {
constexpr auto is_power_of_2(std::integral auto v) {
  return (v != 0) && !(v & (v - 1));
}
} // namespace asp::bp

#endif

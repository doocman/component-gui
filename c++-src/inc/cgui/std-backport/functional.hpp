//
// Created by rvons on 2024-09-21.
//

#ifndef COMPONENT_GUI_FUNCTIONAL_HPP
#define COMPONENT_GUI_FUNCTIONAL_HPP

#include <functional>

namespace cgui::bp {
struct no_op_t {
  constexpr void operator()(auto &&...) const noexcept {}
};
inline constexpr no_op_t no_op;
} // namespace cgui::bp

#endif // COMPONENT_GUI_FUNCTIONAL_HPP

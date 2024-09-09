//
// Created by rvons on 2024-07-09.
//

#ifndef CGUI_CGUI_WARNINGS_HPP
#define CGUI_CGUI_WARNINGS_HPP

namespace cgui {
constexpr void unused(auto &&...) {}
#ifndef NDEBUG
#define CGUI_DEBUG_ONLY(...) __VA_ARGS__
#else
#define CGUI_DEBUG_ONLY(...)
#endif
} // namespace cgui

#endif

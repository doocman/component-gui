//
// Created by rvons on 2024-07-09.
//

#ifndef CGUI_CGUI_WARNINGS_HPP
#define CGUI_CGUI_WARNINGS_HPP

#ifndef NDEBUG
#include <iostream>
#include <source_location>
#include <string_view>
#endif

namespace cgui {
constexpr void unused(auto &&...) {}
#ifndef NDEBUG
#define CGUI_DEBUG_ONLY(...) __VA_ARGS__
constexpr void
cgui_assert(auto &&val, std::string_view text = {},
            std::source_location const &loc = std::source_location::current()) {
  if (!val) {
    std::cerr << loc.file_name() << ':' << loc.line() << ": Assertion failed\n";
    if (!empty(text)) {
      std::cerr << '\t' << text;
    }
  }
}
#define CGUI_ASSERT(EXPR) ::cgui::cgui_assert((EXPR))

#else
#define CGUI_DEBUG_ONLY(...)
#define CGUI_ASSERT(...)
#endif
} // namespace cgui

#endif

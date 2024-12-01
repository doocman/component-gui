
#ifndef CGUI_CGUI_WARNINGS_HPP
#define CGUI_CGUI_WARNINGS_HPP

#ifndef NDEBUG
#include <iostream>
#include <source_location>
#include <string_view>
#endif

namespace cgui {
/// No-op function used to signal that any variables or expressions are ignored
/// on purpose.
/// \return
constexpr void unused(auto &&...) {}
#ifndef NDEBUG
#define CGUI_DEBUG_ONLY(...) __VA_ARGS__
constexpr void
cgui_assert(auto &&val, std::string_view text = {},
            std::source_location const &loc = std::source_location::current()) {
  if (!val) [[unlikely]] {
    std::cerr << loc.file_name() << ':' << loc.line() << ": Assertion failed\n";
    if (!empty(text)) {
      std::cerr << '\t' << text;
    }
    std::abort();
  }
}
#define CGUI_ASSERT(EXPR) ::cgui::cgui_assert((EXPR))

#else
#define CGUI_DEBUG_ONLY(...)
#define CGUI_ASSERT(...)
#endif
} // namespace cgui

#endif

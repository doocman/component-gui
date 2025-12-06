
#ifndef ASPECT_GUI_ASP_ASSERT_HPP
#define ASPECT_GUI_ASP_ASSERT_HPP

#ifndef NDEBUG
#include <asp/import/stl.hpp>
#endif
#include <asp/export/asp_export.hpp>

namespace asp {
/// No-op function used to signal that any variables or expressions are ignored
/// on purpose.
/// \return
ASP_EXPORT constexpr void unused(auto &&...) {}
ASP_EXPORT constexpr void
asp_assert(auto &&val, std::string_view text = {},
           std::source_location const &loc = std::source_location::current()) {
  if (!val) [[unlikely]] {
    std::cerr << loc.file_name() << ':' << loc.line() << ": Assertion failed\n";
    if (!empty(text)) {
      std::cerr << '\t' << text;
    }
    std::abort();
  }
}
#ifndef NDEBUG
#define ASP_DEBUG_ONLY(...) __VA_ARGS__
#define ASP_ASSERT(EXPR) ::asp::asp_assert((EXPR))

#else
#define ASP_DEBUG_ONLY(...)
#define ASP_ASSERT(...)
#endif
} // namespace asp

#endif


#ifndef ASP_ASP_WARNINGS_HPP
#define ASP_ASP_WARNINGS_HPP

#ifndef NDEBUG
#include <iostream>
#include <source_location>
#include <string_view>
#endif

#define ASP_PRAGMA_(X) _Pragma(#X)
#define ASP_PRAGMA(X) ASP_PRAGMA_(X)
#define ASP_PRAGMA_S(X) _Pragma(X)

#if defined(__clang__)
#elif defined(__GNUC__) || defined(__GNUG__)
#elif defined(_MSC_VER)
#define ASP_WARNINGS_PUSH _Pragma("warning(push)")
#define ASP_SUPPRESSW_MSVC(X) ASP_PRAGMA(warning(disable : X))
#define ASP_WARNINGS_POP _Pragma("warning(pop)")
#endif

#ifndef ASP_SUPPRESSW_MSVC
#define ASP_SUPPRESSW_MSVC(...)
#endif
#ifndef ASP_WARNINGS_PUSH
#define ASP_WARNINGS_PUSH
#define ASP_WARNINGS_POP
#endif

namespace asp {
/// No-op function used to signal that any variables or expressions are ignored
/// on purpose.
/// \return
constexpr void unused(auto &&...) {}
#ifndef NDEBUG
#define ASP_DEBUG_ONLY(...) __VA_ARGS__
constexpr void
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
#define ASP_ASSERT(EXPR) ::asp::asp_assert((EXPR))

#else
#define ASP_DEBUG_ONLY(...)
#define ASP_ASSERT(...)
#endif
} // namespace asp

#endif


#ifndef ASP_ASP_WARNINGS_HPP
#define ASP_ASP_WARNINGS_HPP

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

#endif


#ifndef ASPECT_GUI_ASP_ANNOTATE_HPP
#define ASPECT_GUI_ASP_ANNOTATE_HPP

#include "asp/export/asp_export.hpp"

namespace asp {


ASP_EXPORT template <typename T>
struct out {
  T&& result;

  constexpr explicit out(T&& t) : result(std::forward<T>(t)) {}
  constexpr T& operator*() const { return result; }
  constexpr T&& get_forwarded() const { return std::forward<T>(result); }
};

template <typename T>
out(T&&) -> out<T>;

}

#endif // ASPECT_GUI_ANNOTATE_HPP

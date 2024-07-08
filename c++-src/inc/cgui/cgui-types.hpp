
#ifndef COMPONENT_GUI_CGUI_TYPES_HPP
#define COMPONENT_GUI_CGUI_TYPES_HPP

#include <optional>
#include <tuple>
#include <utility>

namespace cgui {
struct rect {
  int x;
  int y;
  int w;
  int h;
};
struct no_auto_cleanup_t {};
inline constexpr no_auto_cleanup_t no_auto_cleanup;
struct auto_cleanup_t {};
inline constexpr auto_cleanup_t auto_cleanup;

template <typename TQuit, typename... TArgs> struct cleanup_object_t {
  using value_type = std::tuple<TArgs...>;
  std::optional<value_type> values;
private:
  void cleanup() {
    if (values) {
      std::apply(TQuit{}, *values);
    }
  }
public:

  constexpr cleanup_object_t() = default;
  constexpr explicit cleanup_object_t(TArgs... args) requires(sizeof...(TArgs) > 0 && (std::is_copy_constructible_v<TArgs> && ...))
      : values(std::tuple<TArgs...>(std::move(args)...)) {}
  constexpr explicit cleanup_object_t(no_auto_cleanup_t)
      : values(std::nullopt) {}
  constexpr explicit cleanup_object_t(std::in_place_t, auto &&...args)
      : values(std::in_place, std::forward<decltype(args)>(args)...) {}
  constexpr cleanup_object_t(cleanup_object_t &&other) noexcept
      : values(std::exchange(other.values, std::nullopt)) {}
  constexpr cleanup_object_t &operator=(cleanup_object_t &&other) noexcept {
    std::swap(values, other.values);
    return *this;
  }
  constexpr ~cleanup_object_t() {
    cleanup();
  }
  constexpr void reset() {
    cleanup();
    values = std::nullopt;
  }
  constexpr void reset(std::convertible_to<value_type> auto&& v) noexcept {
    cleanup();
    values = std::forward<decltype(v)>(v);
  }
  constexpr bool has_value() const noexcept {
    return values.has_value();
  }
  constexpr operator bool() const noexcept {
    return has_value();
  }
};

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_TYPES_HPP

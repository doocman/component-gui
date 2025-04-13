
#ifndef COMPONENT_GUI_CGUI_LIST_LAYOUTS_HPP
#define COMPONENT_GUI_CGUI_LIST_LAYOUTS_HPP

#include <cstddef>

#include <cgui/cgui-call.hpp>
#include <cgui/geometry.hpp>

namespace cgui {
template <typename T, typename SWH = default_point_size_wh,
          typename C = default_point_coordinate>
concept is_layout =
    requires(T const &t, std::ptrdiff_t i, SWH const &swh, C const &c) {
      { t.area_for_index(i, swh) } -> point_rect;
      { t.index_at(c, swh) } -> std::convertible_to<std::ptrdiff_t>;
    };

class vertical_list_layout {
  point_unit_t<int> element_height_;

public:
  explicit constexpr vertical_list_layout(point_unit_t<int> eh) noexcept
      : element_height_(eh) {}
  constexpr default_point_rect
  area_for_index(int i, point_size_wh auto const &wrapped_area) const noexcept {
    auto y_start = element_height_.value() * i;
    return default_point_rect{{{0, y_start},
                               {call::width(wrapped_area).value(),
                                y_start + element_height_.value()}}};
  }
  constexpr std::ptrdiff_t index_at(point_coordinate auto const &p,
                                    point_size_wh auto const &) const noexcept {
    auto index = static_cast<std::ptrdiff_t>(call::y_of(p).value() /
                                             element_height_.value());
    return index;
  }
};

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_LIST_LAYOUTS_HPP

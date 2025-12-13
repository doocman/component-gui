
#ifndef COMPONENT_GUI_ASP_LIST_LAYOUTS_HPP
#define COMPONENT_GUI_ASP_LIST_LAYOUTS_HPP

#include <asp/import/stl.hpp>

#include <asp/call.hpp>
#include <asp/geometry.hpp>

namespace asp {
template <typename T, typename SWH = basic_coordinate_delta<point, float>,
          typename C = basic_coordinate<point, float>>
concept is_layout =
    requires(T const &t, std::ptrdiff_t i, SWH const &swh, C const &c) {
      { t.area_for_index(i, swh) } -> point_rect;
      { t.index_at(c, swh) } -> std::convertible_to<std::ptrdiff_t>;
    };

template <typename Rep>
class vertical_list_layout {
  mp_units::quantity<mp_units::isq::height[point], rep> element_height_;

public:
  explicit constexpr vertical_list_layout(mp_units::quantity<mp_units::isq::height[point], rep> eh) noexcept
      : element_height_(eh) {}
  constexpr default_point_rect
  area_for_index(int i, is_width_and_height_with_unit<point> auto const &wrapped_area) const noexcept {
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

} // namespace asp

#endif // COMPONENT_GUI_ASP_LIST_LAYOUTS_HPP

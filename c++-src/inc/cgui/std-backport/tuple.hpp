#ifndef COMPONENT_GUI_TUPLE_HPP
#define COMPONENT_GUI_TUPLE_HPP

#include <tuple>

namespace cgui::bp {
template <typename T>
concept has_tuple_size = requires() {
  std::tuple_size<std::remove_cvref_t<T>>::value;
};
}

#endif // COMPONENT_GUI_TUPLE_HPP

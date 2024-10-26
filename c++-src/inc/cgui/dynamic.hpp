//
// Created by rvons on 2024-10-25.
//

#ifndef COMPONENT_GUI_DYNAMIC_HPP
#define COMPONENT_GUI_DYNAMIC_HPP

#include <tuple>

#include <cgui/std-backport/utility.hpp>

namespace cgui::dynamic {
class uni_sized_widget_list_impl {

};
template <typename Displays>
class uni_sized_widget_list_builder_impl : bp::empty_structs_optimiser<Displays> {
  using _base_t = bp::empty_structs_optimiser<Displays>;
public:
  using _base_t::_base_t;

  template <typename T2>
  constexpr impl::
};

constexpr uni_sized_widget_list_builder_impl<std::tuple<>> uni_sized_widget_list_builder() {
  return {};
}
}

#endif // COMPONENT_GUI_DYNAMIC_HPP

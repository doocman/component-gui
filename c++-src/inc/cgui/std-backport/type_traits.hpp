//
// Created by rvons on 2024-08-20.
//

#ifndef COMPONENT_GUI_TYPE_TRAITS_HPP
#define COMPONENT_GUI_TYPE_TRAITS_HPP

#include <type_traits>

namespace cgui::bp {
template <typename T>
using pointer_reference_t = decltype(*std::declval<T&&>());
}

#endif // COMPONENT_GUI_TYPE_TRAITS_HPP

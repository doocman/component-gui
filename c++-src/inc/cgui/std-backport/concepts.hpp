//
// Created by rvons on 2024-07-24.
//

#ifndef COMPONENT_GUI_CONCEPTS_HPP
#define COMPONENT_GUI_CONCEPTS_HPP

#include <concepts>
#include <type_traits>

namespace cgui::bp {
template <typename T>
concept pure_value = std::is_same_v<T, std::remove_cvref_t<T>>;
template <typename T, typename TRaw>
concept cvref_type = std::is_same_v<std::remove_cvref_t<T>, std::remove_cvref_t<TRaw>>;
}

#endif // COMPONENT_GUI_CONCEPTS_HPP

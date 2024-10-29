//
// Created by doocm on 2024-09-30.
//

#ifndef ARRAY_HPP
#define ARRAY_HPP

#include <array>

namespace cgui::bp {
namespace impl {
template <std::invocable TCreator, std::size_t... tIs>
constexpr std::array<std::invoke_result_t<TCreator>, sizeof...(tIs)>
do_array_from_args(TCreator const &creator, std::index_sequence<tIs...>) {
  return {{creator(tIs)...}};
}
} // namespace impl
/// @brief constructs all elements in array using the same arguments to the
/// constructors.
/// \tparam T array value type. Used explicitly.
/// \tparam tSz size of array. Used explicitly.
/// \tparam TArgs Type of the arguments to pass to constructors.
/// \param args Arguments to pass to constructors.
/// \return constructed array.
template <typename T, std::size_t tSz, typename... TArgs>
  requires(std::constructible_from<T, TArgs const &...>)
constexpr std::array<T, tSz> array_from_args(TArgs const &...args) {
  return impl::do_array_from_args([&args...](auto &&...) { return T(args...); },
                                  std::make_index_sequence<tSz>{});
}
} // namespace cgui::bp

#endif

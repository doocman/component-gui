
#ifndef RANGES_HPP
#define RANGES_HPP

#include <ranges>

#include <cgui/std-backport/tuple.hpp>

namespace cgui::bp {
template <typename /*Base*/, typename /*NewElementType*/>
struct transformed_range {};
template <template <typename, typename...> typename TRange, typename TVal,
          typename... Ts, typename NewElementType>
  requires std::is_same_v<std::ranges::range_value_t<TRange<TVal, Ts...>>, TVal>
struct transformed_range<TRange<TVal, Ts...>, NewElementType> {
  using type = TRange<NewElementType, Ts...>;
};

template <typename Base, typename NewValueT>
using transformed_range_t =
    typename transformed_range<std::remove_cvref_t<Base>, NewValueT>::type;

template <std::ranges::range T, typename ET = std::ranges::range_reference_t<T>,
          std::invocable<ET> F,
          typename FR = std::unwrap_ref_decay_t<std::invoke_result_t<T, ET>>>
constexpr transformed_range_t<T, FR> transform_range(T &&r, F &&f) {
  if constexpr (has_tuple_size<T>) {
    return std::apply([&f](auto &&...vals) {
      return transformed_range_t<T, FR>{f(vals)...};
    });
  } else {
    transformed_range_t<T, FR> res;
    if constexpr (requires() { res.reserve(0); }) {
      res.reserve(std::ranges::size(r));
    }
    if constexpr (requires() { res.push_back(std::declval<FR &&>()); }) {
      std::ranges::transform(std::forward<T>(r), std::back_inserter(res),
                             std::forward<F>(f));
    } else {
      // If we hit this assert, then we don't understand how the range works...
      CGUI_ASSERT(std::ranges::ssize(res) == std::ranges::ssize(r));
      std::ranges::transform(std::forward<T>(r), std::ranges::begin(res),
                             std::forward<F>(f));
    }
    return res;
  }
}

} // namespace cgui::bp

#endif

//
// Created by rvons on 2024-09-21.
//

#ifndef COMPONENT_GUI_FUNCTIONAL_HPP
#define COMPONENT_GUI_FUNCTIONAL_HPP

#include <functional>

namespace cgui::bp {
struct no_op_t {
  constexpr void operator()(auto &&...) const noexcept {}
};
inline constexpr no_op_t no_op;

template <typename TF, typename... TVals> class trailing_curried {
  TF f_;
  std::tuple<TVals...> values_;

public:
  template <typename TF2, typename... TArgs>
    requires(std::constructible_from<TF, TF2> &&
             (std::constructible_from<TVals, TArgs> && ...))
  constexpr explicit trailing_curried(TF2 &&f, TArgs &&...args)
      : f_(std::forward<TF2>(f)), values_(std::forward<TArgs>(args)...) {}

  constexpr decltype(auto) operator()(auto &&...args) const {
    return std::apply(
        [this, &args...](auto &&...vals) -> decltype(auto) {
          return f_(std::forward<decltype(args)>(args)...,
                    std::forward<decltype(vals)>(vals)...);
        },
        values_);
  }
};

template <typename TF, typename... TArgs>
trailing_curried(TF &&, TArgs &&...)
    -> trailing_curried<std::unwrap_ref_decay_t<TF>,
                        std::unwrap_ref_decay_t<TArgs>...>;

} // namespace cgui::bp

#endif // COMPONENT_GUI_FUNCTIONAL_HPP

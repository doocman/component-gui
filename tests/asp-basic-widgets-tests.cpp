
#include <gmock/gmock.h>

#include <asp/types.hpp>

namespace asp::tests {
struct stub_renderer {};

class fill_rectangle {

public:
  struct render_cache_t {};
  constexpr explicit fill_rectangle(colour auto) {}
  friend constexpr render_cache_t initial_render_cache(fill_rectangle const &,
                                                       auto &&) {
    return {};
  }
};

TEST(FillRect, DISABLED_InitialRenderCache) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(255, 0, 0, 255));
  auto renderer = stub_renderer{};
  // auto cache = initial_render_cache(fr, render_context(renderer,
  // default_point_rect{}));
}
} // namespace asp::tests


#ifndef COMPONENT_GUI_TESTS_CGUI_TEST_UTILS_HPP
#define COMPONENT_GUI_TESTS_CGUI_TEST_UTILS_HPP

#include <source_location>

#include <gmock/gmock.h>

#include <cgui/cgui.hpp>

namespace cgui::tests {
inline auto
expect_colour_eq(cgui::colour auto const &val,
                 cgui::colour auto const &expected,
                 std::source_location s = std::source_location::current()) {
  using namespace ::testing;
  auto [vr, vg, vb, va] = val;
  auto [er, eg, eb, ea] = expected;
  EXPECT_THAT((std::array{vr, vg, vb, va}),
              ElementsAre(Eq(er), Eq(eg), Eq(eb), Eq(ea)))
      << "Called at line " << s.line();
}

inline void expect_box_equal(
    bounding_box auto const &to_test, bounding_box auto const &to_expect,
    std::source_location const &sl = std::source_location::current()) {
  using namespace ::testing;
  EXPECT_THAT(call::l_x(to_test), Eq(call::l_x(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
  EXPECT_THAT(call::t_y(to_test), Eq(call::t_y(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
  EXPECT_THAT(call::r_x(to_test), Eq(call::r_x(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
  EXPECT_THAT(call::b_y(to_test), Eq(call::b_y(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
}

struct test_renderer {
  struct individual_colours_t {
    std::vector<std::uint_least8_t> red, green, blue, alpha;
    explicit individual_colours_t(std::vector<default_colour_t> const &pix) {
      red.reserve(size(pix));
      green.reserve(size(pix));
      blue.reserve(size(pix));
      alpha.reserve(size(pix));
      for (auto const &c : pix) {
        red.push_back(c.red);
        green.push_back(c.green);
        blue.push_back(c.blue);
        alpha.push_back(c.alpha);
      }
    }
  };

  default_rect a_;
  std::vector<default_colour_t> drawn_pixels;
  std::vector<default_rect> failed_calls;
  std::vector<default_pixel_coord> failed_pixel_draws;

  explicit test_renderer(default_rect a)
      : a_(a), drawn_pixels(call::width(a) * call::height(a)) {}

  auto &at(int x, int y) {
    auto index = x + y * call::width(area());
    return drawn_pixels.at(index);
  }

  void draw_pixels(bounding_box auto &&b, auto &&cb) {
    if (!box_includes_box(area(), b)) {
      failed_calls.push_back(box_from_xyxy<default_rect>(
          call::l_x(b), call::t_y(b), call::r_x(b), call::b_y(b)));
      return;
    }
    cb([this, &b](auto &&pos, auto &&col) {
      if (!hit_box(b, pos)) {
        failed_pixel_draws.emplace_back(call::x_of(pos), call::y_of(pos));
        return;
      }
      at(call::x_of(pos), call::y_of(pos)) = {
          call::red(col), call::green(col), call::blue(col), call::alpha(col)};
    });
  }

  default_rect area() const { return a_; }

  individual_colours_t individual_colours() const {
    return individual_colours_t(drawn_pixels);
  }
};

struct mock_colourable {
  MOCK_METHOD(void, do_render, (), (const));
  MOCK_METHOD(void, colour, (default_colour_t const &), ());

  void render(auto &&...) const { do_render(); }
};

struct mock_state_aware_renderer {
  MOCK_METHOD(void, do_render, (int), (const));
  MOCK_METHOD(void, do_set_state, (int), (const));
  std::string_view mutable render_failed_type{};

  void render(renderer auto &&, auto &&arg) const {
    using state_t =
        std::remove_cvref_t<decltype(arg.widget_state().current_state())>;
    if constexpr (std::is_integral_v<state_t>) {
      do_render(arg.widget_state().current_state());
    } else {
      render_failed_type = std::string_view(typeid(state_t).name());
    }
  }
  void set_state(state_marker auto const &i, widget_back_propagater auto &&cb) {
    do_set_state(i.current_state());
    cb.rerender();
  }
};

struct int_states {
  int state_ = 0;
  void handle(int i) {
    CGUI_ASSERT(i == 0 || i == 1);
    state_ = i;
  }
  widget_state_marker<int, 0, 1> state() const { return state_; }
};

static_assert(has_handle<int_states, int>);

struct int_as_event_handler {
  int state_ = 0;
  void handle(auto const &, int i) { state_ = std::clamp(i, 0, 1); }
  widget_state_marker<int, 0, 1> state() const { return state_; }
};

struct mock_widget {
  default_rect a_{};
  MOCK_METHOD(void, do_area, (default_rect const &));
  MOCK_METHOD(void, do_render, (), (const));

  default_rect const &area() const { return a_; }
  void area(bounding_box auto const &a) {
    a_ = copy_box<default_rect>(a);
    do_area(a_);
  }
  void render(auto &&...) const { do_render(); }
};

constexpr void click_widget(auto &w, default_pixel_coord const &pos = {},
                            auto &&...args) {
  w.handle(dummy_mouse_down_event{.pos = pos, .button_id = {}}, args...);
  w.handle(dummy_mouse_up_event{.pos = pos, .button_id = {}}, args...);
}

} // namespace cgui::tests

#endif // COMPONENT_GUI_CGUI_TEST_UTILS_HPP
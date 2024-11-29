
#ifndef COMPONENT_GUI_TESTS_CGUI_TEST_UTILS_HPP
#define COMPONENT_GUI_TESTS_CGUI_TEST_UTILS_HPP

#include <source_location>

#include <gmock/gmock.h>

#include <cgui/cgui.hpp>

namespace cgui::tests {
template <interpreted_events evt>
constexpr interpreted_event<evt, std::chrono::steady_clock::time_point>
create_event(auto &&...args) {
  return interpreted_event<evt, std::chrono::steady_clock::time_point>(
      std::chrono::steady_clock::time_point{}, args...);
}
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

template <bounding_box ToTest, bounding_box ToExpect>
  requires(!size_tagged<ToTest> && !size_tagged<ToExpect>)
inline void expect_box_equal(
    ToTest const &to_test, ToExpect const &to_expect,
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

inline void expect_box_equal(
    pixel_or_point_rect_basic auto const &to_test,
    pixel_or_point_rect_basic auto const &to_expect,
    std::source_location const &sl = std::source_location::current()) {
  return expect_box_equal(to_test.value(), to_expect.value(), sl);
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

  pixel_unit_t<default_rect> a_;
  std::vector<default_colour_t> drawn_pixels;
  std::vector<default_rect> failed_calls;
  std::vector<default_coordinate> failed_pixel_draws;

  explicit test_renderer(default_rect a)
      : a_(a), drawn_pixels(call::width(a) * call::height(a)) {}

  auto &at(int x, int y) {
    auto index = x + y * call::width(pixel_area().value());
    return drawn_pixels.at(index);
  }
  auto &at(pixel_unit_t<int> x, pixel_unit_t<int> y) {
    return at(x.value(), y.value());
  }

  void draw_pixels(default_pixel_rect b, auto &&cb) {
    if (!box_includes_box(pixel_area(), b)) {
      failed_calls.push_back(
          box_from_xyxy<default_pixel_rect>(call::l_x(b), call::t_y(b),
                                            call::r_x(b), call::b_y(b))
              .value());
      return;
    }
    cb([this, &b](auto &&pos, auto &&col) {
      if (!hit_box(b, pos)) {
        failed_pixel_draws.emplace_back(call::x_of(pos).value(),
                                        call::y_of(pos).value());
        return;
      }
      at(call::x_of(pos), call::y_of(pos)) = {
          call::red(col), call::green(col), call::blue(col), call::alpha(col)};
    });
  }

  default_pixel_rect pixel_area() const { return a_; }

  individual_colours_t individual_colours() const {
    return individual_colours_t(drawn_pixels);
  }

  static constexpr int pixel_scale() { return 1; }
};
static_assert(canvas<test_renderer>);

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
  default_point_rect a_{};
  MOCK_METHOD(void, do_area, (default_point_rect const &));
  MOCK_METHOD(void, do_render, (), (const));

  default_point_rect const &area() const { return a_; }
  void area(point_rect auto const &a) {
    a_ = copy_box<default_point_rect>(a);
    do_area(a_);
  }
  void render(auto &&...) const { do_render(); }
};

constexpr void click_widget(auto &w, default_point_coordinate const &pos = {},
                            auto &&...args) {
  if constexpr (has_handle<decltype(w), default_mouse_down_event, decltype(args)...>) {
    w.handle(default_mouse_down_event{.pos = pos, .button_id = {}}, args...);
    w.handle(default_mouse_up_event{.pos = pos, .button_id = {}}, args...);
  } else {
    w.handle(interpreted_event<interpreted_events::primary_click>(std::chrono::steady_clock::time_point{}, pos), args...);
    w.handle(interpreted_event<interpreted_events::pointer_hover>(std::chrono::steady_clock::time_point{}, pos), args...);
  }
}

} // namespace cgui::tests

#endif // COMPONENT_GUI_CGUI_TEST_UTILS_HPP

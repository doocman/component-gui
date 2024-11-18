

#include <cgui/std-backport/limits.hpp>

#include <cgui/auto_ref.hpp>
#include <cgui/cgui-types.hpp>
#include <cgui/cgui.hpp>
#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/expected.hpp>
#include <cgui/stl_extend.hpp>
#include <cgui/widget_algorithm.hpp>

#include <array>
#include <optional>
#include <source_location>
#include <string_view>
#include <tuple>
#include <typeinfo>

#include <gmock/gmock.h>

#include <dooc/named_args_tuple.hpp>

#include <cgui_test_utils.hpp>

namespace cgui::tests {
static_assert(
    std::is_same_v<std::common_type_t<bp::default_init_valued_t, int>, int>);
static_assert(requires(std::tuple<int> t) {
  call::impl::do_apply_to{}(t, bp::no_op);
});
static_assert(single_pixel_draw<dummy_pixel_drawer>);
static_assert(single_alpha_draw<dummy_alpha_drawer>);
static_assert(pixel_draw_callback<dummy_pixel_draw_callback>);
static_assert(alpha_draw_callback<dummy_alpha_draw_callback>);

using namespace ::testing;

static_assert(canvas<dummy_canvas>);
static_assert(renderer<dummy_renderer>);

struct mock_button_callback {
  MOCK_METHOD(void, do_on_button_hover, ());
  MOCK_METHOD(void, do_on_button_hold, ());
  MOCK_METHOD(void, do_on_button_click, (mouse_buttons b));
  MOCK_METHOD(void, do_on_button_exit, ());

  void handle(button_state_events::hover) { do_on_button_hover(); }
  void handle(button_state_events::hold) { do_on_button_hold(); }
  void handle(button_state_events::click const &e) {
    do_on_button_click(e.button);
  }
  void handle(button_state_events::exit) { do_on_button_exit(); }
  void operator()(auto const &e) { handle(e); }
  no_state_t state() const { return {}; }
};
static_assert(button_state<mock_button_callback>);

TEST(ButtonlikeEventTrigger, MouseHoverAndClick) // NOLINT
{
  auto button_state = mock_button_callback();
  auto trig = buttonlike_trigger(std::ref(button_state));
  auto checkpoint = MockFunction<void()>();
  InSequence s;
  EXPECT_CALL(button_state, do_on_button_hover());
  EXPECT_CALL(button_state, do_on_button_hold());
  EXPECT_CALL(checkpoint, Call());
  EXPECT_CALL(button_state, do_on_button_click(Eq(mouse_buttons::primary)));
  EXPECT_CALL(button_state, do_on_button_exit());
  constexpr auto area = point_unit(default_rect{{0, 0}, {4, 4}});
  trig.handle(area, default_mouse_move_event{{1, 1}});
  trig.handle(area, default_mouse_down_event{{1, 1}, mouse_buttons::primary});
  checkpoint.Call();
  trig.handle(area, default_mouse_up_event{{1, 1}, mouse_buttons::primary});
  trig.handle(area, default_mouse_move_event{{-1, 1}});
}

struct mock_widget_resize {

  MOCK_METHOD(void, do_resize, (int w, int h));
  void area(bounding_box auto const &b) {
    do_resize(call::width(b.value()), call::height(b.value()));
  }
  void render(auto &&) const {}
};

template <typename T> struct ref_builder {
  T *to_return_;

  constexpr T &build(auto &&...) const { return *to_return_; }
};

TEST(GuiContext, BuildResize) // NOLINT
{
  auto w = mock_widget_resize();
  InSequence s;
  EXPECT_CALL(w, do_resize(2, 2));
  EXPECT_CALL(w, do_resize(3, 3));
  auto gui = gui_context_builder()
                 .widgets(std::ref(w))
                 .on_resize([](size_wh auto const &wh, auto &&widgets) {
                   auto &[w] = widgets;
                   w.area(extend_api_t<default_point_rect>::from_xywh(
                       bp::default_init_valued, bp::default_init_valued,
                       call::width(wh), call::height(wh)));
                 })
                 .build({{0, 0}, {2, 2}});
  auto area = gui.handle(default_window_resized_event{{3, 3}});
  expect_box_equal(area, box_from_xyxy<default_point_rect>(0, 0, 3, 3));
}

struct rerender_if_state {
  int rerender_state;

  constexpr void render(auto &&...) const noexcept {}
  constexpr void set_state(state_marker auto const &i,
                           widget_back_propagater auto &&cb) {
    if (i.current_state() == rerender_state) {
      cb.rerender();
    }
  }
};
TEST(GuiContext, RerenderOutput) // NOLINT
{
  auto w1b = widget_builder()
                 .area(default_rect{{0, 0}, {1, 1}})
                 .event(int_as_event_handler{})
                 .display(rerender_if_state{0});
  auto w2b = widget_builder()
                 .area(default_rect{{1, 0}, {2, 1}})
                 .event(int_as_event_handler{})
                 .display(rerender_if_state{1});
  auto w3b = widget_builder()
                 .area(default_rect{{2, 0}, {3, 1}})
                 .event(int_as_event_handler{})
                 .display(rerender_if_state{0});
  auto r = test_renderer({{0, 0}, {3, 1}});
  auto guic = gui_context_builder()
                  .widgets(std::move(w1b), std::move(w2b), std::move(w3b))
                  .build({{0, 0}, {1, 1}});
  guic.render(r);
  auto rarea = guic.handle(1);
  expect_box_equal(rarea, box_from_xyxy<default_pixel_rect>(1, 0, 2, 1));
  rarea = guic.handle(0);
  EXPECT_TRUE(
      box_includes_box(rarea, box_from_xyxy<default_point_rect>(0, 0, 1, 1)));
  EXPECT_TRUE(
      box_includes_box(rarea, box_from_xyxy<default_point_rect>(0, 0, 1, 1)));
}

struct reference_stack_tester {
  int my_value{};

  // not constexpr by intention
  explicit reference_stack_tester(int mv) : my_value(mv) {}

  // Recursively collect `my_value` from the stack into a vector.
  static constexpr std::vector<int>
  collect_stack_values(auto &&refstack, std::vector<int> result = {}) {
    result.push_back(refstack.ref().my_value);
    if constexpr (requires() { refstack.previous(); }) {
      return collect_stack_values(refstack.previous(), std::move(result));
    } else {
      return result;
    }
  }
};

TEST(ReferenceStack, BasicBehaviour) {
  reference_stack_tester v1{1};
  std::vector<int> vals =
      reference_stack_tester::collect_stack_values(reference_stack(v1));
  EXPECT_THAT(vals, ElementsAre(1));
  static_assert(size(reference_stack(v1)) == 1);

  reference_stack_tester v2{2}, v3{3};
  vals = reference_stack_tester::collect_stack_values(
      reference_stack(v1).push(v2).push(v3));
  EXPECT_THAT(vals, ElementsAre(3, 2, 1));
  static_assert(size(reference_stack(v1).push(v2).push(v3)) == 3);
}
} // namespace cgui::tests

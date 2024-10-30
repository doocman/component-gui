
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
static_assert(requires(std::tuple<int> t) {
  call::impl::do_apply_to{}(t, bp::no_op);
});

using namespace ::testing;

static_assert(canvas<dummy_canvas>);
static_assert(renderer<dummy_renderer>);

TEST(UiEventsMatcher, Basics) // NOLINT
{
  auto f = MockFunction<void(std::string_view)>();
  InSequence s;
  EXPECT_CALL(f, Call(StrEq("Move")));
  EXPECT_CALL(f, Call(StrEq("Down")));
  EXPECT_CALL(f, Call(StrEq("Up")));

  using enum ui_events;
  auto my_switch = [&f](auto &&evt) {
    ui_event_switch(evt,
                    event_case<mouse_button_down>([&f] { f.Call("Down"); }),
                    event_case<mouse_button_up>([&f] { f.Call("Up"); }),
                    event_case<mouse_move>([&f] { f.Call("Move"); }));
  };
  my_switch(dummy_mouse_move_event{});
  my_switch(dummy_mouse_down_event{});
  my_switch(dummy_mouse_up_event{});
}

TEST(UiEventsMatcher, BasicsState) // NOLINT
{
  auto f = MockFunction<void(std::string_view)>();
  InSequence s;
  EXPECT_CALL(f, Call(StrEq("Move")));
  EXPECT_CALL(f, Call(StrEq("Down")));
  EXPECT_CALL(f, Call(StrEq("Up")));

  using enum ui_events;
  auto my_switch = [&f](auto &&evt) {
    ui_event_switch(
        evt, f, event_case<mouse_button_down>([](auto &&, auto &f2) {
          f2.Call("Down");
        }),
        event_case<mouse_button_up>([](auto &&, auto &f2) { f2.Call("Up"); }),
        event_case<mouse_move>([&f] { f.Call("Move"); }));
  };
  my_switch(dummy_mouse_move_event{});
  my_switch(dummy_mouse_down_event{});
  my_switch(dummy_mouse_up_event{});
}

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
  constexpr auto area = default_rect{{0, 0}, {4, 4}};
  trig.handle(area, dummy_mouse_move_event{{1, 1}});
  trig.handle(area, dummy_mouse_down_event{{1, 1}, mouse_buttons::primary});
  checkpoint.Call();
  trig.handle(area, dummy_mouse_up_event{{1, 1}, mouse_buttons::primary});
  trig.handle(area, dummy_mouse_move_event{{-1, 1}});
}

struct mock_widget_resize {

  MOCK_METHOD(void, do_resize, (int w, int h));
  void area(bounding_box auto const &b) {
    do_resize(call::width(b), call::height(b));
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
  auto gui =
      gui_context_builder()
          .widgets(std::ref(w))
          .on_resize([](size_wh auto const &wh, auto &&widgets) {
            auto &[w] = widgets;
            w.area(default_rect{0, 0, call::width(wh), call::height(wh)});
          })
          .build({{0, 0}, {2, 2}});
  auto area = gui.handle(dummy_window_resized_event{{3, 3}});
  expect_box_equal(area, default_rect{{0, 0}, {3, 3}});
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
  expect_box_equal(rarea, default_rect{{1, 0}, {2, 1}});
  rarea = guic.handle(0);
  EXPECT_TRUE(box_includes_box(rarea, default_rect{{0, 0}, {1, 1}}));
  EXPECT_TRUE(box_includes_box(rarea, default_rect{{2, 0}, {3, 1}}));
}
} // namespace cgui::tests


#include <cgui/ui_events.hpp>

#include <gmock/gmock.h>

namespace cgui::tests {
using namespace ::testing;


TEST(UiEventsMatcher, Basics) // NOLINT
{
  auto f = MockFunction<void(std::string_view)>();
  InSequence s;
  EXPECT_CALL(f, Call(StrEq("Move")));
  EXPECT_CALL(f, Call(StrEq("Down")));
  EXPECT_CALL(f, Call(StrEq("Up")));

  using enum input_events;
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

  using enum input_events;
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

TEST(InterpretedEvents, MouseClick) // NOLINT
{
  auto to_test = input_event_interpreter();
  auto update_tt = [&to_test](auto &&evt) {
    to_test.update(evt);
    return evt;
  };
  EXPECT_FALSE(is_event<interpreted_events::primary_click>(
      update_tt(dummy_mouse_down_event{}), to_test));
  EXPECT_TRUE(is_event<interpreted_events::primary_click>(
      update_tt(dummy_mouse_up_event{}), to_test));

  EXPECT_FALSE(is_event<interpreted_events::primary_click>(
      update_tt(dummy_mouse_down_event{{}, mouse_buttons::middle}), to_test));
  EXPECT_FALSE(is_event<interpreted_events::primary_click>(
      update_tt(dummy_mouse_up_event{{}, mouse_buttons::middle}), to_test));

  EXPECT_TRUE(is_event<interpreted_events::context_menu_click>(
      update_tt(dummy_mouse_up_event{{}, mouse_buttons::secondary}), to_test));
}

TEST(InterpretedEvents, TouchClick) // NOLINT
{
  FAIL() << "Not yet implemented";
}
} // namespace cgui::tests

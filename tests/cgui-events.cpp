
#include <cgui/cgui.hpp>

#include <gmock/gmock.h>

#include <cgui_test_utils.hpp>

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
  my_switch(default_mouse_move_event{});
  my_switch(default_mouse_down_event{});
  my_switch(default_mouse_up_event{});
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
  my_switch(default_mouse_move_event{});
  my_switch(default_mouse_down_event{});
  my_switch(default_mouse_up_event{});
}

}

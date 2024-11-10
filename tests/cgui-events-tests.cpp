
#include <cgui/ui_events.hpp>

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

TEST(GestureEvents, MouseClick) // NOLINT
{
  using namespace std::chrono_literals;
  interpreted_events last_event{};
  using state_t = std::variant<int, primary_mouse_click_translator::state>;
  int last_confirmed = -1;
  int event_calls{};
  auto to_test = event_interpreter<primary_mouse_click_translator>{};
  auto reset_values = [&] {
    last_confirmed = -1;
    last_event = {};
    event_calls = {};
  };
  auto test_callback = [&] <interpreted_events ev, bool confirmed> (interpreted_event<ev, confirmed> const&) {
    ++event_calls;
    last_event = ev;
    last_confirmed = confirmed ? 1 : 0;
  };
  auto invoke_tt = [&] (auto const& input_evt) {
    reset_values();
    to_test.handle(input_evt, test_callback);
  };
  invoke_tt(default_mouse_down_event{});
  EXPECT_THAT(event_calls, Eq(0));
  invoke_tt(default_mouse_up_event{});
  EXPECT_THAT(last_event, Eq(interpreted_events::primary_click));
  EXPECT_THAT(last_confirmed, Eq(0));
  EXPECT_THAT(event_calls, Eq(1));
  reset_values();
  to_test.pass_time(201ms, test_callback);
  EXPECT_THAT(last_event, Eq(interpreted_events::primary_click));
  EXPECT_THAT(last_confirmed, Eq(1));
  EXPECT_THAT(event_calls, Eq(1));
}

}

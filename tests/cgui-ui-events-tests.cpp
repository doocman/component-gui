
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

TEST(GestureEvents, MouseClick) // NOLINT
{
  using namespace std::chrono_literals;
  interpreted_events last_event{};
  int event_calls{};
  auto to_test = mouse_keys_translator([&last_event, &event_calls] <interpreted_events ev> (interpreted_event<ev> const&) {
    ++event_calls;
    last_event = ev;
  });
  to_test.handle(dummy_mouse_down_event{});
  EXPECT_THAT(event_calls, Eq(0));
  to_test.handle(dummy_mouse_up_event{});
  EXPECT_THAT(last_event, Eq(interpreted_events::preliminary_primary_click));
  EXPECT_THAT(event_calls, Eq(1));
  to_test.pass_time(201ms);
  EXPECT_THAT(last_event, Eq(interpreted_events::confirmed_primary_click));
  EXPECT_THAT(event_calls, Eq(2));
}
/*
TEST(InterpretedEvents, MouseClick) // NOLINT
{
  auto to_test = input_event_interpreter();
  auto update_tt = [&to_test](auto &&evt) {
    to_test.update(evt);
    return evt;
  };
  static_assert(can_be_event<interpreted_events::preliminary_primary_click, dummy_mouse_up_event>());
  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_mouse_down_event{}), to_test));
  EXPECT_TRUE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_mouse_up_event{}), to_test));

  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_mouse_down_event{{}, mouse_buttons::middle}), to_test));
  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_mouse_up_event{{}, mouse_buttons::middle}), to_test));

  EXPECT_TRUE(is_event<interpreted_events::context_menu_click>(
      update_tt(dummy_mouse_up_event{{}, mouse_buttons::secondary}), to_test));
}

TEST(InterpretedEvents, TouchClick) // NOLINT
{
  using namespace std::chrono;
  auto to_test = input_event_interpreter();
  auto update_tt = [&to_test](auto &&evt) {
    to_test.update(evt);
    return evt;
  };
  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_touch_finger_down_event{}), to_test));
  EXPECT_TRUE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_touch_finger_up_event{}), to_test));
  EXPECT_FALSE(is_event<interpreted_events::confirmed_primary_click>(dummy_touch_finger_up_event {}, to_test));
  bool called_in_update{};
  //to_test.update(std::chrono::milliseconds(201), [&] (auto&& e) {
  //  called_in_update = true;
  //  EXPECT_TRUE(is_event<interpreted_events::confirmed_primary_click>(e, to_test));
  //});
  EXPECT_TRUE(called_in_update);

  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_touch_finger_down_event{}), to_test));
  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_time_event{501ms}), to_test));
  EXPECT_FALSE(is_event<interpreted_events::preliminary_primary_click>(
      update_tt(dummy_touch_finger_up_event{}), to_test));
  EXPECT_TRUE(is_event<interpreted_events::context_menu_click>(
      dummy_touch_finger_up_event{}, to_test));
}

TEST(InterpretedEvents, SimpleSwitchCase) // NOLINT
{
  using namespace std::chrono;
  auto to_test = input_event_interpreter();
  auto update_tt = [&to_test](auto &&evt) {
    to_test.update(evt);
    return evt;
  };
  int res{};
  to_test.update(dummy_mouse_down_event{});

  EXPECT_TRUE(ui_event_switch(update_tt(dummy_mouse_up_event{}), to_test,
                              event_case<interpreted_events::preliminary_primary_click>(
                                  [&res](auto const &...) { res = 1; }),
                              event_case<interpreted_events::context_menu_click>(
                                  [&res](auto const &...) { res = 2; })
                              ));
  EXPECT_THAT(res, Eq(1));

  EXPECT_TRUE(ui_event_switch(update_tt(dummy_mouse_up_event{}), to_test, res,
                              event_case<interpreted_events::preliminary_primary_click>(
                                  [](auto const &, auto const&, int& res) { res = 1; }),
                              event_case<interpreted_events::context_menu_click>(
                                  [](auto const &, auto const&, int& res) { res = 2; })
                                  ));
}
*/
} // namespace cgui::tests

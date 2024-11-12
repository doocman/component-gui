
#include <cgui/ui_events.hpp>

#include <type_traits>

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

template <interpreted_events ie_v, typename... Ts>
constexpr interpreted_events to_interpred_event_enum(
    std::type_identity<interpreted_event_impl<ie_v, Ts...>>) {
  return ie_v;
}
template <typename Impl, early_event_tag eet, typename TimePoint>
constexpr interpreted_events to_interpred_event_enum(
    std::type_identity<interpreted_event<Impl, eet, TimePoint>>) {
  return to_interpred_event_enum(std::type_identity<Impl>());
}

template <typename Evt> constexpr interpreted_events to_interpred_event_enum() {
  return to_interpred_event_enum(std::type_identity<Evt>());
}

template <typename Impl, early_event_tag eet, typename TimePoint>
constexpr early_event_tag early_event_status(
    std::type_identity<interpreted_event<Impl, eet, TimePoint>>) {
  return eet;
}

template <typename Evt> constexpr early_event_tag early_event_status() {
  return early_event_status(std::type_identity<Evt>());
}

TEST(GestureEvents, MouseClick) // NOLINT
{
  using namespace std::chrono;
  using time_point_t = steady_clock::time_point;
  auto first_ts = time_point_t{};
  interpreted_events last_event{};
  auto last_confirmed = static_cast<early_event_tag>(-1);
  int event_calls{};
  auto to_test =
      event_interpreter<time_point_t, primary_mouse_click_translator>{};
  auto reset_values = [&] {
    last_confirmed = static_cast<early_event_tag>(-1);
    last_event = {};
    event_calls = {};
  };
  auto test_callback = [&]<any_interpreted_event_c Evt>(Evt const &) {
    ++event_calls;
    last_event = to_interpred_event_enum<Evt>();
    last_confirmed = early_event_status<Evt>();
  };
  auto invoke_tt = [&](auto const &input_evt) {
    reset_values();
    to_test.handle(input_evt, test_callback);
  };
  invoke_tt(default_mouse_down_event{.common_data{first_ts}});
  EXPECT_THAT(event_calls, Eq(0));
  invoke_tt(default_mouse_up_event{.common_data{first_ts}});
  EXPECT_THAT(last_event, Eq(interpreted_events::primary_click));
  EXPECT_THAT(last_confirmed, Eq(early_event_tag::preliminary));
  EXPECT_THAT(event_calls, Eq(1));
  reset_values();
  first_ts += 101ms;
  to_test.pass_time(first_ts, test_callback);
  EXPECT_THAT(event_calls, Eq(0));
  first_ts += 100ms;
  to_test.pass_time(first_ts, test_callback);
  EXPECT_THAT(last_event, Eq(interpreted_events::primary_click));
  EXPECT_THAT(last_confirmed, Eq(early_event_tag::confirmed));
  EXPECT_THAT(event_calls, Eq(1));
  reset_values();
  first_ts += 101ms;
  to_test.pass_time(first_ts, test_callback);
  EXPECT_THAT(event_calls, Eq(0));
}

TEST(GestureEvents, MouseDoubleClick) // NOLINT
{
  FAIL() << "Not yet implemented";
}

} // namespace cgui::tests

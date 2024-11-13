
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
  first_ts += 400ms;
  to_test.pass_time(first_ts, test_callback);
  EXPECT_THAT(last_event, Eq(interpreted_events::primary_click));
  EXPECT_THAT(last_confirmed, Eq(early_event_tag::confirmed));
  EXPECT_THAT(event_calls, Eq(1));
  reset_values();
  first_ts += 501ms;
  to_test.pass_time(first_ts, test_callback);
  EXPECT_THAT(event_calls, Eq(0));
}

struct event_counter {
  std::vector<interpreted_events> event_types;
  std::vector<early_event_tag> event_ee_tags;
  template <typename Evt> constexpr void operator()(Evt const &e) {
    // last_event = to_interpred_event_enum<Evt>();
    // last_confirmed = early_event_status<Evt>();
    event_types.push_back(to_interpred_event_enum<Evt>());
    event_ee_tags.push_back(early_event_status<Evt>());
  }

  void reset() {
    event_types.clear();
    event_ee_tags.clear();
  }
};

TEST(GestureEvents, MouseDoubleClick) // NOLINT
{
  using namespace std::chrono;
  using time_point_t = steady_clock::time_point;
  auto first_ts = time_point_t{};
  auto counter = event_counter();
  auto to_test =
      event_interpreter<time_point_t, primary_mouse_click_translator>{};

  auto invoke_tt = [&](auto const &input_evt) {
    counter.reset();
    to_test.handle(input_evt, counter);
  };
  invoke_tt(default_mouse_down_event{.common_data{first_ts}});
  invoke_tt(default_mouse_up_event{.common_data{first_ts}});
  invoke_tt(default_mouse_up_event{.common_data{first_ts}});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click,
                          interpreted_events::double_primary_click));
  EXPECT_THAT(counter.event_ee_tags, ElementsAre(early_event_tag::cancelled,
                                                 early_event_tag::confirmed));
  first_ts += 5001ms;
  counter.reset();
  to_test.pass_time(first_ts, counter);
  EXPECT_THAT(counter.event_ee_tags, IsEmpty());
  EXPECT_THAT(counter.event_types, IsEmpty());
}

template <typename> struct dummy_event_interpreter {
  struct state {};
  struct settings {};
  template <typename E>
  static constexpr auto can_handle =
      _interpreter_can_handle<input_events::system>::op<E>;

  static constexpr std::optional<state>
  handle(default_event<input_events::system>, auto &&...) {
    return state{};
  }
  static constexpr std::optional<state> pass_time(auto &&...) {
    return std::nullopt;
  }
  static constexpr void cancel_state(auto &&...) {}
};

TEST(GestureEvents, MultiInterpretersQuickconfirm) // NOLINT
{
  using namespace std::chrono;
  using time_point_t = steady_clock::time_point;
  auto first_ts = time_point_t{};
  auto counter = event_counter();
  auto to_test = event_interpreter<time_point_t, primary_mouse_click_translator,
                                   dummy_event_interpreter>{};
  auto invoke_tt = [&](auto const &input_evt) {
    counter.reset();
    to_test.handle(input_evt, counter);
  };
  invoke_tt(default_mouse_down_event{.common_data{first_ts}});
  invoke_tt(default_mouse_up_event{.common_data{first_ts}});
  invoke_tt(default_event<input_events::system>{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click));
  EXPECT_THAT(counter.event_ee_tags, ElementsAre(early_event_tag::confirmed));
}

TEST(GestureEvents, ContextMenuMouseClick) // NOLINT
{
  using namespace std::chrono;
  using time_point_t = steady_clock::time_point;
  auto first_ts = time_point_t{};
  auto counter = event_counter();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = [&](auto const &input_evt) {
    counter.reset();
    to_test.handle(input_evt, counter);
  };
  invoke_tt(default_mouse_down_event{.button_id = mouse_buttons::secondary,
                                     .common_data{first_ts}});
  EXPECT_THAT(counter.event_types, IsEmpty());
  EXPECT_THAT(counter.event_ee_tags, IsEmpty());
  invoke_tt(default_mouse_up_event{.button_id = mouse_buttons::secondary,
                                   .common_data{first_ts}});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::context_menu_click));
  EXPECT_THAT(counter.event_ee_tags, ElementsAre(early_event_tag::confirmed));
}

} // namespace cgui::tests


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

template <interpreted_events ie_v, typename TimePoint>
constexpr interpreted_events to_interpreted_event_enum(
    std::type_identity<interpreted_event<ie_v, TimePoint>>) {
  return ie_v;
}

template <typename Evt>
constexpr interpreted_events to_interpreted_event_enum() {
  return to_interpreted_event_enum(std::type_identity<Evt>());
}

struct event_counter {
  std::vector<interpreted_events> event_types;
  template <typename Evt> constexpr void operator()(Evt const &e) {
    event_types.push_back(to_interpreted_event_enum<Evt>());
  }

  void reset() { event_types.clear(); }
};

using namespace std::chrono;
class GestureEventsTests : public ::testing::Test {
public:
  using time_point_t = steady_clock::time_point;

  struct dummy_widget_query {
    event_counter *counter{};
    std::vector<interpreted_events> events_to_allow{};
    template <typename T, interpreted_events... evts>
    constexpr bool
    operator()(query_interpreted_events_t<T, evts...> const &q) const {
      if (std::ranges::any_of(events_to_allow,
                              [](auto &&e) { return ((e == evts) || ...); })) {
        CGUI_ASSERT(counter != nullptr);
        q(*counter);
        return true;
      }
      return false;
    }
  };

  time_point_t first_ts{};
  event_counter counter{};
  dummy_widget_query event_query;

  auto get_invoke_tt(auto &to_test) {
    event_query.counter = &counter;
    return [this, &to_test](auto const &input_evt) {
      counter.reset();
      to_test.handle(input_evt, event_query);
    };
  }

  void enable_all_events() {
    using enum interpreted_events;
    event_query.events_to_allow.assign({
        primary_click,         //
        context_menu_click,    //
        pointer_drag_start,    //
        pointer_drag_move,     //
        pointer_drag_finished, //
        pointer_hover,         //
        pointer_hold,          //
        pointer_enter,         //
        pointer_exit           //
    });
  };
  void enable_all_events_except(std::same_as<interpreted_events> auto... evts) {
    enable_all_events();
    auto tot_found = (std::erase(event_query.events_to_allow, evts) + ...);
    CGUI_ASSERT(tot_found == sizeof...(evts));
  }
};

TEST_F(GestureEventsTests, AllQueriesFail) // NOLINT
{
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{});
  EXPECT_THAT(counter.event_types, IsEmpty());
  invoke_tt(default_mouse_up_event{});
  EXPECT_THAT(counter.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{});
  EXPECT_THAT(counter.event_types, IsEmpty());
}

TEST_F(GestureEventsTests, MouseClick) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  auto &event_types = counter.event_types;
  invoke_tt(default_mouse_down_event{.common_data{first_ts}});
  EXPECT_THAT(event_types, ElementsAre(interpreted_events::pointer_hold));
  invoke_tt(default_mouse_up_event{.common_data{first_ts}});
  EXPECT_THAT(event_types, ElementsAre(interpreted_events::primary_click));
  counter.reset();
  first_ts += 101ms;
  to_test.pass_time(first_ts, counter);
  EXPECT_THAT(event_types, IsEmpty());
  first_ts += 400ms;
  to_test.pass_time(first_ts, counter);
  EXPECT_THAT(event_types, IsEmpty());
  counter.reset();
  first_ts += 501ms;
  to_test.pass_time(first_ts, counter);
  EXPECT_THAT(event_types, IsEmpty());
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

TEST_F(GestureEventsTests, ContextMenuMouseClick) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{.button_id = mouse_buttons::secondary,
                                     .common_data{first_ts}});
  EXPECT_THAT(counter.event_types, IsEmpty());
  invoke_tt(default_mouse_up_event{.button_id = mouse_buttons::secondary,
                                   .common_data{first_ts}});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::context_menu_click));
}

TEST_F(GestureEventsTests, MouseDrag) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{});
  invoke_tt(default_mouse_move_event{.pos = {20, 20}});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_drag_start));
  invoke_tt(default_mouse_up_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_drag_finished));
}
TEST_F(GestureEventsTests, MouseNoDrag) // NOLINT
{
  enable_all_events_except(interpreted_events::pointer_drag_start);
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{});
  invoke_tt(default_mouse_move_event{.pos = {20, 20}});
  EXPECT_THAT(counter.event_types, IsEmpty());
  invoke_tt(default_mouse_up_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click));
}

TEST_F(GestureEventsTests, MouseHover) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_hover));
}

} // namespace cgui::tests


#include <cgui/ui_events.hpp>

#include <initializer_list>
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
  default_point_rect a_ =
      point_unit(default_rect{{-1000, -1000}, {1000, 1000}});
  event_counter() = default;
  explicit event_counter(default_point_rect a) : a_(a) {}
  template <typename Evt> constexpr void operator()(Evt const &e) {
    event_types.push_back(to_interpreted_event_enum<Evt>());
  }

  constexpr auto area() const { return a_; }

  void reset() { event_types.clear(); }
};

inline std::size_t enable_all_events(std::vector<interpreted_events> &vec) {
  using enum interpreted_events;
  vec.assign({
      primary_click,                     //
      context_menu_click,                //
      pointer_drag_start,                //
      pointer_drag_move,                 //
      pointer_drag_finished_destination, //
      pointer_drag_finished_source,      //
      pointer_hover,                     //
      pointer_hold,                      //
      pointer_enter,                     //
      pointer_exit,                      //
      scroll,                            //
      zoom                               //
  });
  return size(vec);
}
inline void
enable_all_events(std::same_as<std::vector<interpreted_events>> auto &...vs)
  requires(sizeof...(vs) > 1)
{
  unused(enable_all_events(vs)...);
}
inline void
disable_events(interpreted_events evt,
               std::same_as<std::vector<interpreted_events>> auto &...vs) {
  unused(std::erase(vs, evt)...);
}
inline void
disable_events(std::initializer_list<interpreted_events> evts,
               std::same_as<std::vector<interpreted_events>> auto &...vs) {
  for (auto ie : evts) {
    disable_events(ie, vs...);
  }
}
inline void enable_all_events_except(
    interpreted_events evt,
    std::same_as<std::vector<interpreted_events>> auto &...vs) {
  enable_all_events(vs...);
  disable_events(evt, vs...);
}
inline void enable_all_events_except(
    std::initializer_list<interpreted_events> evts,
    std::same_as<std::vector<interpreted_events>> auto &...vs) {
  enable_all_events(vs...);
  disable_events(evts, vs...);
}

constexpr auto direct_invoke = [](auto &f, auto &&...args) {
  return f(std::forward<decltype(args)>(args)...);
};

using namespace std::chrono;
class GestureEventsTests : public ::testing::Test {
public:
  using time_point_t = steady_clock::time_point;

  struct dummy_widget_query {
    event_counter *counter{};
    std::vector<interpreted_events> events_to_allow{};
    template <typename P, typename T, interpreted_events... evts>
    constexpr bool
    operator()(query_interpreted_events_t<P, T, evts...> const &q) const {
      if (std::ranges::any_of(events_to_allow,
                              [](auto &&e) { return ((e == evts) || ...); })) {
        CGUI_ASSERT(counter != nullptr);
        q(*counter, direct_invoke);
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

  static void
  enable_all_events(std::convertible_to<dummy_widget_query> auto &...qs) {
    constexpr auto do_assign = [](dummy_widget_query &q) {
      tests::enable_all_events(q.events_to_allow);
      return 0;
    };
    unused(do_assign(qs)...);
  }
  void enable_all_events() { enable_all_events(event_query); };
  static constexpr void
  disable_event(interpreted_events evt,
                std::same_as<dummy_widget_query> auto &...qs) {
    unused(std::erase(qs.events_to_allow, evt)...);
  }
  static void
  enable_all_events_except(std::initializer_list<interpreted_events> evts,
                           std::same_as<dummy_widget_query> auto &...qs) {
    enable_all_events(qs...);
    for (auto &e : evts) {
      disable_event(e, qs...);
    }
  }
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
  EXPECT_THAT(event_types, ElementsAre(interpreted_events::primary_click,
                                       interpreted_events::pointer_hover));
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
              ElementsAre(interpreted_events::pointer_drag_start,
                          interpreted_events::pointer_drag_move));
  invoke_tt(default_mouse_up_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_drag_finished_source,
                          interpreted_events::pointer_drag_finished_destination,
                          interpreted_events::pointer_hover));
}
TEST_F(GestureEventsTests, MouseNoDrag) // NOLINT
{
  enable_all_events_except(interpreted_events::pointer_drag_start,
                           interpreted_events::pointer_drag_move);
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{});
  invoke_tt(default_mouse_move_event{.pos = {20, 20}});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_hold));
  invoke_tt(default_mouse_up_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click,
                          interpreted_events::pointer_hover));
}

TEST_F(GestureEventsTests, MouseHover) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_enter,
                          interpreted_events::pointer_hover));
}

TEST_F(GestureEventsTests, MouseScroll) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
}

TEST_F(GestureEventsTests, MouseScrollToZoomLCtrl) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  invoke_tt(default_key_down_event(keycode::lctrl));
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
}

TEST_F(GestureEventsTests, MouseScrollToZoomRCtrl) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  invoke_tt(default_key_down_event(keycode::rctrl));
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
}

TEST_F(GestureEventsTests, MouseScrollLiftLCtrlScrolls) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  invoke_tt(default_key_down_event(keycode::lctrl));
  invoke_tt(default_key_up_event(keycode::lctrl));
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
}

TEST_F(GestureEventsTests, MouseScrollLiftRLCtrlScrolls) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  invoke_tt(default_key_down_event(keycode::lctrl));
  invoke_tt(default_key_down_event(keycode::rctrl));
  invoke_tt(default_key_up_event(keycode::lctrl));
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
  invoke_tt(default_key_down_event(keycode::lctrl));
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
  invoke_tt(default_key_up_event(keycode::rctrl));
  invoke_tt(default_key_up_event(keycode::lctrl));
  invoke_tt(default_mouse_scroll_event{});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
}

struct GestureEventsHitTests : public ::testing::Test {
  using time_point_t = steady_clock::time_point;
  struct mock_widget {
    std::vector<interpreted_events> allowed_events;
    event_counter counter{};

    explicit mock_widget(default_point_rect a) : counter(a) {}
  };
  struct widget_query {
    std::vector<mock_widget> widgets;
    template <typename P, typename T, interpreted_events... evts>
    constexpr bool
    operator()(query_interpreted_events_t<P, T, evts...> const &q) {
      for (auto &w : widgets) {
        if (std::ranges::any_of(
                w.allowed_events,
                [](auto &&e) { return ((e == evts) || ...); }) &&
            q(w.counter, direct_invoke)) {
          return true;
        }
      }
      return false;
    }
    void reset_all() {
      for (auto &w : widgets) {
        w.counter.reset();
      }
    }
  };
  widget_query query;
  void add_widget(
      default_rect area,
      std::function<void(std::vector<interpreted_events> &)> const
          &on_creation = [](auto &v) { enable_all_events(v); }) {
    auto &new_w = query.widgets.emplace_back(point_unit(area));
    if (on_creation) {
      on_creation(new_w.allowed_events);
    }
  }

  auto get_invoke_tt(auto &to_test) {
    return [this, &to_test](auto const &input_evt) {
      query.reset_all();
      to_test.handle(input_evt, query);
    };
  }
};

TEST_F(GestureEventsHitTests, MouseEnterExit) // NOLINT
{
  auto to_test = default_event_interpreter<time_point_t>{};
  add_widget({{0, 0}, {50, 50}});
  add_widget({{50, 0}, {100, 50}});
  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{});
  using enum interpreted_events;
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_enter, pointer_hover));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {25, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_hover));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {50, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_exit));
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_enter, pointer_hover));
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(cl.event_types, IsEmpty());
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_hover));
}

TEST_F(GestureEventsHitTests, DragDelayEnterExit) // NOLINT
{
  auto to_test = default_event_interpreter<time_point_t>{};
  add_widget({{0, 0}, {50, 50}});
  add_widget({{50, 0}, {100, 50}});
  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{.pos = {0, 0}});
  using enum interpreted_events;
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_hold));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {25, 0}});
  EXPECT_THAT(cl.event_types,
              ElementsAre(pointer_drag_start, pointer_drag_move));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_drag_move));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_up_event{.pos = {75, 0}});
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_drag_finished_destination,
                                          pointer_enter, pointer_hover));
  EXPECT_THAT(cl.event_types,
              ElementsAre(pointer_drag_finished_source, pointer_exit));
}

TEST_F(GestureEventsHitTests, MouseDownNoDragEnterExit) // NOLINT
{
  auto to_test = default_event_interpreter<time_point_t>{};
  auto const event_enabler = [](auto &v) {
    enable_all_events_except({interpreted_events::pointer_drag_start,
                              interpreted_events::pointer_drag_move},
                             v);
  };
  add_widget({{0, 0}, {50, 50}}, event_enabler);
  add_widget({{50, 0}, {100, 50}}, event_enabler);
  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{.pos = {0, 0}});
  using enum interpreted_events;
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_hold));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {25, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_hold));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_exit));
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_enter, pointer_hold));
  invoke_tt(default_mouse_up_event{.pos = {75, 0}});
  EXPECT_THAT(cr.event_types, ElementsAre(primary_click, pointer_hover));
  EXPECT_THAT(cl.event_types, IsEmpty());
}

TEST_F(GestureEventsHitTests, MouseDownNoDragEnterExitFar) // NOLINT
{
  auto to_test = default_event_interpreter<time_point_t>{};
  auto const event_enabler = [](auto &v) {
    enable_all_events_except({interpreted_events::pointer_drag_start,
                              interpreted_events::pointer_drag_move},
                             v);
  };
  add_widget({{0, 0}, {50, 50}}, event_enabler);
  add_widget({{50, 0}, {100, 50}}, event_enabler);
  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{.pos = {0, 0}});
  using enum interpreted_events;
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_hold));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_exit));
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_enter, pointer_hold));
  invoke_tt(default_mouse_up_event{.pos = {75, 0}});
  EXPECT_THAT(cr.event_types, ElementsAre(primary_click, pointer_hover));
  EXPECT_THAT(cl.event_types, IsEmpty());
}

} // namespace cgui::tests

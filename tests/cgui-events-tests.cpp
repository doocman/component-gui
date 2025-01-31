
#include <cgui/ui_events.hpp>

#include <initializer_list>
#include <type_traits>

#include <gmock/gmock.h>

#include <cgui_test_utils.hpp>

CGUI_WARNINGS_PUSH
CGUI_SUPPRESSW_MSVC(4244)

namespace cgui::tests {
using namespace ::testing;

static_assert(
    has_event_type_of_type<interpreted_event<interpreted_events::primary_click>,
                           interpreted_events>);
static_assert(
    can_be_event<interpreted_events::primary_click,
                 interpreted_event<interpreted_events::primary_click>>());
static_assert(interpreted_event_types<
              interpreted_event<interpreted_events::primary_click>,
              interpreted_events::primary_click>);

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
  static widget_id_t next_id() {
    static widget_id_t next{0};
    auto res = next;
    ++next.value;
    return res;
  }
  std::vector<interpreted_events> event_types;
  default_point_rect a_ =
      point_unit(default_rect{{-1000, -1000}, {1000, 1000}});
  zoom_factor_t zf{1.f, 1.f};
  widget_id_t id_ = next_id();
  event_counter() = default;
  explicit event_counter(default_point_rect a) : a_(a) {}
  template <typename Evt> constexpr void handle(Evt const &e) {
    event_types.push_back(to_interpreted_event_enum<Evt>());
  }

  constexpr auto area() const { return a_; }

  void reset() { event_types.clear(); }
  constexpr zoom_factor_t const &zoom_factor() const { return zf; }
  constexpr widget_id_t widget_id() const noexcept { return id_; }

  template <typename T> struct with_cb_t {
    T &cb_;
    event_counter &ec_;
    constexpr void handle(auto const &e) const {
      cb_(e);
      ec_.handle(e);
    }
    constexpr auto area() const { return ec_.area(); }
    constexpr auto zoom_factor() const { return ec_.zoom_factor(); }
    constexpr auto widget_id() const { return ec_.widget_id(); }
  };
  template <typename T> constexpr auto with_cb(T &cb) {
    return with_cb_t<T>(cb, *this);
  }
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
    zoom_factor_t zf_{1.f, 1.f};
    template <typename QM, interpreted_events... evts>
    constexpr bool
    operator()(query_interpreted_events_t<QM, evts...> const &q) const {
      if (std::ranges::any_of(events_to_allow,
                              [](auto &&e) { return ((e == evts) || ...); })) {
        CGUI_ASSERT(counter != nullptr);
        q(*counter);
        return true;
      }
      return false;
    }

    template <typename CB = bp::no_op_t>
    constexpr auto get_query(CB &&cb = {}) {
      return [this, cb]<typename QM, interpreted_events... evts>(
                 query_interpreted_events_t<QM, evts...> const &q) {
        if (std::ranges::any_of(events_to_allow, [](auto &&e) {
              return ((e == evts) || ...);
            })) {
          CGUI_ASSERT(counter != nullptr);
          q(counter->with_cb(cb)
            //      , [cb](auto &dw, auto &&e) {
            //  cb(e);
            //  dw(e);
            //}
          );
          return true;
        }
        return false;
      };
    }
    constexpr zoom_factor_t const &zoom_factor() const { return zf_; }
  };

  time_point_t first_ts{};
  event_counter counter{};
  dummy_widget_query event_query;

  auto get_invoke_tt(auto &to_test, auto &&cb) {
    event_query.counter = &counter;
    return [this, &to_test, cb](auto const &input_evt) {
      counter.reset();
      to_test.handle(input_evt, event_query.get_query(cb));
    };
  }
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
  float scale_x{};
  float scale_y{};
  settings<primary_mouse_click_translator>(to_test).zoom_scale = 0.1f;
  auto invoke_tt = get_invoke_tt(to_test, [&]<typename T>(T const &e) {
    if constexpr (can_be_event<interpreted_events::zoom, T>()) {
      scale_x = call::scale_x(e);
      scale_y = call::scale_y(e);
    }
  });
  invoke_tt(default_mouse_move_event{});
  invoke_tt(default_key_down_event(keycode::rctrl));
  invoke_tt(default_mouse_scroll_event{.dy = 1.f});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
  EXPECT_THAT(scale_x, FloatEq(1.1f));
  EXPECT_THAT(scale_y, FloatEq(1.1f));
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

TEST_F(GestureEventsTests, TouchClick) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_touch_down_event());
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_enter,
                          interpreted_events::pointer_hold));
  invoke_tt(default_touch_up_event());
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click,
                          interpreted_events::pointer_exit));
}

TEST_F(GestureEventsTests, TouchZoom) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_touch_down_event{.pos = {10, 0}, .finger_index = 0});
  invoke_tt(default_touch_down_event{.pos = {20, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {8, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {22, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {6, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {24, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {4, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {26, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {2, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {28, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
}

TEST_F(GestureEventsTests, TouchPan) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  settings<touch_translator>(to_test).scroll_threshold = 2;
  settings<touch_translator>(to_test).drag_threshold = 2;
  invoke_tt(default_touch_down_event{.pos = {10, 0}, .finger_index = 0});
  invoke_tt(default_touch_down_event{.pos = {20, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {12, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, IsEmpty());
  invoke_tt(default_touch_move_event{.pos = {22, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {14, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
  invoke_tt(default_touch_move_event{.pos = {24, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {16, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {26, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {18, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {28, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));

  invoke_tt(default_touch_up_event{.finger_index = 0});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_exit));

  invoke_tt(default_touch_down_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_enter,
                          interpreted_events::pointer_hold));
  invoke_tt(default_touch_up_event{});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click,
                          interpreted_events::pointer_exit));

  invoke_tt(default_touch_up_event{.finger_index = 1});
  invoke_tt(default_touch_down_event{.finger_index = 1});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_enter,
                          interpreted_events::pointer_hold));
  invoke_tt(default_touch_up_event{.finger_index = 1});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::primary_click,
                          interpreted_events::pointer_exit));
}

TEST_F(GestureEventsTests, TouchPanAndZoom) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_touch_down_event{.pos = {10, 0}, .finger_index = 0});
  invoke_tt(default_touch_down_event{.pos = {20, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {11, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {23, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {12, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {26, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {13, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {29, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {14, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {32, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types,
              UnorderedElementsAre(interpreted_events::scroll,
                                   interpreted_events::zoom));
}

TEST_F(GestureEventsTests, TouchPanLevels) {
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  int last_scroll_x{};
  int total_scroll_x{};
  int last_scroll_y{};
  int total_scroll_y{};
  static_assert(can_be_event<
                interpreted_events::scroll,
                interpreted_event<interpreted_events::scroll, time_point_t>>());
  auto invoke_tt = get_invoke_tt(to_test, [&]<typename E>(E const &e) {
    if constexpr (can_be_event<interpreted_events::scroll, E>()) {
      last_scroll_x = call::delta_x(e);
      last_scroll_y = call::delta_y(e);
      total_scroll_x += call::delta_x(e);
      total_scroll_y += call::delta_y(e);
    }
  });
  invoke_tt(default_touch_down_event{.pos = {10, 10}, .finger_index = 0});
  invoke_tt(default_touch_down_event{.pos = {20, 20}, .finger_index = 1});
  int last_gen_x = 0;
  int last_gen_y = 0;
  auto generate_pan = [&](int px, int py) {
    auto end_x = last_gen_x + px;
    auto end_y = last_gen_y + py;
    auto end_point = std::max(end_x, end_y) + 1;
    for (int i = std::min(last_gen_x, last_gen_y) + 1; i < end_point; ++i) {
      CGUI_ASSERT(last_gen_x <= end_x);
      CGUI_ASSERT(last_gen_y <= end_y);
      last_gen_x = std::clamp(i, last_gen_x, end_x);
      last_gen_y = std::clamp(i, last_gen_y, end_y);
      invoke_tt(default_touch_move_event{
          .pos = {10 + last_gen_x, 10 + last_gen_y}, .finger_index = 0});
      invoke_tt(default_touch_move_event{
          .pos = {20 + last_gen_x, 20 + last_gen_y}, .finger_index = 1});
    }
  };
  generate_pan(1, 0);
  EXPECT_THAT(counter.event_types, ElementsAre());
  generate_pan(3, 0);
  EXPECT_THAT(counter.event_types, ElementsAre());
  generate_pan(2, 0);
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
  EXPECT_THAT(total_scroll_x, Eq(6));
  EXPECT_THAT(total_scroll_y, Eq(0));
  EXPECT_THAT(last_scroll_y, Eq(0));
  generate_pan(2, 2);
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
  EXPECT_THAT(total_scroll_x, Eq(8));
  EXPECT_THAT(total_scroll_y, Eq(2));

  invoke_tt(default_touch_up_event{.pos = {10 + last_gen_x, 10 + last_gen_y},
                                   .finger_index = 0});
  EXPECT_THAT(counter.event_types,
              ElementsAre(interpreted_events::pointer_exit));
  invoke_tt(default_touch_up_event{.pos = {20 + last_gen_x, 20 + last_gen_y},
                                   .finger_index = 1});
  EXPECT_THAT(counter.event_types, IsEmpty());

  // Must incorporate touch up and touch down again and check for any
  // exponential 'exploding' behaviours.
}

TEST_F(GestureEventsTests, PanThenZoom) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  settings<touch_translator>(to_test).scroll_threshold = 2;
  settings<touch_translator>(to_test).drag_threshold = 2;
  settings<touch_translator>(to_test).zoom_threshold = 5;
  auto invoke_tt = get_invoke_tt(to_test);

  invoke_tt(default_touch_down_event{.pos = {10, 0}, .finger_index = 0});
  invoke_tt(default_touch_down_event{.pos = {20, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {12, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {22, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {14, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
  invoke_tt(default_touch_move_event{.pos = {21, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::scroll));
  invoke_tt(default_touch_move_event{.pos = {16, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types,
              UnorderedElementsAre(interpreted_events::scroll,
                                   interpreted_events::zoom));
  invoke_tt(default_touch_move_event{.pos = {20, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types,
              UnorderedElementsAre(interpreted_events::scroll,
                                   interpreted_events::zoom));
  invoke_tt(default_touch_move_event{.pos = {18, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types,
              UnorderedElementsAre(interpreted_events::scroll,
                                   interpreted_events::zoom));
}

TEST_F(GestureEventsTests, ZoomThenPan) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  settings<touch_translator>(to_test).scroll_threshold = 3;
  settings<touch_translator>(to_test).drag_threshold = 3;
  settings<touch_translator>(to_test).zoom_threshold = 3;
  auto invoke_tt = get_invoke_tt(to_test);

  invoke_tt(default_touch_down_event{.pos = {10, 0}, .finger_index = 0});
  invoke_tt(default_touch_down_event{.pos = {20, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {11, 0}, .finger_index = 0});
  invoke_tt(default_touch_move_event{.pos = {19, 0}, .finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {12, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
  invoke_tt(default_touch_move_event{.pos = {21, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
  invoke_tt(default_touch_move_event{.pos = {15, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, ElementsAre(interpreted_events::zoom));
  invoke_tt(default_touch_move_event{.pos = {23, 0}, .finger_index = 1});
  EXPECT_THAT(counter.event_types,
              UnorderedElementsAre(interpreted_events::scroll,
                                   interpreted_events::zoom));
  invoke_tt(default_touch_move_event{.pos = {18, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types,
              UnorderedElementsAre(interpreted_events::scroll,
                                   interpreted_events::zoom));
}
constexpr void apply_touch_zoom_x(auto &&handler, int start_distance,
                                  int end_distance, int start_x = 0) {
  auto distance_diff = (end_distance - start_distance) / 2;
  auto sign_i = 1;
  if (distance_diff < 0) {
    sign_i = -1;
    distance_diff = -distance_diff;
  }
  CGUI_ASSERT(distance_diff >= 0);
  handler(default_touch_down_event{.pos = {start_x, 0}, .finger_index = 0});
  handler(default_touch_down_event{.pos = {start_x + start_distance, 0},
                                   .finger_index = 1});
  for (auto i = 1; i <= distance_diff; ++i) {
    auto xl = start_x - i * sign_i;
    auto xr = start_x + start_distance + i * sign_i;
    handler(default_touch_move_event{.pos = {xl, 0}, .finger_index = 0});
    handler(default_touch_move_event{.pos = {xr, 0}, .finger_index = 1});
  }
}
constexpr void apply_touch_pan_x(auto &&handler, int distance,
                                 int start_x = 0) {
  int sign = 1;
  if (distance < 0) {
    sign = -1;
    distance = -distance;
  }
  constexpr int fdist = 10;
  handler(
      default_touch_down_event{.pos = {start_x - fdist, 0}, .finger_index = 0});
  handler(
      default_touch_down_event{.pos = {start_x + fdist, 0}, .finger_index = 1});
  for (auto i = 1; i <= distance; ++i) {
    auto xl = start_x - fdist + i * sign;
    auto xr = start_x + fdist + i * sign;
    handler(default_touch_move_event{.pos = {xl, 0}, .finger_index = 0});
    handler(default_touch_move_event{.pos = {xr, 0}, .finger_index = 1});
  }
}
constexpr void apply_touch_pan_zoom_x(auto &&handler, int pan_distance,
                                      int start_distance, int end_distance,
                                      int start_x = 0) {
  auto distance_diff = (end_distance - start_distance) / 2;
  auto sign_z = 1;
  auto sign_p = 1;
  if (distance_diff < 0) {
    sign_z = -1;
    distance_diff = -distance_diff;
  }
  if (pan_distance < 0) {
    sign_p = -1;
    pan_distance = -pan_distance;
  }
  CGUI_ASSERT(distance_diff >= 0);
  handler(default_touch_down_event{.pos = {start_x, 0}, .finger_index = 0});
  handler(default_touch_down_event{.pos = {start_x + start_distance, 0},
                                   .finger_index = 1});
  for (auto i = 1; i <= distance_diff; ++i) {
    auto zoom_v = std::min(i, distance_diff) * sign_z;
    auto pan_v = std::min(i, pan_distance) * sign_p + start_x;
    auto xl = pan_v - zoom_v;
    auto xr = pan_v + start_distance + zoom_v;
    handler(default_touch_move_event{.pos = {xl, 0}, .finger_index = 0});
    handler(default_touch_move_event{.pos = {xr, 0}, .finger_index = 1});
  }
}
TEST_F(GestureEventsTests, TouchZoomLevels) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  float scale = 1.f;
  int zoom_count{};
  auto invoke_tt = get_invoke_tt(to_test, [&]<typename T>(T const &event) {
    if constexpr (can_be_event<interpreted_events::zoom, T>()) {
      scale = call::scale_x(event);
      ++zoom_count;
    }
    return true;
  });
  apply_touch_zoom_x(invoke_tt, 20, 40);
  EXPECT_THAT(scale, FloatEq(2.f));
  EXPECT_THAT(zoom_count, Gt(0));
}

TEST_F(GestureEventsTests, NoDragAfterTouchPan) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  float scale = 1.f;
  int zoom_count{};
  auto invoke_tt = get_invoke_tt(to_test);
  apply_touch_pan_x(invoke_tt, 20);
  invoke_tt(default_touch_up_event{.finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {40, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, IsEmpty());
}

TEST_F(GestureEventsTests, NoDragAfterTouchZoom) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  float scale = 1.f;
  int zoom_count{};
  auto invoke_tt = get_invoke_tt(to_test);
  apply_touch_zoom_x(invoke_tt, 20, 40);
  invoke_tt(default_touch_up_event{.finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {40, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, IsEmpty());
}

TEST_F(GestureEventsTests, NoDragAfterTouchPanZoom) // NOLINT
{
  enable_all_events();
  auto to_test = default_event_interpreter<time_point_t>{};
  float scale = 1.f;
  int zoom_count{};
  auto invoke_tt = get_invoke_tt(to_test);
  apply_touch_pan_zoom_x(invoke_tt, 20, 20, 40);
  invoke_tt(default_touch_up_event{.finger_index = 1});
  invoke_tt(default_touch_move_event{.pos = {40, 0}, .finger_index = 0});
  EXPECT_THAT(counter.event_types, IsEmpty());
}

// TODO:
// Scroll/ZoomOnParentWithChild //< First touch down reaches a child, then the
// zoom should only be registered by the parent, in which a pointer_exit should
// be called to the child.

struct GestureEventsHitTests : public ::testing::Test {
  using time_point_t = steady_clock::time_point;
  struct mock_widget {
    std::vector<interpreted_events> allowed_events;
    event_counter counter{};

    explicit mock_widget(default_point_rect a) : counter(a) {}
  };
  struct widget_query {
    std::vector<mock_widget> widgets;
    template <typename QM, interpreted_events... evts>
    constexpr bool
    operator()(query_interpreted_events_t<QM, evts...> const &q) {
      for (auto &w : widgets) {
        if (std::ranges::any_of(
                w.allowed_events,
                [](auto &&e) { return ((e == evts) || ...); }) &&
            q(w.counter)) {
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
  auto get_invoke_tt_no_clear(auto &to_test) {
    return [this, &to_test](auto const &input_evt) {
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

TEST_F(GestureEventsHitTests, MoveMouseOutsideWidgetDrag) // NOLINT
{
  add_widget({{0, 0}, {50, 50}});
  auto &w = query.widgets[0].counter;
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  using enum interpreted_events;
  invoke_tt(default_mouse_move_event{.pos = {0, 0}});
  invoke_tt(default_mouse_move_event{.pos = {-1, 0}});
  EXPECT_THAT(w.event_types, ElementsAre(pointer_exit));
  invoke_tt(default_mouse_move_event{.pos = {0, 0}});
  invoke_tt(default_mouse_down_event{.pos = {-1, 0}});
  // EXPECT_THAT(w.event_types, ElementsAre(pointer_exit));  // DISABLED:
  // robustness test that we currently hope to not need.
  // Possible implementation solution to fix this problem if it arise: store the
  // last position of an event. if a non-move event occurs with a different
  // position, handle a 'dummy move event' first with the new position and then
  // run the proper handling.
  invoke_tt(default_mouse_move_event{.pos = {0, 0}});
  invoke_tt(default_mouse_up_event{.pos = {-1, 0}});
  // EXPECT_THAT(w.event_types, ElementsAre(pointer_exit)); // DISABLED:
  // robustness test that we currently hope to not need.
  invoke_tt(default_mouse_move_event{.pos = {0, 0}});
  invoke_tt(default_mouse_down_event{.pos = {0, 0}});
  invoke_tt(default_mouse_move_event{.pos = {25, 0}});
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(w.event_types, ElementsAre(pointer_drag_move));
  invoke_tt(default_mouse_up_event{.pos = {75, 0}});
  EXPECT_THAT(w.event_types,
              ElementsAre(pointer_drag_finished_source, pointer_exit));
}

TEST_F(GestureEventsHitTests, MoveMouseOutsideWidgetNoDrag) // NOLINT
{
  using enum interpreted_events;
  add_widget({{0, 0}, {50, 50}}, [](auto &v) {
    enable_all_events_except({pointer_drag_start, pointer_drag_move}, v);
  });
  auto &w = query.widgets[0].counter;
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_move_event{.pos = {0, 0}});
  invoke_tt(default_mouse_down_event{.pos = {0, 0}});
  invoke_tt(default_mouse_move_event{.pos = {25, 0}});
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(w.event_types, ElementsAre(pointer_exit));
  invoke_tt(default_mouse_up_event{.pos = {75, 0}});
  EXPECT_THAT(w.event_types, IsEmpty());
}

TEST_F(GestureEventsHitTests, MouseHoldMultipleWidgets) // NOLINT
{
  using enum interpreted_events;
  add_widget({{0, 0}, {50, 50}}, [](auto &v) {
    enable_all_events_except({pointer_drag_start, pointer_drag_move}, v);
  });
  add_widget({{50, 0}, {100, 50}}, [](auto &v) {
    enable_all_events_except({pointer_drag_start, pointer_drag_move}, v);
  });

  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_mouse_down_event{.pos = {0, 0}});
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(pointer_exit));
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_enter, pointer_hold));
  invoke_tt(default_mouse_move_event{.pos = {75, 75}});
  EXPECT_THAT(cl.event_types, ElementsAre());
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_exit));
  invoke_tt(default_mouse_up_event{.pos = {75, 75}});
  EXPECT_THAT(cl.event_types, ElementsAre());
  EXPECT_THAT(cr.event_types, ElementsAre());
  invoke_tt(default_mouse_move_event{.pos = {75, 0}});
  EXPECT_THAT(cr.event_types, ElementsAre(pointer_enter, pointer_hover));
}

TEST_F(GestureEventsHitTests, TouchDrag) // NOLINT
{
  add_widget({{0, 0}, {50, 50}});
  add_widget({{50, 0}, {100, 50}});

  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_touch_down_event());
  invoke_tt(default_touch_move_event{.pos = {2, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(interpreted_events::pointer_hold));
  invoke_tt(default_touch_move_event{.pos = {20, 0}});
  EXPECT_THAT(cl.event_types,
              ElementsAre(interpreted_events::pointer_drag_start,
                          interpreted_events::pointer_drag_move));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_touch_move_event{.pos = {40, 0}});
  EXPECT_THAT(cl.event_types,
              ElementsAre(interpreted_events::pointer_drag_move));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_touch_move_event{.pos = {60, 0}});
  EXPECT_THAT(cl.event_types,
              ElementsAre(interpreted_events::pointer_drag_move));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_touch_up_event{.pos = {60, 0}});
  EXPECT_THAT(
      cr.event_types,
      ElementsAre(interpreted_events::pointer_drag_finished_destination));
  EXPECT_THAT(cl.event_types,
              ElementsAre(interpreted_events::pointer_drag_finished_source,
                          interpreted_events::pointer_exit));
}

TEST_F(GestureEventsHitTests, TouchMoveNoDrag) // NOLINT
{
  auto constexpr event_setter = [](auto &v) {
    enable_all_events_except({interpreted_events::pointer_drag_start,
                              interpreted_events::pointer_drag_move},
                             v);
  };
  add_widget({{0, 0}, {50, 50}}, event_setter);
  add_widget({{50, 0}, {100, 50}}, event_setter);

  auto &cl = query.widgets[0].counter;
  auto &cr = query.widgets[1].counter;
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt(to_test);
  invoke_tt(default_touch_down_event());
  invoke_tt(default_touch_move_event{.pos = {2, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(interpreted_events::pointer_hold));
  invoke_tt(default_touch_move_event{.pos = {20, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(interpreted_events::pointer_hold));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_touch_move_event{.pos = {40, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(interpreted_events::pointer_hold));
  EXPECT_THAT(cr.event_types, IsEmpty());
  invoke_tt(default_touch_move_event{.pos = {60, 0}});
  EXPECT_THAT(cl.event_types, ElementsAre(interpreted_events::pointer_exit));
  EXPECT_THAT(cr.event_types, ElementsAre(interpreted_events::pointer_enter,
                                          interpreted_events::pointer_hold));
  invoke_tt(default_touch_up_event{.pos = {60, 0}});
  EXPECT_THAT(cr.event_types, ElementsAre(interpreted_events::primary_click,
                                          interpreted_events::pointer_exit));
  EXPECT_THAT(cl.event_types, IsEmpty());
}

TEST_F(GestureEventsHitTests, TouchParentPanCallsExitOnChild) // NOLINT
{
  // Add child first
  add_widget({{0, 0}, {100, 100}}, [](auto &v) {
    enable_all_events_except(
        {interpreted_events::zoom, interpreted_events::scroll}, v);
  });
  add_widget({{0, 0}, {100, 100}});
  auto &child = query.widgets[0].counter;
  auto &parent = query.widgets[1].counter;
  auto to_test = default_event_interpreter<time_point_t>{};
  auto invoke_tt = get_invoke_tt_no_clear(to_test);
  apply_touch_pan_x(invoke_tt, 20, 40);
  EXPECT_THAT(child.event_types, Contains(interpreted_events::pointer_exit));
}

} // namespace cgui::tests

CGUI_WARNINGS_POP

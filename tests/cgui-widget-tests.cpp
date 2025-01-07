
#include <cgui/cgui.hpp>

#include <gmock/gmock.h>

#include <cgui_test_utils.hpp>

namespace cgui::tests {

using namespace ::testing;

struct mock_renderable {
  MOCK_METHOD(void, do_render, (), (const));

  void render(auto &&...) const { do_render(); }
};

TEST(WidgetBuilder, BuildRender) // NOLINT
{
  auto renderable = mock_renderable();
  EXPECT_CALL(renderable, do_render());
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               .display(std::ref(renderable))
               .build();
  call::render(w, dummy_renderer{});
}

TEST(SubFind, FindInTuple) // NOLINT
{
  auto v = std::tuple(1, 2, 3, 3);
  int const *res = nullptr;
  int calls{};
  std::size_t last_index = -1;
  auto cb = [&res, &calls, &last_index](int const &i, std::size_t index) {
    res = &i;
    last_index = index;
    ++calls;
  };
  auto found = call::find_sub(v, equals(2), cb);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&std::get<1>(v)));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(1));

  auto reset = [&] {
    res = nullptr;
    calls = 0;
    last_index = -1;
  };
  reset();
  found = call::find_sub(v, equals(3), cb);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&std::get<2>(v)));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(2));

  reset();
  found = call::find_sub(v, equals(4), cb);
  EXPECT_THAT(found, IsFalse());
  EXPECT_THAT(res, IsNull());
  EXPECT_THAT(calls, Eq(0));
  EXPECT_THAT(last_index, Eq(std::size_t{highest_possible}));
}

struct dummy_for_eachable {
  int i1, i2, i3, i4;

  constexpr void for_each(auto &&cb) {
    cb(i1, 0);
    cb(i2, 1);
    cb(i3, 2);
    cb(i4, 3);
  }
};

TEST(SubFind, FindInForEach) // NOLINT
{
  auto v = dummy_for_eachable{1, 2, 3, 3};
  int const *res = nullptr;
  int calls{};
  std::size_t last_index = -1;
  auto cb = [&res, &calls, &last_index](int const &i, std::size_t index) {
    res = &i;
    last_index = index;
    ++calls;
  };
  auto found = call::find_sub(v, equals(2), cb);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&v.i2));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(1));

  auto reset = [&] {
    res = nullptr;
    calls = 0;
    last_index = highest_possible;
  };
  reset();
  found = call::find_sub(v, equals(3), cb);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&v.i3));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(2));

  reset();
  found = call::find_sub(v, equals(4), cb);
  EXPECT_THAT(found, IsFalse());
  EXPECT_THAT(res, IsNull());
  EXPECT_THAT(calls, Eq(0));
  EXPECT_THAT(last_index, Eq(std::size_t{highest_possible}));
}

TEST(WidgetBuilder, SetColour) // NOLINT
{
  auto m1 = mock_colourable{};
  auto m2 = mock_colourable{};
  InSequence s;
  default_colour_t m1c{}, m2c{};
  EXPECT_CALL(m1, colour(_)).WillOnce([&](default_colour_t const &c) {
    m1c = c;
  });
  EXPECT_CALL(m2, colour(_)).WillOnce([&](default_colour_t const &c) {
    m2c = c;
  });
  EXPECT_CALL(m1, do_render()).Times(1);
  EXPECT_CALL(m2, do_render()).Times(1);
  using namespace dooc::tuple_literals;

  auto w = widget_builder()
               .area(default_rect{})
               .display("text"_na = std::ref(m1), "fill"_na = std::ref(m2))
               .build();
  auto constexpr exp_m1c = default_colour_t{255, 0, 0, 255};
  auto constexpr exp_m2c = default_colour_t{0, 255, 0, 255};
  "text"_from(w.displays()).colour(exp_m1c);
  "fill"_from(w.displays()).colour(exp_m2c);
  expect_colour_eq(m1c, exp_m1c);
  expect_colour_eq(m2c, exp_m2c);
  w.render(dummy_renderer{});
}

TEST(WidgetBuilder, BuildWithState) // NOLINT
{
  auto state_aware_rend = mock_state_aware_renderer{};
  MockFunction<void()> checkpoint{};
  InSequence s;
  EXPECT_CALL(state_aware_rend, do_render(Eq(0)));
  EXPECT_CALL(state_aware_rend, do_set_state(Eq(1)));
  EXPECT_CALL(checkpoint, Call());
  EXPECT_CALL(state_aware_rend, do_render(Eq(1)));
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               //.state(int_states{})
               .event(int_as_event_handler{})
               .display(std::ref(state_aware_rend))
               .build();
  w.render(dummy_renderer{});
  w.handle(1);
  checkpoint.Call();
  w.render(dummy_renderer{});
  EXPECT_THAT(state_aware_rend.render_failed_type, IsEmpty());
}

TEST(WidgetBuilder, DisplayForEachState) // NOLINT
{
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               .event(int_as_event_handler{})
               //.state(int_states{})
               .display(display_per_state(fill_rect{}))
               .build();
  auto &[per_state] = w.displays();
  get<0>(per_state).colour() = default_colour_t{255, 0, 0, 255};
  get<1>(per_state).colour() = default_colour_t{0, 255, 0, 255};

  auto r = test_renderer({0, 0, 1, 1});
  auto sr = sub_renderer(r);
  w.render(sr);
  ASSERT_THAT(r.drawn_pixels, SizeIs(Eq(1)));
  auto &[red, green, blue, alpha] = r.drawn_pixels[0];
  EXPECT_THAT(red, Eq(255));
  EXPECT_THAT(green, Eq(0));
  EXPECT_THAT(blue, Eq(0));
  EXPECT_THAT(alpha, Eq(255));
  r.drawn_pixels[0] = {};
  bounding_box auto new_area = w.handle(1);
  expect_box_equal(new_area, call::point_area(r));
  w.render(sr);
  ASSERT_THAT(r.drawn_pixels, SizeIs(Eq(1)));
  EXPECT_THAT(red, Eq(0));
  EXPECT_THAT(green, Eq(255));
  EXPECT_THAT(blue, Eq(0));
  EXPECT_THAT(alpha, Eq(255));
}

TEST(WidgetBuilder, SubcomponentsResize) // NOLINT
{
  mock_widget subw;
  int mock_calls{};
  auto sc_area = default_point_rect{};
  EXPECT_CALL(subw, do_area(_))
      .WillRepeatedly([&mock_calls, &sc_area](auto const &a) {
        ++mock_calls;
        sc_area = a;
      });
  auto w = widget_builder()
               .area(box_from_xyxy<default_point_rect>(0, 1, 3, 4))
               .subcomponents(std::ref(subw))
               .on_resize([](auto &&self, bounding_box auto const &new_area) {
                 self.subcomponent().area(new_area);
               })
               .build();
  EXPECT_THAT(mock_calls, Eq(1)) << "Should be called once on creation";
  expect_box_equal(sc_area, w.area());
  w.area(box_from_xyxy<default_point_rect>(0, 2, 4, 5));
  EXPECT_THAT(mock_calls, Eq(2));
  expect_box_equal(sc_area, w.area());
}

TEST(WidgetBuilder, SubcomponentsRender) // NOLINT
{
  auto s1 = NiceMock<mock_widget>{};
  auto s2 = NiceMock<mock_widget>{};
  EXPECT_CALL(s1, do_area(_)).Times(1);
  EXPECT_CALL(s2, do_area(_)).Times(1);
  {
    InSequence s;
    EXPECT_CALL(s1, do_render()).Times(1);
    EXPECT_CALL(s2, do_render()).Times(1);
  }
  auto w = widget_builder()
               .area(default_rect{{0, 0}, {3, 3}})
               .subcomponents(std::ref(s1), std::ref(s2))
               .on_resize([](auto &&self, bounding_box auto b) {
                 auto &[s1, s2] = self.subcomponents();
                 s1.area(trim_from_left(&b, point_unit(1)));
                 s2.area(b);
               })
               .build();
  w.render(dummy_renderer{});
}

TEST(Widget, BasicButton) // NOLINT
{
  bool clicked{};
  using enum momentary_button_states;
  auto last_state = off;
  int calls{};
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               .event(buttonlike_trigger(
                   momentary_button()
                       .click([&clicked, &calls](auto &&...) {
                         clicked = true;
                         ++calls;
                       })
                       .hover([&last_state, &calls](auto &&...) {
                         last_state = hover;
                         ++calls;
                       })
                       .hold([&last_state, &calls](auto &&...) {
                         last_state = hold;
                         ++calls;
                       })
                       .exit([&last_state, &calls](auto &&...) {
                         last_state = off;
                         ++calls;
                       })
                       .build()))
               .display(display_per_state(fill_rect{}))
               .build();
  auto &[filler] = w.displays();
  get<off>(filler).colour() = {0, 0, 0, 255};
  get<hover>(filler).colour() = {1, 0, 0, 255};
  get<hold>(filler).colour() = {2, 0, 0, 255};
  test_renderer r{{0, 0, 1, 1}};
  ASSERT_THAT(r.drawn_pixels, SizeIs(1));
  auto &[red, green, blue, alpha] = r.drawn_pixels.front();
  auto sr = sub_renderer(r);
  auto reset = [&clicked, &calls] {
    clicked = false;
    calls = 0;
  };

  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(0, 0, 0, 255));

  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(0));
  EXPECT_THAT(last_state, Eq(off));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(0, 0, 0, 255));
  reset();

  w.handle(create_event<interpreted_events::pointer_hover>(
      default_point_coordinate{}));
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_state, Eq(hover));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(1, 0, 0, 255));
  reset();

  w.handle(create_event<interpreted_events::pointer_hold>(
      default_point_coordinate{}));
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_state, Eq(hold));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(2, 0, 0, 255));
  reset();

  w.handle(create_event<interpreted_events::primary_click>(
      default_point_coordinate{}));
  EXPECT_THAT(clicked, IsTrue());
  EXPECT_THAT(calls, Eq(2));
  EXPECT_THAT(last_state, Eq(hover));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(1, 0, 0, 255));
  reset();

  w.handle(create_event<interpreted_events::pointer_exit>());
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_state, Eq(off));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(0, 0, 0, 255));
  reset();
}

TEST(Widget, ButtonSharedStateCallback) // NOLINT
{
  int i{};
  auto w = widget_builder()
               .area(default_rect{{0, 0}, {2, 2}})
               .event(buttonlike_trigger(momentary_button()
                                             .callback_state(std::ref(i))
                                             .click([](int &i_in) { ++i_in; })
                                             .build()))
               .build();
  EXPECT_THAT(i, Eq(0));
  w.handle(create_event<interpreted_events::primary_click>(
      default_point_coordinate{}));
  EXPECT_THAT(i, Eq(1));
}

struct test_button_list {
  struct element_t {
    test_button_list &parent;
    int index;

    constexpr void handle(radio_button::trigger_on,
                          widget_back_propagater auto &&bp) {
      parent.on_activate(index);
      bp.rerender(box_from_xyxy<default_point_rect>(index, 0, index + 1, 1));
    }
    constexpr void handle(radio_button::trigger_off,
                          widget_back_propagater auto &&bp) {
      parent.on_deactivate(index);
      bp.rerender(box_from_xyxy<default_point_rect>(index, 0, index + 1, 1));
    }
    template <state_marker S, widget_back_propagater T>
    constexpr void set_state(S const &s, T &&t) const {
      t.rerender(box_from_xyxy<default_point_rect>(index, 0, index + 1, 1));
      parent.state_change(s.current_state(), index);
    }
  };
  std::function<void(int)> on_activate = bp::no_op;
  std::function<void(int)> on_deactivate = bp::no_op;
  int sz{};
  std::function<void(radio_button::element_state, int)> state_change =
      bp::no_op;
  using element_id = std::size_t;
  constexpr bool find_sub_at_location(
      pixel_coord auto const &pos,
      std::invocable<element_t &, std::size_t> auto &&on_find) {
    if (call::x_of(pos).value() >= 0 && call::x_of(pos).value() < sz) {
      auto i = call::x_of(pos).value();
      auto e = element_t(*this, i);
      on_find(e, i);
      return true;
    }
    return false;
  }
  constexpr void for_each(auto &&f) {
    for (int i = 0; i < sz; ++i) {
      f(element_t{*this, i}, static_cast<std::size_t>(i));
    }
  }

  constexpr void render(renderer auto &&r, button_list_args auto &&args) const {
    for (int i = 0; i < sz; ++i) {
      auto bright =
          static_cast<std::uint_least8_t>(args.button_state(i).current_state());
      fill(r,
           box_from_xyxy<default_point_rect>(i, 0, i + 1, call::height(args)),
           default_colour_t{bright, bright, bright, 255u});
    }
  }

  [[nodiscard]] constexpr default_point_rect intrinsic_min_size() const {
    return box_from_xywh<default_point_rect>(0, 0, sz, 1);
  }
};
static_assert(radio_button::element<test_button_list>);

TEST(Widget, RadioButtonDecorator) // NOLINT
{
  int current_element = lowest_possible;
  int activations{};
  int deactivations{};
  auto constexpr full_area = box_from_xyxy<default_point_rect>(0, 0, 16, 10);
  auto list =
      widget_builder()
          .area(full_area)
          .event(radio_button_trigger()
                     .elements(test_button_list{
                         [&activations, &current_element](int element) {
                           ++activations;
                           current_element = element;
                         },
                         [&deactivations, &current_element](int element) {
                           ++deactivations;
                           EXPECT_THAT(element, Eq(current_element));
                           current_element = -1;
                         },
                         3})
                     .build())
          .build();
  // activate button 0
  auto backprop = basic_widget_back_propagater(full_area);
  click_widget(list, {}, backprop);
  EXPECT_THAT(activations, Eq(1));
  EXPECT_THAT(deactivations, Eq(0));
  EXPECT_THAT(current_element, Eq(0));
  // deactivate button 0
  click_widget(list, {}, backprop);
  EXPECT_THAT(activations, Eq(1));
  EXPECT_THAT(deactivations, Eq(1));
  EXPECT_THAT(current_element, Eq(-1));
  // click outside any subcomponents
  click_widget(list, {12, 0}, backprop);
  EXPECT_THAT(activations, Eq(1));
  EXPECT_THAT(deactivations, Eq(1));
  EXPECT_THAT(current_element, Eq(-1));

  // activate button 1
  click_widget(list, {1, 7}, backprop);
  EXPECT_THAT(activations, Eq(2));
  EXPECT_THAT(deactivations, Eq(1));
  EXPECT_THAT(current_element, Eq(1));

  EXPECT_THAT(call::width(list.intrinsic_min_size()).value(), Eq(3));
}

TEST(Widget, RadioButtonListRender) // NOLINT
{
  constexpr auto full_area = default_rect{{0, 0}, {3, 1}};
  constexpr auto state2bright = [](radio_button::element_state s) {
    return static_cast<std::uint_least8_t>(s);
  };
  constexpr auto state2colour = [=](radio_button::element_state s) {
    auto const val = state2bright(s);
    return default_colour_t{val, val, val, 255};
  };
  using enum radio_button::element_state;
  std::vector<radio_button::element_state> states{relaxed_off, relaxed_off,
                                                  relaxed_off};
  auto exp_states = states;
  std::vector<std::pair<radio_button::element_state, int>> state_changes;
  auto list = widget_builder()
                  .area(full_area)
                  .event(radio_button_trigger()
                             .elements(test_button_list{
                                 bp::no_op, bp::no_op, 3,
                                 [&](auto s, auto i) {
                                   state_changes.emplace_back(s, i);
                                   ASSERT_THAT(i, Lt(ssize(states)));
                                   states.at(i) = s;
                                 }})
                             .build())
                  .build();
  auto rend = test_renderer{full_area};
  auto sr = sub_renderer(rend);
  auto rgba_sep = rend.individual_colours();
  auto &[r, g, b, a] = rgba_sep;
  auto do_render = [&] {
    list.render(sr);
    rgba_sep = rend.individual_colours();
  };

  do_render();
  EXPECT_THAT(r, AllOf(SizeIs(3), Each(red(state2colour(relaxed_off)))));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));

  using enum interpreted_events;
  auto re_area = call::handle(
      list, create_event<pointer_hover>(default_point_coordinate{{0, 0}}));
  EXPECT_THAT(state_changes, ElementsAre(Pair(hover_off, 0)));
  exp_states[0] = hover_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(hover_off), state2bright(relaxed_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
  expect_box_equal(re_area, box_from_xyxy<default_point_rect>(0, 0, 1, 1));

  state_changes.clear();
  re_area = call::handle(
      list, create_event<pointer_hover>(default_point_coordinate{{1, 0}}));
  EXPECT_THAT(state_changes,
              UnorderedElementsAre(Pair(relaxed_off, 0), Pair(hover_off, 1)));
  exp_states[0] = relaxed_off;
  exp_states[1] = hover_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(relaxed_off), state2bright(hover_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
  expect_box_equal(re_area, box_from_xyxy<default_point_rect>(0, 0, 2, 1));

  state_changes.clear();
  re_area = call::handle(
      list, create_event<pointer_hold>(default_point_coordinate{{1, 0}}));
  EXPECT_THAT(state_changes, UnorderedElementsAre(Pair(hold_off, 1)));
  exp_states[0] = relaxed_off;
  exp_states[1] = hold_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(relaxed_off), state2bright(hold_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
  expect_box_equal(re_area, box_from_xyxy<default_point_rect>(1, 0, 2, 1));

  state_changes.clear();
  re_area = call::handle(
      list, create_event<primary_click>(default_point_coordinate{{1, 0}}));
  exp_states[0] = relaxed_off;
  exp_states[1] = hover_on;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(relaxed_off), state2bright(hover_on),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
  expect_box_equal(re_area, box_from_xyxy<default_point_rect>(1, 0, 2, 1));

  click_widget(list, {});
  exp_states[0] = hover_on;
  exp_states[1] = relaxed_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(hover_on), state2bright(relaxed_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));

  re_area = call::handle(list, create_event<pointer_exit>());
  exp_states[0] = relaxed_on;
  exp_states[1] = relaxed_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r,
              ElementsAre(state2bright(relaxed_on), state2bright(relaxed_off),
                          state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));

  exp_states[0] = hover_on;
  re_area = call::handle(
      list, create_event<pointer_hold>(default_point_coordinate{{0, 0}}));
  re_area = call::handle(list, create_event<pointer_exit>());
  re_area = call::handle(
      list, create_event<pointer_hover>(default_point_coordinate{{0, 0}}));
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(hover_on), state2bright(relaxed_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));

  click_widget(list);
  exp_states[0] = hover_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(hover_off), state2bright(relaxed_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
}

struct dummy_vp_item {
  struct dummy_event {};
  default_point_rect size{};
  int dummy_events_received{};
  std::optional<default_point_coordinate> click_point{};
  constexpr void render(renderer auto &&r, render_args auto &&args) const {
    if (args.width() > point_unit(0) && args.height() > point_unit(0)) {
      call::draw_pixels(
          r, convert_pixelpoint<pixel_size_tag>(size, call::pixel_scale(r)),
          [](auto &&b, auto &&cb) {
            for (auto y : y_view(b)) {
              for (auto x : x_view(b)) {
                cb(default_pixel_coordinate{x, y},
                   default_colour_t{
                       static_cast<std::uint_least8_t>(x.value() + 1),
                       static_cast<std::uint_least8_t>(y.value() + 1), 0, 255});
              }
            }
          });
    }
  }
  constexpr default_point_rect const &area() const noexcept { return size; }

  template <bounding_box A, widget_back_propagater BP>
  constexpr void handle(A const &, dummy_event const &, BP &&bp) {
    ++dummy_events_received;
    bp.rerender();
  }
  template <bounding_box A,
            interpreted_event_types<interpreted_events::primary_click> E,
            widget_back_propagater BP>
  constexpr void handle(A const &, E const &event, BP &&) {
    click_point = call::position(event);
  }
};

TEST(Widget, ViewPortPan) // NOLINT
{
  auto constexpr full_area = default_rect{{0, 0}, {2, 2}};
  auto constexpr ext_area = default_rect{{-1, -1}, {4, 4}};
  auto item = dummy_vp_item{default_point_rect(ext_area)};
  auto w = widget_builder()
               .event(view_port_trigger::builder().view(std::ref(item)).build())
               .area(full_area)
               .build();

  auto rend = test_renderer{full_area};
  auto sr = sub_renderer(rend);
  auto rgba_sep = rend.individual_colours();
  auto &[r, g, b, a] = rgba_sep;
  auto do_render = [&] {
    std::ranges::fill(rend.drawn_pixels, default_colour_t{255, 255, 255, 0});
    w.render(sr);
    rgba_sep = rend.individual_colours();
  };
  do_render();
  EXPECT_THAT(r, ElementsAre(1, 2, 1, 2));
  EXPECT_THAT(g, ElementsAre(1, 1, 2, 2));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, 1.f, 0.f));
  do_render();
  EXPECT_THAT(r, ElementsAre(2, 3, 2, 3));
  EXPECT_THAT(g, ElementsAre(1, 1, 2, 2));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, 0.f, 1.f));
  do_render();
  EXPECT_THAT(r, ElementsAre(2, 3, 2, 3));
  EXPECT_THAT(g, ElementsAre(2, 2, 3, 3));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, 2.f, 0.f));
  do_render();
  EXPECT_THAT(r, ElementsAre(3, 4, 3, 4))
      << "The scroll should not exceed past the size of the viewed item";
  EXPECT_THAT(g, ElementsAre(2, 2, 3, 3));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, -8.f, 0.f));
  do_render();
  EXPECT_THAT(r, ElementsAre(0, 1, 0, 1))
      << "The scroll should not exceed past point zero of the view";
  EXPECT_THAT(g, ElementsAre(2, 2, 3, 3));
}

TEST(Widget, ViewPortSmallViewed) // NOLINT
{
  auto constexpr full_area = default_rect{{0, 0}, {2, 2}};
  auto constexpr ext_area = default_rect{{-1, -1}, {0, 0}};
  auto item = dummy_vp_item{default_point_rect(ext_area)};
  auto w = widget_builder()
               .event(view_port_trigger::builder().view(std::ref(item)).build())
               .area(full_area)
               .build();

  auto rend = test_renderer{full_area};
  auto sr = sub_renderer(rend);
  auto rgba_sep = rend.individual_colours();
  auto &[r, g, b, a] = rgba_sep;
  auto do_render = [&] {
    std::ranges::fill(rend.drawn_pixels, default_colour_t{255, 255, 255, 0});
    w.render(sr);
    rgba_sep = rend.individual_colours();
  };
  do_render();
  EXPECT_THAT(r, ElementsAre(0, 255, 255, 255));
  EXPECT_THAT(g, ElementsAre(0, 255, 255, 255));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, 1.f, 1.f));
  do_render();
  EXPECT_THAT(r, ElementsAre(0, 255, 255, 255));
  EXPECT_THAT(g, ElementsAre(0, 255, 255, 255));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, -1.f, -1.f));
  do_render();
  EXPECT_THAT(r, ElementsAre(0, 255, 255, 255));
  EXPECT_THAT(g, ElementsAre(0, 255, 255, 255));
}

TEST(Widget, ViewPortZoom) // NOLINT
{
  auto constexpr full_area = default_rect{{0, 0}, {2, 2}};
  auto constexpr ext_area = default_rect{{0, 0}, {1, 1}};
  auto item = dummy_vp_item{default_point_rect(ext_area)};
  auto w = widget_builder()
               .event(view_port_trigger::builder()
                          .view(std::ref(item))
                          .enable_zoom()
                          .build())
               .area(full_area)
               .build();
  w.handle(interpreted_event<interpreted_events::zoom>(
      {}, default_point_coordinate{}, 2.f, 2.f));

  auto rend = test_renderer{full_area};
  auto sr = sub_renderer(rend);
  auto rgba_sep = rend.individual_colours();
  auto &[r, g, b, a] = rgba_sep;
  auto do_render = [&] {
    std::ranges::fill(rend.drawn_pixels, default_colour_t{255, 255, 255, 0});
    w.render(sr);
    rgba_sep = rend.individual_colours();
  };
  do_render();
  EXPECT_THAT(r, ElementsAre(1, 2, 1, 2));
  EXPECT_THAT(g, ElementsAre(1, 1, 2, 2));
}

TEST(Widget, ViewPortZoomPassHandling) // NOLINT
{
  auto constexpr full_area = default_rect{{0, 0}, {2, 2}};
  auto constexpr ext_area = default_rect{{0, 0}, {4, 4}};
  auto item = dummy_vp_item{default_point_rect(ext_area)};
  auto w = widget_builder()
               .event(view_port_trigger::builder()
                          .view(std::ref(item))
                          .enable_zoom()
                          .build())
               .area(full_area)
               .build();
  auto rerender_area = w.handle(dummy_vp_item::dummy_event{});
  EXPECT_THAT(item.dummy_events_received, Eq(1));
  expect_box_equal(rerender_area.value(), full_area);
  w.handle(interpreted_event<interpreted_events::primary_click>(
      {}, default_point_coordinate{}));
  ASSERT_TRUE(item.click_point.has_value());
  EXPECT_THAT(*item.click_point, Eq(default_point_coordinate{}));
  w.handle(interpreted_event<interpreted_events::scroll>(
      {}, default_point_coordinate{}, 1.f, 1.f));
  w.handle(interpreted_event<interpreted_events::primary_click>(
      {}, default_point_coordinate{}));
  ASSERT_TRUE(item.click_point.has_value());
  EXPECT_THAT(*item.click_point, Eq(default_point_coordinate{1, 1}));
}

TEST(Widget, OnDestruct) // NOLINT
{
  void *ptr_to_widget = nullptr;
  {
    auto w = widget_builder().build();
    ptr_to_widget = &w;
    w.set_on_destruct([&ptr_to_widget](auto &&w) {
      EXPECT_THAT(w, Address(Eq(ptr_to_widget)));
      ptr_to_widget = nullptr;
    });
    // We create a copy of w2. If things works out correctly, w2:s destruction f
    // will be a no-op, making the EXPECT_THAT further up pass.
    auto w2 = w;
    unused(w2);
  }
  EXPECT_THAT(ptr_to_widget, Eq(nullptr));
}

} // namespace cgui::tests

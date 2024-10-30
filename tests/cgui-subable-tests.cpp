
#include <cgui/cgui.hpp>

#include <gmock/gmock.h>

#include <cgui_test_utils.hpp>

namespace cgui::tests {

using namespace ::testing;

TEST(RecursiveAreaNavigator, NavigateSimple) // NOLINT
{
  auto nav = recursive_area_navigator({{0, 0}, {5, 5}});
  expect_box_equal(nav.relative_area(), default_rect{{0, 0}, {5, 5}});
  expect_box_equal(nav.absolute_area(), default_rect{{0, 0}, {5, 5}});
  auto sub = nav.sub({{0, 0}, {4, 4}});
  expect_box_equal(sub.relative_area(), default_rect{{0, 0}, {4, 4}});
  expect_box_equal(sub.absolute_area(), default_rect{{0, 0}, {4, 4}});
  sub = nav.sub({{1, 1}, {2, 2}});
  expect_box_equal(sub.relative_area(), default_rect{{0, 0}, {1, 1}});
  expect_box_equal(sub.absolute_area(), default_rect{{1, 1}, {2, 2}});
  sub = nav.sub({{0, 0}, {6, 6}});
  expect_box_equal(sub.relative_area(), default_rect{{0, 0}, {5, 5}});
  expect_box_equal(sub.absolute_area(), default_rect{{0, 0}, {5, 5}});

  nav = recursive_area_navigator({{1, 1}, {5, 5}});
  sub = nav.sub({{0, 2}, {4, 4}});
  expect_box_equal(sub.relative_area(), default_rect{{1, 0}, {4, 2}});
  expect_box_equal(sub.absolute_area(), default_rect{{1, 2}, {4, 4}});
  auto sub2 = sub.sub({{2, 0}, {4, 3}});
  expect_box_equal(sub2.relative_area(), default_rect{{0, 0}, {2, 2}});
  expect_box_equal(sub2.absolute_area(), default_rect{{2, 2}, {4, 4}});
}

TEST(RecursiveAreaNavigator, Nudger) // NOLINT
{
  auto nav = recursive_area_navigator({{0, 0}, {5, 5}});
  auto nudger = nav.relative_to_absolute_nudger();
  auto xy = nudger(default_pixel_coord{0, 0});
  auto &[x, y] = xy;
  EXPECT_THAT(x, Eq(0));
  EXPECT_THAT(y, Eq(0));
  auto sub = nav.sub({{
                          1,
                          1,
                      },
                      {5, 5}});
  nudger = sub.relative_to_absolute_nudger();
  xy = nudger(default_pixel_coord{0, 0});
  EXPECT_THAT(x, Eq(1));
  EXPECT_THAT(y, Eq(1));
}

TEST(SubRenderer, DrawPixels) // NOLINT
{
  auto r = test_renderer({{0, 0}, {6, 7}});
  auto sr1 = sub_renderer(r, r.area());
  sr1.draw_pixels(default_rect{{0, 0}, {6, 7}},
                  [&](bounding_box auto &&b, auto &&drawer) {
                    EXPECT_THAT(call::l_x(b), Eq(0));
                    EXPECT_THAT(call::t_y(b), Eq(0));
                    EXPECT_THAT(call::width(b), Eq(call::width(r.area())));
                    EXPECT_THAT(call::height(b), Eq(call::height(r.area())));
                    drawer(default_pixel_coord{}, default_colour_t{1, 1, 1, 1});
                  });
  EXPECT_THAT(r.failed_calls, ElementsAre());
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  ASSERT_THAT(ssize(r.drawn_pixels), Eq(6 * 7));
  EXPECT_THAT(r.at(0, 0).red, Eq(1));
  EXPECT_THAT(r.at(0, 0).green, Eq(1));
  EXPECT_THAT(r.at(0, 0).blue, Eq(1));
  EXPECT_THAT(r.at(0, 0).alpha, Eq(1));
  EXPECT_THAT(r.at(1, 0).red, Eq(0));
  EXPECT_THAT(r.at(1, 0).green, Eq(0));
  EXPECT_THAT(r.at(1, 0).blue, Eq(0));
  EXPECT_THAT(r.at(1, 0).alpha, Eq(0));
  EXPECT_THAT(r.at(0, 1).red, Eq(0));
  EXPECT_THAT(r.at(0, 1).green, Eq(0));
  EXPECT_THAT(r.at(0, 1).blue, Eq(0));
  EXPECT_THAT(r.at(0, 1).alpha, Eq(0));
  auto s2_x = 1;
  auto s2_y = 1;
  auto s2 = sr1.sub(default_rect{{s2_x, s2_y}, {4, 4}});
  s2.draw_pixels(default_rect{{0, 0}, {4, 4}},
                 [&](bounding_box auto &&b, auto &&drawer) {
                   EXPECT_THAT(call::l_x(b), Eq(0));
                   EXPECT_THAT(call::t_y(b), Eq(0));
                   EXPECT_THAT(call::width(b), Eq(3));
                   EXPECT_THAT(call::height(b), Eq(3));
                   drawer(default_pixel_coord{}, default_colour_t{2, 1, 1, 1});
                 });
  EXPECT_THAT(r.at(0, 0).red, Eq(1));
  EXPECT_THAT(r.at(0, 0).green, Eq(1));
  EXPECT_THAT(r.at(0, 0).blue, Eq(1));
  EXPECT_THAT(r.at(0, 0).alpha, Eq(1));
  EXPECT_THAT(r.at(1, 0).red, Eq(0));
  EXPECT_THAT(r.at(1, 0).green, Eq(0));
  EXPECT_THAT(r.at(1, 0).blue, Eq(0));
  EXPECT_THAT(r.at(1, 0).alpha, Eq(0));
  EXPECT_THAT(r.at(0, 1).red, Eq(0));
  EXPECT_THAT(r.at(0, 1).green, Eq(0));
  EXPECT_THAT(r.at(0, 1).blue, Eq(0));
  EXPECT_THAT(r.at(0, 1).alpha, Eq(0));
  EXPECT_THAT(r.at(1, 1).red, Eq(2));
  EXPECT_THAT(r.at(1, 1).green, Eq(1));
  EXPECT_THAT(r.at(1, 1).blue, Eq(1));
  EXPECT_THAT(r.at(1, 1).alpha, Eq(1));
}

TEST(SubRenderer, PartialDrawPixels) // NOLINT
{
  auto r = test_renderer({{0, 0}, {4, 4}});
  auto s1 = sub_renderer(r, default_rect{{1, 2}, {3, 3}});
  s1.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(1));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(3));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{1, 1, 1, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  using namespace std::views;
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 1, 0, 0, //
                          0, 0, 0, 0  //
                          ));
  EXPECT_THAT(ic.green, ElementsAre(    //
                            0, 0, 0, 0, //
                            0, 0, 0, 0, //
                            0, 1, 0, 0, //
                            0, 0, 0, 0  //
                            ));
  EXPECT_THAT(ic.blue, ElementsAre(    //
                           0, 0, 0, 0, //
                           0, 0, 0, 0, //
                           0, 1, 0, 0, //
                           0, 0, 0, 0  //
                           ));
  EXPECT_THAT(ic.alpha, ElementsAre(      //
                            0, 0, 0, 0,   //
                            0, 0, 0, 0,   //
                            0, 255, 0, 0, //
                            0, 0, 0, 0    //
                            ));

  auto s2full = s1.sub(r.area());
  s2full.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(1));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(3));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{2, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 2, 0, 0, //
                          0, 0, 0, 0  //
                          ));

  auto s2xid = s2full.sub(default_rect{{1, 0}, {3, 4}});
  s2xid.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(0));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(2));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{3, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 3, 0, 0, //
                          0, 0, 0, 0  //
                          ));

  auto s3x = s2xid.sub(default_rect{{1, 0}, {3, 4}});
  s3x.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(0));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(1));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{4, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 3, 4, 0, //
                          0, 0, 0, 0  //
                          ));

  auto s4 = s2full.sub(default_rect{{2, 2}, {4, 4}});
  s4.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(0));
    EXPECT_THAT(call::t_y(b), Eq(0));
    EXPECT_THAT(call::r_x(b), Eq(1));
    EXPECT_THAT(call::b_y(b), Eq(1));
    drawer(call::top_left(b), default_colour_t{5, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 3, 5, 0, //
                          0, 0, 0, 0  //
                          ));
}

TEST(SubRender, DrawPixelOutsideCanvas) // NOLINT
{
  auto r = test_renderer({{0, 0}, {4, 4}});
  auto s_main = sub_renderer(r, r.area());
  auto s1 = s_main.sub(default_rect{{4, 5}, {5, 6}});
  s1.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::width(b), Eq(0));
    EXPECT_THAT(call::height(b), Eq(0));
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
}

}

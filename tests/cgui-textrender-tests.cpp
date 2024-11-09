
#include <cgui/cgui.hpp>

#include <gmock/gmock.h>

#include <cgui_test_utils.hpp>

namespace cgui::tests {
using namespace ::testing;

struct dummy_glyph {
  pixel_unit_t<int> length;
  pixel_unit_t<int> ascend;
  pixel_unit_t<int> descend;
  std::uint_least8_t alpha;

  dummy_glyph(int l, auto a, pixel_unit_t<int> asc, pixel_unit_t<int> des)
      : length(pixel_unit(l)), alpha(a), ascend(asc), descend(des) {}

  constexpr pixel_unit_t<int> height() const {
    return ascend + descend + pixel_unit(1);
  }
  void render(auto &&renderer) const {
    call::draw_alpha(
        renderer, box_from_xywh<default_pixel_rect>(0, 0, length, height()),
        [alpha = alpha](bounding_box auto &&bbox, auto &&drawer) {
          for (auto i = call::l_x(bbox); i < call::r_x(bbox); ++i) {
            for (auto j = call::t_y(bbox); j < call::b_y(bbox); ++j) {
              drawer(pixel_unit_t<default_coordinate>(i, j), alpha);
            }
          }
        });
  }

  constexpr pixel_unit_t<std::uint_least8_t> advance_x() const {
    return length;
  }
  static constexpr pixel_unit_t<std::uint_least8_t> advance_y() { return {}; }
  constexpr default_pixel_rect pixel_area() const {
    return box_from_xywh<default_pixel_rect>(0, 0, length, height());
  }
  constexpr auto base_to_top() const { return ascend + pixel_unit(1); }
};
struct dummy_font_face {
  int mutable faulty_glyphs{};
  bool use_height_{};

  constexpr dummy_font_face() = default;
  constexpr explicit dummy_font_face(bool use_height)
      : use_height_(use_height) {}

  constexpr auto _gheight(int h) const {
    if (use_height_) {
      return pixel_unit_t<int>(h);
    }
    return pixel_unit_t<int>();
  }

  constexpr auto ascender() const { return _gheight(1); }
  constexpr auto descender() const { return _gheight(1); }

  expected<dummy_glyph, bool> glyph(char c) const {
    switch (c) {
    case '0':
      return dummy_glyph{1, 255, _gheight(0), _gheight(1)};
    case '1':
      return dummy_glyph{2, 127, _gheight(1), _gheight(0)};
    case '2':
      return dummy_glyph{3, 3, _gheight(1), _gheight(1)};
    case '-':
      return dummy_glyph{2, 63, _gheight(1), _gheight(0)};
    case ' ':
      return dummy_glyph{1, 0, _gheight(0), _gheight(0)};
    default:
      ++faulty_glyphs;
      return unexpected(false);
    }
  }
  constexpr auto full_height() const {
    return ascender() + descender() + pixel_unit_t<int>(1);
  }
};

constexpr pixel_unit_t<int> bitmap_top(dummy_font_face const &font,
                                       dummy_glyph const &g) {
  return font.ascender() - g.ascend - pixel_unit_t<int>(1);
}

TEST(TextRender, PerfectWidthString) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});
  call::set_displayed(t2r, pixel_unit(4), pixel_unit(1), "1 0");
  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr, widget_render_args(4, 1));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(255, 255, 255, 255, //
                                  0, 0, 0, 0));
  EXPECT_THAT(ic.alpha, ElementsAre(127, 127, 0, 255, //
                                    0, 0, 0, 0));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}

TEST(TextRender, CenterAligned) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 6, 3});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "1 0");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(0, 0, 0, 0, 0, 0,         //
                                  0, 255, 255, 255, 255, 0, //
                                  0, 0, 0, 0, 0, 0          //

                                  ));
  EXPECT_THAT(ic.alpha, ElementsAre(0, 0, 0, 0, 0, 0,       //
                                    0, 127, 127, 0, 255, 0, //
                                    0, 0, 0, 0, 0, 0        //
                                    ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}

TEST(TextRender, TwoLinesSpace) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "1 1");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(                            //
                          0, 255, 255, AnyOf(Eq(0), Eq(255)), //
                          0, 255, 255, 0                      //
                          ));
  EXPECT_THAT(ic.alpha, ElementsAre(        //
                            0, 127, 127, 0, //
                            0, 127, 127, 0  //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, TwoLinesDashDirect) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "120");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, Each(Eq(255)));
  EXPECT_THAT(ic.alpha, ElementsAre(          //
                            127, 127, 63, 63, //
                            3, 3, 3, 255      //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, TwoLinesDashIndirect) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "1001");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, Each(Eq(255)));
  EXPECT_THAT(ic.alpha, ElementsAre(           //
                            127, 127, 63, 63,  //
                            255, 255, 127, 127 //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, ManualNewLine) // NOLINT
{

  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "1\n1");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.alpha, ElementsAre(        //
                            0, 127, 127, 0, //
                            0, 127, 127, 0  //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, ThreeLines) // NOLINT
{

  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 3});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "10011");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.alpha, ElementsAre(           //
                            127, 127, 63, 63,  //
                            255, 255, 63, 63,  //
                            127, 127, 127, 127 //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
}
TEST(TextRender,
     CenterWithAscendAndDescend) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face(true));

  auto r = test_renderer({0, 0, 6, 5});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "012");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.alpha, ElementsAre(                //
                            0, 0, 0, 0, 0, 0,       //
                            0, 127, 127, 3, 3, 3,   //
                            255, 127, 127, 3, 3, 3, //
                            255, 0, 0, 3, 3, 3,     //
                            0, 0, 0, 0, 0, 0        //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
}
TEST(TextRender, RefFace) // NOLINT
{
  auto face = dummy_font_face();
  auto t2r = text_renderer(std::ref(face));
  t2r.set_displayed(pixel_unit(1), pixel_unit(1), "?");
  dummy_renderer r;
  t2r.render(r, widget_render_args(1, 1));
  EXPECT_THAT(face.faulty_glyphs, Eq(1));
}
struct mock_face {
  MOCK_METHOD((expected<dummy_glyph, bool>), glyph, (char), (const));
  inline pixel_unit_t<int> full_height() const { return {{}, 1}; }
  inline pixel_unit_t<int> ascender() const { return {{}, 1}; }
  inline pixel_unit_t<int> descender() const { return {{}, 1}; }
};
TEST(TextRender, CachedGlyphs) // NOLINT
{
  auto mface = mock_face();
  EXPECT_CALL(mface, glyph(Eq('0'))).WillOnce([](auto &&...) {
    return dummy_font_face().glyph('0');
  });
  EXPECT_CALL(mface, glyph(Not(Eq('0'))))
      .Times(AnyNumber())
      .WillRepeatedly([](auto &&...) { return dummy_font_face().glyph('0'); });
  auto face = cached_font(std::ref(mface));
  auto t2r = text_renderer(std::ref(face));
  t2r.set_displayed(pixel_unit(5), pixel_unit(5), "00");
  dummy_renderer r;
  t2r.render(r, widget_render_args(1, 1));
}
TEST(TextRender, CachedGlyphs4) // NOLINT
{
  using font_t = cached_font<dummy_font_face &>;
  auto dummy_face = dummy_font_face();
  auto t2r = text_renderer(font_t(dummy_face));
  call::set_displayed(t2r, pixel_unit(7), pixel_unit(1), "1 02");
  auto r = test_renderer({0, 0, 7, 1});
  auto sr = sub_renderer(r);
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr, widget_render_args(7, 1));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(dummy_face.faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(255, 255, 255, 255, 255, 255, 255));
  EXPECT_THAT(ic.alpha, ElementsAre(127, 127, 0, 255, 3, 3, 3));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, TooSmallBox) // NOLINT
{

  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face(true));

  auto r = test_renderer({0, 0, 7, 5});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.pixel_area()),
                      call::height(r.pixel_area()), "111111111");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr,
               widget_render_args(call::width(call::point_area(r)),
                                  call::height(call::point_area(r))));
  // No expects, the assert will hit.
}

} // namespace cgui::tests
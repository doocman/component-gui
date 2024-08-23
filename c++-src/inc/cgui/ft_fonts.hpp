//
// Created by rvons on 2024-07-07.
//

#ifndef COMPONENT_GUI_FT_FONTS_HPP
#define COMPONENT_GUI_FT_FONTS_HPP

#include <bit>
#include <cassert>
#include <ranges>
#include <utility>
#include <vector>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <cgui/cgui-types.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {
template <> struct extend_api<FT_BBox> {
  static constexpr auto &&tl_x(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).xMin;
  }
  static constexpr auto &&tl_y(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).yMin;
  }
  static constexpr auto &&br_x(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).xMax;
  }
  static constexpr auto &&br_y(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).yMax;
  }

  static constexpr FT_BBox from_xyxy(FT_Pos xl, FT_Pos yt, FT_Pos xr,
                                     FT_Pos yb) {
    return {xl, yt, xr, yb};
  }
};
inline namespace {

class ft_font_library {
  static constexpr auto cleanup_ = [](FT_Library &l) { FT_Done_FreeType(l); };

  cleanup_object_t<decltype(cleanup_), FT_Library> lib_;

  constexpr ft_font_library() = default;

public:
  constexpr ft_font_library(ft_font_library &&other) noexcept = default;
  constexpr ft_font_library &
  operator=(ft_font_library &&other) noexcept = default;
  constexpr void swap(ft_font_library &r) noexcept { lib_.swap(r.lib_); }
  static expected<ft_font_library, FT_Error> init() {
    auto lib = ft_font_library();
    if (auto ec = FT_Init_FreeType(&lib.handle()); ec != FT_Error{}) {
      return unexpected(ec);
    }
    return lib;
  }
  [[nodiscard]] constexpr FT_Library &handle() noexcept {
    return lib_.first_value();
  }
  [[nodiscard]] constexpr FT_Library const &handle() const noexcept {
    return lib_.first_value();
  }
};
class ft_font_face_open_args {
  FT_Open_Args args{};

public:
  constexpr ft_font_face_open_args() = default;

  constexpr FT_Open_Args const &handle() const noexcept { return args; }
};

class ft_font_face;

class ft_font_glyph {
  static constexpr auto cleanup = [](FT_Glyph g) {
    if (g != nullptr) {
      FT_Done_Glyph(g);
    }
  };
  cleanup_object_t<decltype(cleanup), FT_Glyph> g_{};
  FT_UInt gi_{};
  constexpr ft_font_glyph() = default;
  constexpr FT_Glyph &glyph() { return g_.first_value(); }
  constexpr FT_Glyph glyph() const { return g_.first_value(); }

public:
  friend inline expected<ft_font_glyph, FT_Error> glyph(ft_font_face &face,
                                                        char v);

  constexpr ft_font_glyph(FT_Glyph g, FT_UInt gi) : g_(g), gi_(gi) {}

  constexpr expected<void, FT_Error> render(renderer auto &&rend, int bottom,
                                            int pen_x26_6 = {},
                                            int pen_y26_6 = {}) const {
    unused(g_, gi_);
    auto *bitmap_gl = glyph();
    auto org = FT_Vector{pen_x26_6, pen_y26_6};
    auto pen_x = pen_x26_6 >> 6;
    auto pen_y = pen_y26_6 >> 6;
    if (auto ec =
            FT_Glyph_To_Bitmap(&bitmap_gl, FT_RENDER_MODE_NORMAL, &org, false);
        ec != 0) {
      return unexpected(ec);
    }
    assert(bitmap_gl->format == FT_GLYPH_FORMAT_BITMAP);
    auto bm_destroy = bp::deferred([bitmap_gl] { FT_Done_Glyph(bitmap_gl); });
    auto &bitmap = std::bit_cast<FT_BitmapGlyph>(bitmap_gl)->bitmap;
    using int_t = std::make_signed_t<std::size_t>;
    // assert(static_cast<unsigned>(bottom) >= bitmap.rows);
    auto y_offset = bottom; // static_cast<int>(bottom - bitmap.rows);
    auto res = call::draw_pixels(
        rend,
        default_rect{{pen_x, pen_y + y_offset},
                     {pen_x + static_cast<int>(bitmap.width),
                      pen_y + y_offset + static_cast<int>(bitmap.rows)}},
        [&](pixel_drawer auto &&px_rend) {
          for (auto y :
               std::views::iota(int_t{}, static_cast<int_t>(bitmap.rows))) {
            for (auto x :
                 std::views::iota(int_t{}, static_cast<int_t>(bitmap.width))) {
              px_rend(
                  default_pixel_coord{static_cast<int>(x), static_cast<int>(y)},
                  default_colour_t{255, 255, 255,
                                   bitmap.buffer[y * bitmap.pitch + x]});
            }
          }
        });
    assert(res.has_value());
    return {};
  }
  constexpr void render(renderer auto &&rend, int bottom, not_null<int *> pen_x,
                        not_null<int *> pen_y) const {
    render(std::forward<decltype(rend)>(rend), bottom, *pen_x, *pen_y);
    *pen_x += g_->advance.x >> 10;
    *pen_y += g_->advance.y >> 10;
  }

  constexpr FT_Glyph handle() const noexcept { return glyph(); }
};

class ft_font_face {
public:
  using point_t = font_point<std::ratio<1, 64>, FT_F26Dot6>;

private:
  struct glyph_to_render {
    ft_font_glyph glyph;
    FT_Int bitmap_top;

    glyph_to_render(ft_font_glyph g, FT_Int top)
        : glyph(std::move(g)), bitmap_top(top) {}
  };
  static constexpr auto cleanup_ = [](FT_Face &f) { FT_Done_Face(f); };

  cleanup_object_t<decltype(cleanup_), FT_Face> v_{};
  point_t pts_ = whole_point(12);
  std::vector<glyph_to_render> string_;
  FT_BBox bbox_{};
  FT_Int string_length_{};

  constexpr ft_font_face() = default;

  FT_BBox compute_bbox() {
    auto bbox = FT_BBox{.xMin = highest_possible,
                        .yMin = highest_possible,
                        .xMax = lowest_possible,
                        .yMax = lowest_possible};

    for (auto &g : string_) {
      FT_BBox glyph_bbox;

      FT_Glyph_Get_CBox(g.glyph.handle(), ft_glyph_bbox_pixels, &glyph_bbox);

      if (glyph_bbox.xMin < bbox.xMin)
        bbox.xMin = glyph_bbox.xMin;

      if (glyph_bbox.yMin < bbox.yMin)
        bbox.yMin = glyph_bbox.yMin;

      if (glyph_bbox.xMax > bbox.xMax)
        bbox.xMax = glyph_bbox.xMax;

      if (glyph_bbox.yMax > bbox.yMax)
        bbox.yMax = glyph_bbox.yMax;
    }

    if (bbox.xMin > bbox.xMax) {
      bbox.xMin = 0;
      bbox.yMin = 0;
      bbox.xMax = 0;
      bbox.yMax = 0;
    }
    return bbox;
  }

public:
  constexpr ft_font_face(ft_font_face &&) noexcept = default;
  constexpr ft_font_face &operator=(ft_font_face &&) noexcept = default;
  static expected<ft_font_face, FT_Error>
  init(ft_font_library const &l, char const *path, FT_UInt horz_res,
       FT_UInt vert_res, FT_Long index = 0) {
    auto f = ft_font_face();
    if (auto ec = FT_New_Face(l.handle(), path, index, &f.v_.first_value());
        ec != FT_Error()) {
      return unexpected(ec);
    }
    if (auto ec = FT_Set_Char_Size(f.handle(), 0, 16 << 6, horz_res, vert_res);
        ec != FT_Error()) {
      return unexpected(ec);
    }
    return f;
  }
  static expected<ft_font_face, FT_Error>
  init(ft_font_library const &l, FT_Open_Args const &args, FT_Long index = 0) {
    auto f = ft_font_face();
    if (auto ec = FT_Open_Face(l.handle(), &args, index, &f.v_.first_value());
        ec != FT_Error()) {
      return unexpected(ec);
    }
    return f;
  }

  FT_Face handle() const { return v_.first_value(); }

  void set_text(readable_textc auto const &text) {
    if constexpr (std::is_array_v<std::remove_cvref_t<decltype(text)>>) {
      set_text(std::string_view(text));
    } else {
      string_.clear();
      string_.reserve(std::ranges::size(text));
      string_length_ = 0;
      for (auto c : text) {
        if (auto gle = glyph(*this, c); gle.has_value()) {
          string_length_ += gle->handle()->advance.x >> 16;
          string_.emplace_back(std::move(*gle), handle()->glyph->bitmap_top);
        }
      }
      bbox_ = compute_bbox();
    }
  }

  void render_text(renderer auto &&ren, int width, int height) {
    auto draw_center_x = width / 2;
    auto draw_center_y = height / 2;
    auto a = handle()->ascender;
    auto d = handle()->descender;
    auto text_center_x = string_length_ / 2;
    auto center_x = draw_center_x - text_center_x;
    auto center_y = draw_center_y - (((a - d) / 2) >> 6);
    auto pen_x = static_cast<int>(center_x) << 6;
    auto pen_y = int{};
    for (auto const &g : string_) {
      auto pos = center_y - g.bitmap_top;
      g.glyph.render(ren, pos, &pen_x, &pen_y);
    }
  }
};

inline expected<ft_font_glyph, FT_Error> glyph(ft_font_face &face, char v) {
  auto gl_index = FT_Get_Char_Index(face.handle(), v);
  FT_Glyph gl;
  if (auto ec = FT_Load_Glyph(face.handle(), gl_index, FT_LOAD_DEFAULT);
      ec != 0) {
    return unexpected(ec);
  }
  if (auto ec = FT_Get_Glyph(face.handle()->glyph, &gl)) {
    return unexpected(ec);
  }
  return ft_font_glyph(gl, gl_index);
}
} // namespace
} // namespace cgui

#endif

//
// Created by rvons on 2024-07-07.
//

#ifndef COMPONENT_GUI_FT_FONTS_HPP
#define COMPONENT_GUI_FT_FONTS_HPP

#include <bit>
#include <cassert>
#include <ranges>
#include <utility>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <cgui/cgui-types.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {
namespace extend {
constexpr decltype(auto) tl_x(bp::cvref_type<FT_BBox> auto &&v) {
  return std::forward<decltype(v)>(v).xMin;
}
constexpr decltype(auto) tl_y(bp::cvref_type<FT_BBox> auto &&v) {
  return std::forward<decltype(v)>(v).yMin;
}
constexpr decltype(auto) br_x(bp::cvref_type<FT_BBox> auto &&v) {
  return std::forward<decltype(v)>(v).xMax;
}
constexpr decltype(auto) br_y(bp::cvref_type<FT_BBox> auto &&v) {
  return std::forward<decltype(v)>(v).yMax;
}
} // namespace extend

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
class ft_font_face {
public:
  using point_t = font_point<std::ratio<1, 64>, FT_F26Dot6>;

private:
  static constexpr auto cleanup_ = [](FT_Face &f) { FT_Done_Face(f); };

  cleanup_object_t<decltype(cleanup_), FT_Face> v_{};
  point_t pts_ = whole_point(12);
  constexpr ft_font_face() = default;

public:
  constexpr ft_font_face(ft_font_face &&) noexcept = default;
  constexpr ft_font_face &operator=(ft_font_face &&) noexcept = default;
  static expected<ft_font_face, FT_Error>
  init(ft_font_library const &l, char const *path, FT_Long index = 0) {
    auto f = ft_font_face();
    if (auto ec = FT_New_Face(l.handle(), path, index, &f.v_.first_value());
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
};

class ft_font_glyph_bitmap {
  static constexpr auto deinit = [](FT_Glyph g) { FT_Done_Glyph(g); };
  cleanup_object_t<decltype(deinit), FT_Glyph> gl_{};
  FT_Error ec_{};

public:
  ft_font_glyph_bitmap(FT_Glyph g, FT_Vector const &pen) : gl_(g) {
    assert(gl_.first_value() != nullptr);
    if (ec_ = FT_Glyph_To_Bitmap(&gl_.first_value(), FT_RENDER_MODE_NORMAL,
                                 &pen, 0);
        ec_ != 0) {
      gl_.reset();
    }
  }
};

class ft_font_glyph_ref {
  FT_Glyph g_{};
  FT_UInt gi_{};
  constexpr ft_font_glyph_ref() = default;

public:
  friend inline expected<ft_font_glyph_ref, FT_Error> glyph(ft_font_face &face,
                                                            char v);

  constexpr ft_font_glyph_ref(FT_Glyph g, FT_UInt gi) : g_(g), gi_(gi) {}

  constexpr void render(renderer auto &&rend, int pen_x = {}, int pen_y = {}) {
    unused(g_, gi_);
    auto *bitmap_gl = g_;
    auto org = FT_Vector{};
    if (auto ec =
            FT_Glyph_To_Bitmap(&bitmap_gl, FT_RENDER_MODE_NORMAL, &org, false);
        ec != 0) {
#pragma message("This should be fixed...")
      std::abort();
    }
    assert(bitmap_gl->format == FT_GLYPH_FORMAT_BITMAP);
    auto bm_destroy = bp::deferred([bitmap_gl] { FT_Done_Glyph(bitmap_gl); });
    auto &bitmap = std::bit_cast<FT_BitmapGlyph>(bitmap_gl)->bitmap;
    using int_t = std::make_signed_t<std::size_t>;
    auto res = call::draw_pixels(rend,
        default_rect{
            {pen_x, pen_y},
            {pen_x + static_cast<int>(bitmap.width), pen_y + static_cast<int>(bitmap.rows)}},
        [&](pixel_drawer auto &&px_rend) {
          for (auto y :
               std::views::iota(int_t{}, static_cast<int_t>(bitmap.rows))) {
            for (auto x :
                 std::views::iota(int_t{}, static_cast<int_t>(bitmap.width))) {
              px_rend(default_pixel_coord{static_cast<int>(x),
                                          static_cast<int>(y)},
                      default_colour_t{255, 255, 255,
                                       bitmap.buffer[y * bitmap.pitch + x]});
            }
          }
        });
    assert(res.has_value());
  }
  constexpr void render(renderer auto &&rend, not_null<int> pen_x,
                        not_null<int> pen_y) {
    render(std::forward<decltype(rend)>(rend), *pen_x, *pen_y);
#pragma message("This is incorrect. Something is fishy about the 'advance' here")
    //*pen_x += g_->advance.x >> 6;
    //*pen_y += g_->advance.y >> 6;
    *pen_x += 20;
  }

  constexpr FT_Glyph handle() noexcept { return g_; }
};

inline expected<ft_font_glyph_ref, FT_Error> glyph(ft_font_face &face, char v) {
  auto gl_index = FT_Get_Char_Index(face.handle(), v);
  FT_Glyph gl;
  if (auto ec = FT_Load_Glyph(face.handle(), gl_index, FT_LOAD_DEFAULT);
      ec != 0) {
    return unexpected(ec);
  }
  if (auto ec = FT_Get_Glyph(face.handle()->glyph, &gl)) {
    return unexpected(ec);
  }
  return ft_font_glyph_ref(gl, gl_index);
}

inline FT_BBox
compute_string_ft_bbox(optional_like<FT_Glyph const &> auto &&glyph_get) {
  auto bbox = FT_BBox{.xMin = highest_possible,
                      .yMin = highest_possible,
                      .xMax = lowest_possible,
                      .yMax = lowest_possible};

  while (auto g_opt = glyph_get()) {
    FT_BBox glyph_bbox;

    FT_Glyph_Get_CBox(*g_opt, ft_glyph_bbox_pixels, &glyph_bbox);

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

inline void render_text(ft_font_face &f, readable_textc auto &&text,
                        renderer auto &&ren) {
  auto pen_x = int{};
  auto pen_y = int{};
  for (char c : text) {
    if (auto gle = glyph(f, c); gle.has_value()) {
      // auto gl = gle->handle();
      gle->render(ren, &pen_x, &pen_y);
      // pen_x += slot->advance.x >> 6;
    }
  }
}

} // namespace cgui

#endif

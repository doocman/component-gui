//
// Created by rvons on 2024-07-07.
//

#ifndef COMPONENT_GUI_FT_FONTS_HPP
#define COMPONENT_GUI_FT_FONTS_HPP

#include <bit>
#include <cassert>
#include <ranges>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <cgui/cgui-types.hpp>
#include <cgui/stl_extend.hpp>

#define CGUI_USE_BM_GLYPH 1

namespace cgui {
template <> struct extend_api<FT_BBox> {
  static constexpr auto &&l_x(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).xMin;
  }
  static constexpr auto &&t_y(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).yMin;
  }
  static constexpr auto &&r_x(bp::cvref_type<FT_BBox> auto &&b) noexcept {
    return std::forward<decltype(b)>(b).xMax;
  }
  static constexpr auto &&b_y(bp::cvref_type<FT_BBox> auto &&b) noexcept {
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
#if CGUI_USE_BM_GLYPH
  static constexpr auto cleanup = [](FT_Glyph g) {
    if (g != nullptr) {
      // FT_Done_Glyph(g);
    }
  };
  cleanup_object_t<decltype(cleanup), FT_Glyph> g_{};
#else
  std::vector<std::uint_least8_t> bm_;
  int bm_w_{};
  int bm_h_{};
  int adv_x{};
  int adv_y{};
#endif
  FT_Int top_{};

  constexpr ft_font_glyph() = default;

#if CGUI_USE_BM_GLYPH
  constexpr FT_Glyph &glyph() { return g_.first_value(); }
  constexpr FT_Glyph glyph() const { return g_.first_value(); }
  bool has_bitmap_glyph() const {
    return (glyph() != nullptr) && (glyph()->format == FT_GLYPH_FORMAT_BITMAP);
  }
  FT_BitmapGlyph bitmap_glyph() const {
    assert(has_bitmap_glyph());
    return std::bit_cast<FT_BitmapGlyph>(glyph());
  }
#endif

public:
  friend inline expected<ft_font_glyph, FT_Error> glyph(ft_font_face &face,
                                                        char v);

#if CGUI_USE_BM_GLYPH
  ft_font_glyph(FT_Glyph g, FT_Int top) : g_(g), top_(top) {

    auto &bitmap_gl = glyph();
    assert(bitmap_gl != nullptr);
    constexpr auto org = FT_Vector{};
    if (auto ec =
            FT_Glyph_To_Bitmap(&bitmap_gl, FT_RENDER_MODE_NORMAL, &org, true);
        ec != FT_Error{}) {
      return;
    }
    assert(bitmap_gl->format == FT_GLYPH_FORMAT_BITMAP);
  }
#else
  explicit ft_font_glyph(FT_Face f) {
    auto &bitmap = f->glyph->bitmap;
    for (auto y = 0; y < bitmap.rows; ++y) {
      for (auto x = 0; x < bitmap.width; ++x) {
        bm_.emplace_back(bitmap.buffer[x + y * bitmap.pitch]);
      }
    }
    bm_w_ = bitmap.width;
    bm_h_ = bitmap.rows;
    top_ = f->glyph->bitmap_top;
    adv_x = f->glyph->advance.x >> 6;
    adv_y = f->glyph->advance.y >> 6;

    // f->glyph->bitmap.buffer
  }
#endif

  constexpr auto base_to_top() const noexcept { return top_; }

  constexpr expected<void, FT_Error> render(renderer auto &&rend) const {
#if CGUI_USE_BM_GLYPH
    if (!has_bitmap_glyph()) {
      return unexpected(0);
    }
    auto bmg = bitmap_glyph();
    auto &bitmap = bmg->bitmap;
    call::draw_alpha(rend,
                     default_rect{{},
                                  {static_cast<int>(bitmap.width),
                                   static_cast<int>(bitmap.rows)}},
                     [&](bounding_box auto const &b, auto &&px_rend) {
                       assert(call::box_includes_box(
                           default_rect{{},
                                        {static_cast<int>(bitmap.width),
                                         static_cast<int>(bitmap.rows)}},
                           b));
                       assert(bitmap.pitch >= 0);
                       assert(static_cast<unsigned int>(bitmap.pitch) <=
                              bitmap.width);
                       for (auto y : cgui::y_view(b)) {
                         for (auto x : cgui::x_view(b)) {
                           px_rend(default_pixel_coord{static_cast<int>(x),
                                                       static_cast<int>(y)},
                                   bitmap.buffer[y * bitmap.pitch + x]);
                         }
                       }
                     });
#else
    call::draw_alpha(
        rend,
        default_rect{{}, {static_cast<int>(bm_w_), static_cast<int>(bm_h_)}},
        [&](bounding_box auto const &b, auto &&px_rend) {
          for (auto y : cgui::y_view(b)) {
            for (auto x : cgui::x_view(b)) {
              px_rend(
                  default_pixel_coord{static_cast<int>(x), static_cast<int>(y)},
                  bm_[x + y * bm_w_]);
            }
          }
        });
#endif
    return {};
  }

#if CGUI_USE_BM_GLYPH
  constexpr auto advance_x() const { return g_->advance.x >> 16; }
  constexpr auto advance_y() const { return g_->advance.y >> 16; }
  constexpr FT_Glyph handle() const noexcept { return glyph(); }

  [[nodiscard]] FT_BBox pixel_area() const noexcept {
    FT_BBox b;
    FT_Glyph_Get_CBox(handle(), ft_glyph_bbox_pixels, &b);
    return b;
  }
#else
  [[nodiscard]] constexpr auto advance_x() const { return adv_x; }
  [[nodiscard]] constexpr auto advance_y() const { return adv_y; }
  [[nodiscard]] constexpr default_rect pixel_area() const noexcept {
    return call::box_from_xywh<default_rect>(0, 0, bm_w_, bm_h_);
  }
#endif
};

class ft_font_face {
public:
  using point_t = font_point<std::ratio<1, 64>, FT_F26Dot6>;

private:
  static constexpr auto cleanup_ = [](FT_Face &f) {
    if (f != nullptr) {
      FT_Done_Face(f);
    }
  };

  cleanup_object_t<decltype(cleanup_), FT_Face> v_{};

  constexpr ft_font_face() = default;

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
  init(ft_font_library const &l, std::span<unsigned char const> data,
       FT_UInt horz_res, FT_UInt vert_res, FT_Long index = 0) {
    auto f = ft_font_face();
    if (data.size() > static_cast<std::size_t>(
                          std::numeric_limits<FT_Long>::max())) [[unlikely]] {
      return unexpected(FT_Error{});
    }
    if (auto ec = FT_New_Memory_Face(l.handle(), data.data(),
                                     static_cast<FT_Long>(data.size()), index,
                                     &f.v_.first_value());
        ec != FT_Error{}) {
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

  [[nodiscard]] constexpr FT_Face handle() const { return v_.first_value(); }

  [[nodiscard]] constexpr int full_height() const noexcept {
    return (handle()->ascender + handle()->descender) >> 6;
  }

  auto ascender() const { return handle()->ascender >> 6; }
};

expected<ft_font_glyph, FT_Error> glyph(ft_font_face const &face, char v) {
  auto gl_index = FT_Get_Char_Index(face.handle(), v);
  if (auto ec = FT_Load_Glyph(face.handle(), gl_index, FT_LOAD_DEFAULT);
      ec != 0) {
    return unexpected(ec);
  }
#if CGUI_USE_BM_GLYPH
  FT_Glyph gl;
  if (auto ec = FT_Get_Glyph(face.handle()->glyph, &gl)) {
    return unexpected(ec);
  }
  return ft_font_glyph(gl, face.handle()->glyph->bitmap_top);
#else
  if (auto ec = FT_Render_Glyph(face.handle()->glyph, FT_RENDER_MODE_NORMAL);
      ec != 0) {
    return unexpected(ec);
  }
  return ft_font_glyph(face.handle());
#endif
}
} // namespace
} // namespace cgui

#endif

//
// Created by rvons on 2024-07-07.
//

#ifndef COMPONENT_GUI_FT_FONTS_HPP
#define COMPONENT_GUI_FT_FONTS_HPP

#include <bit>
#include <cassert>
#include <ranges>
#include <utility>
#include <variant>
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
  FT_Int top_{};
  constexpr ft_font_glyph() = default;
  constexpr FT_Glyph &glyph() { return g_.first_value(); }
  constexpr FT_Glyph glyph() const { return g_.first_value(); }

public:
  friend inline expected<ft_font_glyph, FT_Error> glyph(ft_font_face &face,
                                                        char v);

  constexpr ft_font_glyph(FT_Glyph g, FT_UInt gi, FT_Int top) : g_(g), gi_(gi), top_(top) {}
  constexpr auto base_to_top() const noexcept { return top_; }

  constexpr expected<void, FT_Error> render(renderer auto &&rend) const {
    unused(g_, gi_);
    auto *bitmap_gl = glyph();
    auto org = FT_Vector{};
    if (auto ec =
            FT_Glyph_To_Bitmap(&bitmap_gl, FT_RENDER_MODE_NORMAL, &org, false);
        ec != 0) {
      return unexpected(ec);
    }
    assert(bitmap_gl->format == FT_GLYPH_FORMAT_BITMAP);
    auto bm_destroy = bp::deferred([bitmap_gl] { FT_Done_Glyph(bitmap_gl); });
    auto &bitmap = std::bit_cast<FT_BitmapGlyph>(bitmap_gl)->bitmap;
    call::draw_alpha(
        rend,
        default_rect{{},
                     {static_cast<int>(bitmap.width),
                      static_cast<int>(bitmap.rows)}},
        [&](bounding_box auto const& b, auto &&px_rend) {
          for (auto y :
               cgui::y_view(b)) {
            for (auto x :
                cgui::x_view(b)) {
              px_rend(
                  default_pixel_coord{static_cast<int>(x), static_cast<int>(y)},
                  bitmap.buffer[y * bitmap.pitch + x]);
            }
          }
        });
    return {};
  }

  constexpr auto advance_x() const {
    return g_->advance.x >> 16;
  }
  constexpr auto advance_y() const {
    return g_->advance.y >> 16;
  }

  constexpr FT_Glyph handle() const noexcept { return glyph(); }
};

inline FT_BBox pixel_area(ft_font_glyph const &g) noexcept {
  FT_BBox b;
  FT_Glyph_Get_CBox(g.handle(), ft_glyph_bbox_pixels, &b);
  return b;
}

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
  struct render_box {
    FT_Int line_width;
  };
  static constexpr auto cleanup_ = [](FT_Face &f) { FT_Done_Face(f); };

  cleanup_object_t<decltype(cleanup_), FT_Face> v_{};
  point_t pts_ = whole_point(12);
  std::vector<std::variant<glyph_to_render, render_box>> string_;
  FT_BBox bbox_{};
  FT_Int line_count_{};

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
  init(ft_font_library const &l, FT_Open_Args const &args, FT_Long index = 0) {
    auto f = ft_font_face();
    if (auto ec = FT_Open_Face(l.handle(), &args, index, &f.v_.first_value());
        ec != FT_Error()) {
      return unexpected(ec);
    }
    return f;
  }

  [[nodiscard]] constexpr FT_Face handle() const { return v_.first_value(); }

  void set_text(readable_textc auto const &text, int width, int height) {
    if constexpr (std::is_array_v<std::remove_cvref_t<decltype(text)>>) {
      set_text(std::string_view(text), width, height);
    } else {
      string_.clear();
      if (empty(text)) {
        line_count_ = 0;
        return;
      }
      string_.reserve(std::ranges::size(text) + 1);
      string_.emplace(end(string_), std::in_place_type<render_box>);
      line_count_ = 1;
      auto bbox_index = std::make_signed_t<std::size_t>{};
      unused(height);
      auto string_length = 0;
      auto len_at_ws = 0;
      for (auto last_space = ssize(string_); auto c : text) {
        if (auto gle = glyph(*this, c); gle.has_value()) {
          FT_BBox glyph_bbox;
          FT_Glyph_Get_CBox(gle->handle(), ft_glyph_bbox_pixels, &glyph_bbox);
          auto gl_adv = gle->handle()->advance.x >> 16;
          auto gl_w = std::max(gl_adv, call::width(glyph_bbox));
          bool add_char = true;
          if ((string_length + gl_w >= width) || c == '\n') {
            decltype(begin(string_)) it;
            decltype(len_at_ws) length_to_set;
            if (c == ' ' || c == '\n' || last_space == 0) {
              it =
                  string_.emplace(end(string_), std::in_place_type<render_box>);
              add_char = (c != ' ') && (c != '\n');
              length_to_set = string_length;
            } else {
              it = begin(string_) + last_space;
              *it = render_box{};
              length_to_set = len_at_ws;
            }
            assert(bbox_index < ssize(string_));
            assert(std::holds_alternative<render_box>(string_[bbox_index]));
            auto &cur_bbox = std::get<render_box>(string_[bbox_index]);
            cur_bbox.line_width = length_to_set;
            assert(string_length >= length_to_set);
            string_length -= length_to_set;
            last_space = 0;
            len_at_ws = 0;
            ++line_count_;
            bbox_index = std::distance(begin(string_), it);
          }
          if (add_char) {
            bbox_ = call::box_union(bbox_, glyph_bbox);
            if (c == ' ') {
              len_at_ws = string_length;
              last_space = ssize(string_);
            }
            string_length += gl_adv;
            string_.emplace_back(std::in_place_type<glyph_to_render>,
                                 std::move(*gle), handle()->glyph->bitmap_top);
          }
        }
      }
      assert(bbox_index < ssize(string_));
      auto &last_rb = string_[bbox_index];
      assert(std::holds_alternative<render_box>(last_rb));
      std::get<render_box>(last_rb).line_width = string_length;
      // bbox_ = compute_bbox();
    }
  }

  [[nodiscard]] constexpr int full_height() const noexcept {
    return (handle()->ascender + handle()->descender) >> 6;
  }

  auto ascender() const {
    return handle()->ascender >> 6;
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
  return ft_font_glyph(gl, gl_index, face.handle()->glyph->bitmap_top);
}
} // namespace
} // namespace cgui

#endif

//
// Created by rvons on 2024-07-07.
//

#ifndef COMPONENT_GUI_FONTS_HPP
#define COMPONENT_GUI_FONTS_HPP

#include <ft2build.h>
#include FT_FREETYPE_H

#include <cgui/cgui-types.hpp>

namespace cgui {
class font_library {
  static constexpr auto cleanup_ = [] (FT_Library& l) {
    FT_Done_FreeType(l);
  };
  FT_Library lib_{};
public:
  static  font_library init() {

  }
};
}

#endif // COMPONENT_GUI_FONTS_HPP

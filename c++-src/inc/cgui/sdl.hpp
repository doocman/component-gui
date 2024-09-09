#ifndef COMPONENT_GUI_CGUI_SDL_HPP
#define COMPONENT_GUI_CGUI_SDL_HPP

#include <array>
#include <cassert>
#include <exception>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include <SDL.h>

#include <cgui/cgui-types.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {
template <> struct extend_api<SDL_Rect> {
  static constexpr auto &&tl_x(bp::cvref_type<SDL_Rect> auto &&r) {
    return std::forward<decltype(r)>(r).x;
  }
  static constexpr auto &&tl_y(bp::cvref_type<SDL_Rect> auto &&r) {
    return std::forward<decltype(r)>(r).y;
  }
  static constexpr auto &&width(bp::cvref_type<SDL_Rect> auto &&r) {
    return std::forward<decltype(r)>(r).w;
  }
  static constexpr auto &&height(bp::cvref_type<SDL_Rect> auto &&r) {
    return std::forward<decltype(r)>(r).h;
  }
  static constexpr SDL_Rect from_xywh(int x, int y, int w, int h) {
    return {x, y, w, h};
  }
};

inline namespace {
using sdl_window_flag_t = std::uint32_t;
namespace details {

template <typename T> struct sdl_rect_wrapper {
  T r_;
  template <typename T2> static constexpr decltype(auto) handle(T2 &&r) {
    if constexpr (std::is_pointer_v<T>) {
      return *r.r_;
    } else {
      return std::forward<T2>(r).r_;
    }
  }

  template <typename T2> static constexpr decltype(auto) tl_x(T2 &&r) {
    return handle(std::forward<T2>(r)).x;
  }
  template <typename T2> static constexpr decltype(auto) tl_y(T2 &&r) {
    return handle(std::forward<T2>(r)).y;
  }
  template <typename T2> static constexpr decltype(auto) width(T2 &&r) {
    return handle(std::forward<T2>(r)).w;
  }
  template <typename T2> static constexpr decltype(auto) height(T2 &&r) {
    return handle(std::forward<T2>(r)).h;
  }
};

template <typename T> constexpr decltype(auto) handle(T &&val) {
  return std::remove_cvref_t<T>::handle(std::forward<T>(val));
}
template <typename> constexpr bool to_sdl_rect_simple = false;
template <> constexpr bool to_sdl_rect_simple<SDL_Rect> = true;
} // namespace details

template <bounding_box T>
  requires(!details::to_sdl_rect_simple<std::remove_cvref_t<T>>)
constexpr SDL_Rect to_sdl_rect(T &&v) {
  return {call::tl_x(v), call::tl_y(v), call::width(v), call::height(v)};
}
constexpr auto &&to_sdl_rect(bp::cvref_type<SDL_Rect> auto &&v) {
  return std::forward<decltype(v)>(v);
}

namespace details {
template <typename TTag, typename TType, TType tValue> struct flag_value {
  using tag_type = TTag;
  using value_type = TType;
  static constexpr value_type value = tValue;
};
} // namespace details

class sdl_context_instance {
  // bool init_ = true;
  static constexpr auto sdl_quit = [] { SDL_Quit(); };
  cleanup_object_t<decltype(sdl_quit)> init_;
  friend class sdl_context;

  sdl_context_instance() : init_(std::in_place) {}

public:
  explicit sdl_context_instance(no_auto_cleanup_t) : init_(no_auto_cleanup) {}
  sdl_context_instance(sdl_context_instance &&) noexcept = default;
  sdl_context_instance &operator=(sdl_context_instance &&) noexcept = default;
  void swap(sdl_context_instance &other) noexcept {
    std::swap(init_, other.init_);
  }
};

class sdl_context {
public:
  expected<sdl_context_instance, int> build() && {
    if (auto e = SDL_Init(0); e < 0) {
      return unexpected(e);
    }
    return sdl_context_instance{};
  }
};

class sdl_video {
  friend class sdl_window_builder;
  static constexpr auto video_quit = [] { SDL_VideoQuit(); };
  cleanup_object_t<decltype(video_quit)> init_;

public:
  explicit sdl_video(auto_cleanup_t) : init_(std::in_place) {}
  explicit sdl_video(no_auto_cleanup_t) : init_(no_auto_cleanup) {}
};

class sdl_canvas {
  static constexpr auto cleanup = [](SDL_Texture *t) {
    if (t != nullptr) {
      SDL_DestroyTexture(t);
    }
  };
  SDL_Renderer *r_;
  default_rect a_;
  cleanup_object_t<decltype(cleanup), SDL_Texture *> t_{};

  constexpr SDL_Texture *texture() const { return t_.first_value(); }

public:
  constexpr sdl_canvas(SDL_Renderer *r, default_rect const &a) : r_(r), a_(a) {}

  void present() { SDL_RenderPresent(r_); }

  constexpr expected<void, std::string>
  draw_pixels(bounding_box auto &&dest_sz, pixel_draw_callback auto &&cb) {
    // SDL_RendererInfo* rif;
    // rif->max_texture_height
    constexpr auto texture_format =
        SDL_PixelFormatEnum::SDL_PIXELFORMAT_ABGR8888;
    if (t_) {
      int w, h;
      Uint32 format;
      SDL_QueryTexture(texture(), &format, nullptr, &w, &h);
      if (w < call::width(dest_sz) || h < call::height(dest_sz)) {
        t_.reset();
      }
      assert(format == texture_format);
    }
    if (!t_) {
      t_.reset(SDL_CreateTexture(r_, texture_format,
                                 SDL_TEXTUREACCESS_STREAMING,
                                 call::width(dest_sz), call::height(dest_sz)));
      if (!t_) {
        return unexpected(SDL_GetError());
      }
      if (auto ec = SDL_SetTextureBlendMode(texture(), SDL_BLENDMODE_BLEND);
          ec != 0) {
        t_.reset();
        return unexpected(SDL_GetError());
      }
    }
    void *raw_pix_void;
    // decltype(auto) sdl_rect = to_sdl_rect(dest_sz);
    auto zero_point_texture =
        SDL_Rect(0, 0, call::width(dest_sz), call::height(dest_sz));
    int pitch_bytes;
    if (auto ec = SDL_LockTexture(texture(), &zero_point_texture, &raw_pix_void,
                                  &pitch_bytes);
        ec != 0) {
      return unexpected(SDL_GetError());
    }
    if (raw_pix_void == nullptr) {
      return unexpected(SDL_GetError());
    }
    auto raw_pixels = static_cast<default_colour_t *>(raw_pix_void);
    static_assert(sizeof(default_colour_t) == 4);

    std::fill_n(raw_pixels, call::width(dest_sz) * call::height(dest_sz),
                default_colour_t{0, 0, 0, 255});

    cb([raw_pixels, pitch = pitch_bytes / 4, &dest_sz,
        backstep = call::tl_x(dest_sz) + call::tl_y(dest_sz) * pitch_bytes / 4](
           pixel_coord auto &&coord, colour auto &&c) {
      auto x = call::x_of(coord);
      auto y = call::y_of(coord);
      unused(dest_sz);
      auto index = x + y * pitch - backstep;
      assert(index >= 0);
      assert(index <= call::height(dest_sz) * pitch);
      raw_pixels[index] = to_default_colour(c);
    });
    SDL_UnlockTexture(texture());
    decltype(auto) sdl_dest_rect = to_sdl_rect(dest_sz);
    if (auto ec =
            SDL_RenderCopy(r_, texture(), &zero_point_texture, &sdl_dest_rect);
        ec != 0) {
      return unexpected(SDL_GetError());
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(1));

    return {};
  }

  default_rect area() const { return a_; }
};

struct sdl_display_dpi {
  float hori, vert, diag;
};

class sdl_window {
  static constexpr auto deleter = [](SDL_Window *w) {
    if (w != nullptr) {
      SDL_DestroyWindow(w);
    }
  };
  std::string s_;
  cleanup_object_t<decltype(deleter), SDL_Window *> handle_;

  constexpr auto *handle() const {
    assert(handle_.has_value());
    return handle_.first_value();
  }
  SDL_Renderer *renderer() const { return SDL_GetRenderer(handle()); }

public:
  sdl_window() = default;
  explicit sdl_window(SDL_Window *w) : handle_(w) {}

  static constexpr sdl_window string_owner(std::string s) {
    auto res = sdl_window();
    res.s_ = std::move(s);
    return res;
  }

  std::string const &ctor_string() const { return s_; }
  std::string &ctor_string_mut() { return s_; }
  void take_ownership(SDL_Window *w) { handle_.reset(w); }

  [[nodiscard]] SDL_Rect area() const {
    SDL_Rect r;
    SDL_GetWindowSize(handle(), &r.w, &r.h);
    SDL_GetWindowPosition(handle(), &r.x, &r.y);
    return r;
  }

  [[nodiscard]] expected<sdl_canvas, std::string> canvas() {
    if (auto rend = SDL_GetRenderer(handle()); rend != nullptr) {
      int w, h;
      SDL_GetWindowSize(handle(), &w, &h);
      return sdl_canvas{rend, default_rect{{0, 0}, {w, h}}};
    }
    return unexpected(SDL_GetError());
  }

  [[nodiscard]] expected<sdl_display_dpi, std::string> dpi() const {
    auto display_index = SDL_GetWindowDisplayIndex(handle());
    sdl_display_dpi res;
    if (auto ec =
            SDL_GetDisplayDPI(display_index, &res.diag, &res.hori, &res.vert);
        ec != 0) {
      return unexpected(SDL_GetError());
    }
    return res;
  }
};

template <sdl_window_flag_t tValue>
using sdl_window_flag_struct_t =
    details::flag_value<sdl_window, sdl_window_flag_t, tValue>;

inline constexpr sdl_window_flag_struct_t<SDL_WINDOW_RESIZABLE>
    sdl_window_resizable;

class sdl_window_builder {
  std::string title_;
  int w_{};
  int h_{};
  std::uint32_t flags_{};

public:
  CGUI_CONSTEXPR_STRING_F sdl_window_builder(std::string t, int w, int h,
                                             std::uint32_t flags = {})
      : title_(std::move(t)), w_(w), h_(h), flags_(flags) {}

  template <sdl_window_flag_t... tFV>
  constexpr sdl_window_builder &&
  operator()(sdl_window_flag_struct_t<tFV>...) && {
    flags_ |= (tFV | ...);
    return std::move(*this);
  }
  template <sdl_window_flag_t... tFV>
  constexpr sdl_window_builder &operator()(sdl_window_flag_struct_t<tFV>...) & {
    flags_ |= (tFV | ...);
    return *this;
  }

  expected<sdl_window, std::string> build() && {
    auto happy_res = sdl_window::string_owner(std::move(title_));
    if (auto w = SDL_CreateWindow(happy_res.ctor_string().c_str(),
                                  SDL_WINDOWPOS_CENTERED,
                                  SDL_WINDOWPOS_CENTERED, w_, h_, flags_);
        w != nullptr) {
      if (auto r = SDL_GetRenderer(w); r != nullptr) {
        return sdl_window(w);
      }
      if (auto r = SDL_CreateRenderer(w, 0, 0); r != nullptr) {
        return sdl_window(w);
      }
    }
    title_ = std::move(happy_res.ctor_string_mut());
    return unexpected(std::string(SDL_GetError()));
  }
};

template <typename T> struct sdl_event_t {
  T event;
};

template <> struct sdl_event_t<void> {
  SDL_Event raw_event;
};

using sdl_generic_event = sdl_event_t<void>;
using sdl_quit_event = sdl_event_t<SDL_QuitEvent>;

template <typename T>
using sdl_event_callback_result =
    invoke_result_for_all_t<T, sdl_generic_event, sdl_quit_event>;

template <typename T>
concept sdl_event_callback =
    invocable_for_all<T, sdl_event_t<void>, sdl_quit_event>;

inline expected<sdl_context_instance, int> build(sdl_context &&ctx) {
  return std::move(ctx).build();
}
inline expected<sdl_video, int> video(sdl_context_instance const &) {
  if (auto ec = SDL_VideoInit(nullptr); ec < 0) {
    return unexpected(ec);
  }
  return sdl_video(auto_cleanup);
}
inline sdl_window_builder window(sdl_video const &, std::string title, int w,
                                 int h) {
  return {std::move(title), w, h};
}
inline expected<sdl_window, std::string> build(sdl_window_builder &&builder) {
  return std::move(builder).build();
}

inline auto switch_sdl_event(sdl_event_callback auto &&cb,
                             sdl_generic_event const &e)
    -> sdl_event_callback_result<decltype(cb)> {
  constexpr auto gen_evt = []<typename T>(T const &e) {
    return sdl_event_t<T>{e};
  };
  auto &raw_evt = e.raw_event;
  switch (static_cast<SDL_EventType>(raw_evt.type)) {
  case SDL_QUIT:
    return cb(gen_evt(raw_evt.quit));
  default:
    return cb(e);
  }
}

inline int poll_event(sdl_context_instance &, sdl_event_callback auto &&cb) {
  sdl_generic_event e{};
  auto &raw_evt = e.raw_event;
  if (SDL_PollEvent(&raw_evt) != 0) {
    switch_sdl_event(std::forward<decltype(cb)>(cb), e);
    return 1;
  }
  return 0;
}
} // namespace
} // namespace cgui

#endif // COMPONENT_GUI_CGUI_SDL_HPP

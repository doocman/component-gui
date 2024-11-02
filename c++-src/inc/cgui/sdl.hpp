#ifndef COMPONENT_GUI_CGUI_SDL_HPP
#define COMPONENT_GUI_CGUI_SDL_HPP

#include <array>
#include <cassert>
#include <exception>
#include <optional>
#include <thread>
#include <utility>
#include <variant>
#include <vector>

#include <SDL3/SDL.h>

#include <cgui/cgui-types.hpp>
#include <cgui/geometry.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {
template <typename T, typename ValT> struct sdl_rect_extend_api {
  static constexpr auto &&l_x(bp::cvref_type<T> auto &&r) {
    return std::forward<decltype(r)>(r).x;
  }
  static constexpr auto &&t_y(bp::cvref_type<T> auto &&r) {
    return std::forward<decltype(r)>(r).y;
  }
  static constexpr auto &&width(bp::cvref_type<T> auto &&r) {
    return std::forward<decltype(r)>(r).w;
  }
  static constexpr auto &&height(bp::cvref_type<T> auto &&r) {
    return std::forward<decltype(r)>(r).h;
  }
  static constexpr T from_xywh(auto x, auto y, auto w, auto h) {
    return {static_cast<ValT>(x), static_cast<ValT>(y), static_cast<ValT>(w),
            static_cast<ValT>(h)};
  }
};
template <> struct extend_api<SDL_Rect> : sdl_rect_extend_api<SDL_Rect, int> {};
template <>
struct extend_api<SDL_FRect> : sdl_rect_extend_api<SDL_FRect, float> {};

inline namespace {
using sdl_window_flag_t = std::uint32_t;
namespace details {

template <typename T> constexpr decltype(auto) handle(T &&val) {
  return std::remove_cvref_t<T>::handle(std::forward<T>(val));
}
template <typename> constexpr bool to_sdl_rect_simple = false;
template <> constexpr bool to_sdl_rect_simple<SDL_Rect> = true;
template <typename> constexpr bool to_sdl_frect_simple = false;
template <> constexpr bool to_sdl_frect_simple<SDL_FRect> = true;
} // namespace details

template <bounding_box T>
  requires(!details::to_sdl_rect_simple<std::remove_cvref_t<T>>)
constexpr SDL_Rect to_sdl_rect(T &&v) {
  if constexpr (std::is_floating_point_v<
                    std::remove_cvref_t<decltype(call::l_x(v))>>) {
    return {std::lround(call::l_x(v)), std::lround(call::t_y(v)),
            std::lround(call::width(v)), std::lround(call::height(v))};
  } else {
    return {call::l_x(v), call::t_y(v), call::width(v), call::height(v)};
  }
}
constexpr auto &&to_sdl_rect(bp::cvref_type<SDL_Rect> auto &&v) {
  return std::forward<decltype(v)>(v);
}
template <bounding_box T>
  requires(!details::to_sdl_frect_simple<std::remove_cvref_t<T>>)
constexpr SDL_FRect to_sdl_frect(T &&v) {
  return {static_cast<float>(call::l_x(v)), static_cast<float>(call::t_y(v)),
          static_cast<float>(call::width(v)),
          static_cast<float>(call::height(v))};
}
constexpr auto &&to_sdl_frect(bp::cvref_type<SDL_FRect> auto &&v) {
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
  std::optional<sdl_context_instance> build() && {
    if (auto e = SDL_Init(0); !e) {
      return std::nullopt;
    }
    return sdl_context_instance{};
  }
};

class sdl_video {
  friend class sdl_window_builder;
  static constexpr auto video_quit = [] { SDL_QuitSubSystem(SDL_INIT_VIDEO); };
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
  SDL_Window const *w_;
  cleanup_object_t<decltype(cleanup), SDL_Texture *> t_{};

  constexpr SDL_Texture *texture() const { return t_.first_value(); }

public:
  // TODO: The canvas must have it's own intermediate texture if we are to only
  // re-render parts of the GUI. Create it and update it... We may need to think
  // about the API here.
  constexpr sdl_canvas(SDL_Renderer *r, SDL_Window const *w) : r_(r), w_(w) {}

  void present() { SDL_RenderPresent(r_); }
  void flush() { SDL_RenderClear(r_); }

  constexpr expected<void, std::string>
  draw_pixels(bounding_box auto &&dest_sz, canvas_pixel_callback auto &&cb) {
    constexpr auto texture_format = SDL_PixelFormat::SDL_PIXELFORMAT_ABGR8888;
    if (t_) {
      float w, h;
      SDL_GetTextureSize(texture(), &w, &h);
      if (std::lround(w) < call::width(dest_sz) ||
          std::lround(h) < call::height(dest_sz)) {
        t_.reset();
      }
    }
    if (!t_) {
      t_.reset(SDL_CreateTexture(r_, texture_format,
                                 SDL_TEXTUREACCESS_STREAMING,
                                 call::width(dest_sz), call::height(dest_sz)));
      if (!t_) {
        return unexpected(SDL_GetError());
      }
      if (auto ec = SDL_SetTextureBlendMode(texture(), SDL_BLENDMODE_BLEND);
          !ec) {
        t_.reset();
        return unexpected(SDL_GetError());
      }
    }
    void *raw_pix_void;
    auto zero_point_texture =
        SDL_FRect(0.f, 0.f, static_cast<float>(call::width(dest_sz)),
                  static_cast<float>(call::height(dest_sz)));
    int pitch_bytes;
    auto zpt_int = to_sdl_rect(zero_point_texture);
    if (auto ec =
            SDL_LockTexture(texture(), &zpt_int, &raw_pix_void, &pitch_bytes);
        !ec) {
      return unexpected(SDL_GetError());
    }
    if (raw_pix_void == nullptr) {
      return unexpected(SDL_GetError());
    }
    auto raw_pixels = static_cast<default_colour_t *>(raw_pix_void);
    static_assert(sizeof(default_colour_t) == 4);
    auto pitch = pitch_bytes / 4;

    cb([raw_pixels, pitch, &dest_sz,
        backstep = call::l_x(dest_sz) + call::t_y(dest_sz) * pitch_bytes / 4](
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
    decltype(auto) sdl_dest_rect = to_sdl_frect(dest_sz);
    if (auto ec = SDL_RenderTexture(r_, texture(), &zero_point_texture,
                                    &sdl_dest_rect);
        !ec) {
      return unexpected(SDL_GetError());
    }

    return {};
  }

  void fill(bounding_box auto const &b, colour auto const &c) {
    decltype(auto) sdlb = copy_box<SDL_FRect>(b);
    SDL_SetRenderDrawColor(r_, call::red(c), call::green(c), call::blue(c),
                           call::alpha(c));
    SDL_RenderFillRect(r_, &sdlb);
  }

  [[nodiscard]] SDL_Rect area() const {
    // return a_;
    auto w = SDL_GetRenderWindow(r_);
    if (w == nullptr) [[unlikely]] {
      throw std::runtime_error(SDL_GetError());
    }
    SDL_Rect res;
    res.x = {};
    res.y = {};
    SDL_GetWindowSize(w, &res.w, &res.h);
    return res;
  }
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

  [[nodiscard]] constexpr auto *handle() const {
    assert(handle_.has_value());
    return handle_.first_value();
  }
  [[nodiscard]] SDL_Renderer *renderer() const {
    return SDL_GetRenderer(handle());
  }

public:
  sdl_window() = default;
  explicit sdl_window(SDL_Window *w) : handle_(w) {}

  static sdl_window string_owner(std::string s) {
    auto res = sdl_window();
    res.s_ = std::move(s);
    return res;
  }

  [[nodiscard]] std::string const &ctor_string() const { return s_; }
  [[nodiscard]] std::string &ctor_string_mut() { return s_; }
  void take_ownership(SDL_Window *w) { handle_.reset(w); }

  [[nodiscard]] SDL_Rect local_area() const {
    SDL_Rect r{};
    SDL_GetWindowSize(handle(), &r.w, &r.h);
    return r;
  }
  [[nodiscard]] SDL_Rect area() const {
    SDL_Rect r;
    SDL_GetWindowSize(handle(), &r.w, &r.h);
    SDL_GetWindowPosition(handle(), &r.x, &r.y);
    return r;
  }

  [[nodiscard]] expected<sdl_canvas, std::string> canvas() {
    if (auto rend = SDL_GetRenderer(handle()); rend != nullptr) {
      return sdl_canvas{rend, handle()};
    }
    return unexpected(SDL_GetError());
  }

  [[nodiscard]] expected<sdl_display_dpi, std::string> dpi() const {
    // auto display_index = SDL_GetDisplayForWindow(handle());
    sdl_display_dpi res; // NOLINT(*-pro-type-member-init)
    auto scale = SDL_GetWindowDisplayScale(handle());
    // TODO: figure out how we should set this... Android and IOS uses 160.
    res.hori = scale * 96;
    res.diag = scale * 96;
    res.vert = scale * 96;
    return res;
  }
};

template <sdl_window_flag_t tValue>
using sdl_window_flag_struct_t =
    details::flag_value<sdl_window, sdl_window_flag_t, tValue>;

inline constexpr sdl_window_flag_struct_t<SDL_WINDOW_RESIZABLE>
    sdl_window_resizable;
inline constexpr sdl_window_flag_struct_t<SDL_WINDOW_OPENGL> sdl_window_opengl;

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
    if (auto w =
            SDL_CreateWindow(happy_res.ctor_string().c_str(), w_, h_, flags_);
        w != nullptr) {
      if (auto r = SDL_GetRenderer(w); r != nullptr) {
        return sdl_window(w);
      }
      if (auto r = SDL_CreateRenderer(w, nullptr); r != nullptr) {
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

using sdl_generic_event = SDL_Event;  // sdl_event_t<void>;
using sdl_quit_event = SDL_QuitEvent; // sdl_event_t<SDL_QuitEvent>;
using sdl_mouse_move_event =
    SDL_MouseMotionEvent; // sdl_event_t<SDL_MouseMotionEvent>;

template <typename T>
using sdl_event_callback_result =
    invoke_result_for_all_t<T, sdl_generic_event, sdl_quit_event>;

template <typename T>
concept sdl_event_callback =
    invocable_for_all<T, sdl_event_t<void>, sdl_quit_event>;

inline std::optional<sdl_context_instance> build(sdl_context &&ctx) {
  return std::move(ctx).build();
}
inline std::optional<sdl_video> video(sdl_context_instance const &) {
  if (auto ec = SDL_InitSubSystem(SDL_INIT_VIDEO); !ec) {
    return std::nullopt;
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

inline auto switch_sdl_event(sdl_event_callback auto &&cb, SDL_Event const &e)
    -> sdl_event_callback_result<decltype(cb)> {
  constexpr auto gen_evt = []<typename T>(T const &e) { return e; };
  switch (static_cast<SDL_EventType>(e.type)) {
  case SDL_EVENT_QUIT:
    return cb(gen_evt(e.quit));
  case SDL_EVENT_MOUSE_MOTION:
    return cb(gen_evt(e.motion));
  case SDL_EVENT_MOUSE_BUTTON_UP:
    [[fallthrough]];
  case SDL_EVENT_MOUSE_BUTTON_DOWN:
    return cb(gen_evt(e.button));
  case SDL_EVENT_WINDOW_RESIZED:
    return cb(gen_evt(e.window));
  default:
    return cb(e);
  }
}

inline int poll_event(sdl_context_instance &, sdl_event_callback auto &&cb) {
  SDL_Event e;
  if (SDL_PollEvent(&e)) {
    switch_sdl_event(std::forward<decltype(cb)>(cb), e);
    return 1;
  }
  return 0;
}
} // namespace

template <> struct extend_api<SDL_MouseMotionEvent> {
  static constexpr subset_ui_events<ui_events::mouse_move>
  event_type(SDL_MouseMotionEvent const &) {
    return {};
  }
  static constexpr basic_pixel_coord<float>
  position(SDL_MouseMotionEvent const &e) {
    return {e.x, e.y};
  }
};
template <> struct extend_api<SDL_MouseButtonEvent> {
  static constexpr subset_ui_events<ui_events::mouse_button_up,
                                    ui_events::mouse_button_down>
  event_type(SDL_MouseButtonEvent const &e) {
    CGUI_ASSERT(e.type == SDL_EVENT_MOUSE_BUTTON_UP ||
                e.type == SDL_EVENT_MOUSE_BUTTON_DOWN);
    return {e.type == SDL_EVENT_MOUSE_BUTTON_UP ? ui_events::mouse_button_up
                                                : ui_events::mouse_button_down};
  }

  static constexpr mouse_buttons mouse_button(SDL_MouseButtonEvent const &e) {
    return static_cast<mouse_buttons>(e.button);
  }
  static constexpr basic_pixel_coord<float>
  position(SDL_MouseButtonEvent const &e) {
    return {e.x, e.y};
  }
};
template <> struct extend_api<SDL_WindowEvent> {
  static constexpr subset_ui_events<ui_events::window_resized,
                                    ui_events::system>
  event_type(SDL_WindowEvent const &e) {
    return e.type == SDL_EVENT_WINDOW_RESIZED ? ui_events::window_resized
                                              : ui_events::system;
  }
  static constexpr default_size_wh size_of(SDL_WindowEvent const &e) {
    return {e.data1, e.data2};
  }
};
} // namespace cgui

#endif // COMPONENT_GUI_CGUI_SDL_HPP

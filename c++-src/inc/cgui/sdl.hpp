#ifndef COMPONENT_GUI_CGUI_SDL_HPP
#define COMPONENT_GUI_CGUI_SDL_HPP

#include <cassert>
#include <exception>
#include <optional>
#include <utility>
#include <variant>

#include <SDL.h>

#include <cgui/cgui-types.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {
using sdl_window_flag_t = std::uint32_t;
namespace extend {
constexpr decltype(auto) tl_x(bp::cvref_type<SDL_Rect> auto&& r) {
  return std::forward<decltype(r)>(r).x;
}
constexpr decltype(auto) tl_y(bp::cvref_type<SDL_Rect> auto&& r) {
  return std::forward<decltype(r)>(r).y;
}
constexpr decltype(auto) width(bp::cvref_type<SDL_Rect> auto&& r) {
  return std::forward<decltype(r)>(r).w;
}
constexpr decltype(auto) height(bp::cvref_type<SDL_Rect> auto&& r) {
  return std::forward<decltype(r)>(r).h;
}
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

class sdl_window {
  static constexpr auto deleter = [](SDL_Window *w) {
    if (w != nullptr) {
      SDL_DestroyWindow(w);
    }
  };
  std::string s_;
  cleanup_object_t<decltype(deleter), SDL_Window *> handle_;

public:
  sdl_window() = default;
  explicit(false) sdl_window(SDL_Window *w) : handle_(w) {}

  static constexpr sdl_window string_owner(std::string s) {
    auto res = sdl_window();
    res.s_ = std::move(s);
    return res;
  }

  std::string const &ctor_string() const { return s_; }
  std::string &ctor_string_mut() { return s_; }
  void take_ownership(SDL_Window *w) { handle_.reset(w); }
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

  expected<sdl_window, int> build() && {
    auto happy_res = sdl_window::string_owner(std::move(title_));
    if (auto w = SDL_CreateWindow(happy_res.ctor_string().c_str(),
                                  SDL_WINDOWPOS_CENTERED,
                                  SDL_WINDOWPOS_CENTERED, w_, h_, flags_);
        w != nullptr) {
      return sdl_window(w);
    }
    title_ = std::move(happy_res.ctor_string_mut());
    return unexpected(-1);
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
inline expected<sdl_window, int> build(sdl_window_builder &&builder) {
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

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_SDL_HPP

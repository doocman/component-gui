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
namespace details {
template <typename TTag, typename TType, TType tValue> struct flag_value {
  using tag_type = TTag;
  using value_type = TType;
  static constexpr value_type value = tValue;
};
} // namespace details

class bad_sdl_result_access_exception : public std::exception {
  std::string err_;

public:
  explicit bad_sdl_result_access_exception(int) : err_(SDL_GetError()) {}
  [[nodiscard]] const char *what() const override { return err_.c_str(); }
};

struct sdl_error {
  int ec;
};

template <typename T> class sdl_result {
  std::variant<T, int> res_;

  static constexpr auto
  do_unwrap(auto &&res_type) -> decltype(cgui::forward_like<decltype(res_type)>(
                                 std::get<0>(res_type.res_))) {
    if (!res_type.has_value()) {
      assert(!res_type.res_.valueless_by_exception());
      throw bad_sdl_result_access_exception(std::get<1>(res_type.res_));
    }
    return cgui::forward_like<decltype(res_type)>(std::get<0>(res_type.res_));
  }

public:
  constexpr sdl_result() = default;
  explicit(false) constexpr sdl_result(T in) : res_(std::move(in)) {}
  explicit(false) constexpr sdl_result(sdl_error in) : res_(in.ec) {}

  T &operator*() {
    assert(std::holds_alternative<T>(res_));
    return std::get<0>(res_);
  }
  bool has_value() const noexcept { return std::holds_alternative<T>(res_); }
  T &unwrap() & { return do_unwrap(*this); }
  T &&unwrap() && { return do_unwrap(std::move(*this)); }
  T const &unwrap() const & { return do_unwrap(*this); }
  T const &&unwrap() const && { return do_unwrap(std::move(*this)); }
};

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
  sdl_result<sdl_context_instance> build() && {
    if (auto e = SDL_Init(0); e < 0) {
      return sdl_error(e);
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

  sdl_result<sdl_window> build() && {
    auto happy_res = sdl_window::string_owner(std::move(title_));
    if (auto w = SDL_CreateWindow(happy_res.ctor_string().c_str(),
                                  SDL_WINDOWPOS_CENTERED,
                                  SDL_WINDOWPOS_CENTERED, w_, h_, flags_);
        w != nullptr) {
      return sdl_window(w);
    }
    title_ = std::move(happy_res.ctor_string_mut());
    return sdl_error(-1);
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

inline sdl_result<sdl_context_instance> build(sdl_context &&ctx) {
  return std::move(ctx).build();
}
inline sdl_result<sdl_video> video(sdl_context_instance const &) {
  if (auto ec = SDL_VideoInit(nullptr); ec < 0) {
    return sdl_error{ec};
  }
  return sdl_video(auto_cleanup);
}
inline sdl_window_builder window(sdl_video const &, std::string title, int w,
                                 int h) {
  return {std::move(title), w, h};
}
inline sdl_result<sdl_window> build(sdl_window_builder &&builder) {
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

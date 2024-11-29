#ifndef COMPONENT_GUI_CGUI_SDL_HPP
#define COMPONENT_GUI_CGUI_SDL_HPP

#include <array>
#include <cassert>
#include <cmath>
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
  static constexpr T from_xywh(std::convertible_to<ValT> auto x,
                               std::convertible_to<ValT> auto y,
                               std::convertible_to<ValT> auto w,
                               std::convertible_to<ValT> auto h) {
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

class sdl_canvas_renderer;

class sdl_canvas {
  friend class sdl_canvas_renderer;
  static constexpr auto cleanup = [](SDL_Texture *t) {
    if (t != nullptr) {
      SDL_DestroyTexture(t);
    }
  };
  SDL_Renderer *r_;
  SDL_Window *w_;
  cleanup_object_t<decltype(cleanup), SDL_Texture *> t_{};
  cleanup_object_t<decltype(cleanup), SDL_Texture *> t2_{};
  default_colour_t clear_colour_{0, 0, 0, 255};
  bool render_prepared_{};

  constexpr SDL_Texture *texture() const { return t_.first_value(); }
  constexpr SDL_Texture *tmp_texture() const { return t2_.first_value(); }

public:
  auto pixel_scale() const { return SDL_GetWindowDisplayScale(w_); }

  constexpr sdl_canvas(SDL_Renderer *r, SDL_Window *w) : r_(r), w_(w) {}

  void present() { SDL_RenderPresent(r_); }
  void clear() {
    SDL_SetRenderDrawColor(r_, clear_colour_.red, clear_colour_.green,
                           clear_colour_.blue, clear_colour_.alpha);
    SDL_RenderClear(r_);
  }

  template <typename... Args, has_render<sdl_canvas_renderer, Args...> UI>
  inline auto render_to(UI &&ui, Args &&...args);

  [[nodiscard]] pixel_unit_t<SDL_Rect> pixel_area() const {
    auto w = SDL_GetRenderWindow(r_);
    if (w == nullptr) [[unlikely]] {
      throw std::runtime_error(SDL_GetError());
    }
    SDL_Rect res;
    res.x = {};
    res.y = {};
    SDL_GetWindowSizeInPixels(w, &res.w, &res.h);
    return pixel_unit(res);
  }
};

class sdl_canvas_renderer {
  sdl_canvas *p_;

  constexpr auto const &renderer() const { return p_->r_; }
  constexpr auto &texture_wrap() const { return p_->t_; }
  constexpr auto const &texture() const { return texture_wrap().first_value(); }
  constexpr auto &tmp_texture_wrap() { return p_->t2_; }
  constexpr std::add_pointer_t<SDL_Texture> &tmp_texture() {
    return tmp_texture_wrap().first_value();
  }

  static bool update_texture_sz(auto &to_update, int w_in, int h_in,
                                SDL_Renderer *r, SDL_TextureAccess access,
                                auto &&pred) {

    if (to_update) {
      float w, h;
      SDL_GetTextureSize(to_update.first_value(), &w, &h);
      if (!pred(std::lround(w), w_in) || !pred(std::lround(h), h_in)) {
        to_update.reset();
      }
    }
    if (!to_update) {
      to_update.reset(SDL_CreateTexture(r, texture_format, access, w_in, h_in));
      if (!to_update) {
        throw std::runtime_error(SDL_GetError());
      }
      if (auto ec = SDL_SetTextureBlendMode(to_update.first_value(),
                                            SDL_BLENDMODE_BLEND);
          !ec) {
        to_update.reset();
        throw std::runtime_error(SDL_GetError());
      }
      return true;
    }
    return false;
  }

  static expected<void, std::string> do_draw_pixels(SDL_Rect const &sdl_dest,
                                                    auto const &true_dest,
                                                    SDL_Texture *texture,
                                                    auto &&cb) {
    void *raw_pix_void;
    int pitch_bytes;
    if (auto ec =
            SDL_LockTexture(texture, &sdl_dest, &raw_pix_void, &pitch_bytes);
        !ec) {
      return unexpected(SDL_GetError());
    }
    if (raw_pix_void == nullptr) {
      return unexpected(SDL_GetError());
    }
    auto raw_pixels = static_cast<default_colour_t *>(raw_pix_void);
    static_assert(sizeof(default_colour_t) == 4);
    auto pitch = pitch_bytes / 4;

    cb([raw_pixels, pitch, &true_dest,
        backstep = call::l_x(true_dest) + call::t_y(true_dest) * pitch_bytes /
                                              4](pixel_coordinate auto &&coord,
                                                 colour auto &&c) {
      auto x = call::x_of(coord.value());
      auto y = call::y_of(coord.value());
      auto index = x + y * pitch - backstep;
      CGUI_ASSERT(index >= 0);
      CGUI_ASSERT(index <= call::height(true_dest) * pitch);
      unused(true_dest);
      raw_pixels[index] = to_default_colour(c);
    });
    SDL_UnlockTexture(texture);
    return {};
  }
  static constexpr auto texture_format =
      SDL_PixelFormat::SDL_PIXELFORMAT_ABGR8888;

public:
  auto pixel_area() const { return p_->pixel_area(); }
  auto pixel_scale() const { return p_->pixel_scale(); }

  explicit sdl_canvas_renderer(sdl_canvas &parent) : p_(&parent) {
    auto window_area = pixel_area();
    bool new_texture =
        update_texture_sz(texture_wrap(), call::width(window_area).value(),
                          call::height(window_area).value(), renderer(),
                          SDL_TEXTUREACCESS_TARGET, std::equal_to{});
    if (!SDL_SetRenderTarget(renderer(), texture())) {
      throw std::runtime_error(SDL_GetError());
    }
    if (new_texture) {
      fill(window_area, p_->clear_colour_);
    }
  }
  sdl_canvas_renderer(sdl_canvas_renderer const &) = delete;
  sdl_canvas_renderer &operator=(sdl_canvas_renderer const &) = delete;
  ~sdl_canvas_renderer() {
    SDL_SetRenderTarget(renderer(), nullptr);
    SDL_RenderTexture(renderer(), texture(), nullptr, nullptr);
  }
  void fill(pixel_rect auto const &bin, colour auto const &c) {
    auto px_area = pixel_area();
    auto sdlb = box_intersection<SDL_FRect>(bin.value(), px_area.value());
    CGUI_ASSERT(valid_box(sdlb));
    // CGUI_ASSERT(box_includes_box(px_area, b));
    // decltype(auto) sdlb = copy_box<SDL_FRect>(b.value());
    SDL_SetRenderDrawColor(renderer(), call::red(c), call::green(c),
                           call::blue(c), call::alpha(c));
    if (!SDL_RenderFillRect(renderer(), &sdlb)) {
      throw std::runtime_error(SDL_GetError());
    }
  }
  expected<void, std::string> draw_pixels(pixel_unit_t<SDL_Rect> const &dest_sz,
                                          canvas_pixel_callback auto &&cb) {
    CGUI_ASSERT(box_includes_box(pixel_area(), dest_sz));
    auto const &sdl_dest = dest_sz.value();
    // Render to temporary texture implementation
    update_texture_sz(tmp_texture_wrap(), call::width(sdl_dest),
                      call::height(sdl_dest), renderer(),
                      SDL_TEXTUREACCESS_STREAMING, std::greater_equal{});
    auto zero_point_pos = move_tl_to(sdl_dest, default_coordinate{});
    if (auto ec = do_draw_pixels(zero_point_pos, sdl_dest, tmp_texture(), cb);
        !ec) {
      return ec;
    }
    auto zp_f = to_sdl_frect(zero_point_pos);
    auto dest_f = to_sdl_frect(sdl_dest);
    if (!SDL_RenderTexture(renderer(), tmp_texture(), &zp_f, &dest_f)) {
      return unexpected(SDL_GetError());
    }
    return {};
  }
};

template <typename... Args, has_render<sdl_canvas_renderer, Args...> UI>
inline auto sdl_canvas::render_to(UI &&ui, Args &&...args) {
  return ui.render(sdl_canvas_renderer(*this), std::forward<Args>(args)...);
}

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

  template <typename... Ts> void pixel_to_points(Ts *...vals) const {
    auto sc = SDL_GetWindowDisplayScale(handle());
    unused((*vals = static_cast<Ts>(std::lround(*vals / sc)))...);
  }

public:
  sdl_window() = default;
  explicit sdl_window(SDL_Window *w) : handle_(w) {
    CGUI_ASSERT(w != nullptr);
    SDL_ShowWindow(w);
  }

  static sdl_window string_owner(std::string s) {
    auto res = sdl_window();
    res.s_ = std::move(s);
    return res;
  }

  [[nodiscard]] std::string const &ctor_string() const { return s_; }
  [[nodiscard]] std::string &ctor_string_mut() { return s_; }
  void take_ownership(SDL_Window *w) { handle_.reset(w); }

  [[nodiscard]] point_unit_t<SDL_Rect> local_area() const {
    SDL_Rect r{};
    SDL_GetWindowSize(handle(), &r.w, &r.h);
    pixel_to_points(&r.w, &r.h);
    return {{}, r};
  }
  [[nodiscard]] point_unit_t<SDL_Rect> area() const {
    SDL_Rect r;
    SDL_GetWindowSize(handle(), &r.w, &r.h);
    SDL_GetWindowPosition(handle(), &r.x, &r.y);
    pixel_to_points(&r.x, &r.y, &r.w, &r.h);
    return {{}, r};
  }

  [[nodiscard]] expected<sdl_canvas, std::string> renderer() {
    if (auto rend = SDL_GetRenderer(handle()); rend != nullptr) {
      return sdl_canvas{rend, handle()};
    }
    return unexpected(SDL_GetError());
  }

  [[nodiscard]] expected<sdl_display_dpi, std::string> dpi() const {
    auto scale = SDL_GetWindowDisplayScale(handle());
    // TODO: figure out how we should set this... Android and IOS uses 160.
    constexpr auto platform_dpi = 96.f;
    sdl_display_dpi res{scale * platform_dpi, scale * platform_dpi,
                        scale * platform_dpi};
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
    if (auto w = SDL_CreateWindow(happy_res.ctor_string().c_str(), w_, h_,
                                  flags_ | SDL_WINDOW_HIDDEN);
        w != nullptr) {
      auto pxs = SDL_GetWindowDisplayScale(w);
      if (pxs == 0.f) {
        title_ = std::move(happy_res.ctor_string_mut());
        return unexpected(std::string(SDL_GetError()));
      }
      SDL_SetWindowSize(w, static_cast<int>(std::lround(pxs * w_)),
                        static_cast<int>(std::lround(pxs * h_)));
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

template <typename Out, typename In> constexpr Out int_cast(In const &in) {
  if constexpr (std::is_integral_v<Out> && !std::is_integral_v<In>) {
    return static_cast<Out>(std::lround(in));
  } else {
    return static_cast<Out>(in);
  }
}

template <is_point_sized T, typename TX, typename TY>
T scaled_point(TX x, TY y, auto win_id) {
  auto *w = SDL_GetWindowFromID(win_id);
  auto s = SDL_GetWindowDisplayScale(w);
  return {int_cast<TX>(x / s), int_cast<TY>(y / s)};
}
} // namespace

struct sdl_event_extend_api {
  // TODO: SDL should have its own clock with SDL_GetTicks to be correct.
  using time_point_t = std::chrono::steady_clock::time_point;
  static constexpr time_point_t time_stamp(auto const &e)
    requires(requires() { e.timestamp; })
  {
    return time_point_t(std::chrono::nanoseconds(e.timestamp));
  }
};

template <> struct extend_api<SDL_MouseMotionEvent> : sdl_event_extend_api {
  static constexpr subset_input_events<input_events::mouse_move>
  event_type(SDL_MouseMotionEvent const &) {
    return {};
  }
  static point_unit_t<basic_coordinate<float>>
  position(SDL_MouseMotionEvent const &e) {
    return scaled_point<point_unit_t<basic_coordinate<float>>>(e.x, e.y,
                                                               e.windowID);
  }
};
template <> struct extend_api<SDL_MouseButtonEvent> : sdl_event_extend_api {
  static constexpr subset_input_events<input_events::mouse_button_up,
                                       input_events::mouse_button_down>
  event_type(SDL_MouseButtonEvent const &e) {
    CGUI_ASSERT(e.type == SDL_EVENT_MOUSE_BUTTON_UP ||
                e.type == SDL_EVENT_MOUSE_BUTTON_DOWN);
    return {e.type == SDL_EVENT_MOUSE_BUTTON_UP
                ? input_events::mouse_button_up
                : input_events::mouse_button_down};
  }

  static constexpr mouse_buttons mouse_button(SDL_MouseButtonEvent const &e) {
    return static_cast<mouse_buttons>(e.button);
  }
  static point_unit_t<basic_coordinate<float>>
  position(SDL_MouseButtonEvent const &e) {
    return scaled_point<point_unit_t<basic_coordinate<float>>>(e.x, e.y,
                                                               e.windowID);
  }
};
template <> struct extend_api<SDL_WindowEvent> : sdl_event_extend_api {
  static constexpr subset_input_events<input_events::window_resized,
                                       input_events::system>
  event_type(SDL_WindowEvent const &e) {
    return e.type == SDL_EVENT_WINDOW_RESIZED ? input_events::window_resized
                                              : input_events::system;
  }
  static default_point_size_wh size_of(SDL_WindowEvent const &e) {
    return scaled_point<default_point_size_wh>(e.data1, e.data2, e.windowID);
  }
};
} // namespace cgui

#endif // COMPONENT_GUI_CGUI_SDL_HPP

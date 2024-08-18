#ifndef COMPONENT_GUI_CGUI_HPP
#define COMPONENT_GUI_CGUI_HPP

#include <tuple>
#include <type_traits>

#include <cgui/cgui-types.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {

struct dummy_renderer {};
static_assert(renderer<dummy_renderer>);

template <canvas T> class cgui_renderer {
  T *c_;

public:
  constexpr explicit cgui_renderer(T &c) : c_(std::addressof(c)) {}

  constexpr auto draw_pixels(bounding_box auto &&dest,
                             pixel_draw_callback auto &&cb) {
    return call::draw_pixels(*c_, std::forward<decltype(dest)>(dest),
                             std::forward<decltype(cb)>(cb));
  }
};

template <typename T, typename TRender>
concept widget_display = has_render_direct<T, TRender>;

template <typename T>
concept widgety = widget_display<T, dummy_renderer>;

template <canvas T, widgety... TWidgets> class gui_context {
  std::tuple<TWidgets...> widgets_;

public:
  explicit constexpr gui_context(T const &, TWidgets... ws)
      : widgets_(std::move(ws)...) {}
  explicit constexpr gui_context(TWidgets... ws) : widgets_(std::move(ws)...) {}
  explicit constexpr gui_context(std::tuple<TWidgets...> ws)
      : widgets_(std::move(ws)) {}

  [[nodiscard]] default_rect area() const { return {}; }

  template <widgety... TW2>
  [[nodiscard]] constexpr gui_context<T, TWidgets..., TW2...>
  with(TW2 &&...ws) && {
    unused(ws...);
    return gui_context<T, TWidgets..., TW2...>(std::tuple_cat(
        std::move(widgets_), std::tuple<TW2...>(std::forward<TW2>(ws)...)));
  }
  template <widgety... TW2>
  [[nodiscard]] constexpr gui_context<T, TWidgets..., TW2...>
  with(TW2 &&...ws) const & {
    unused(ws...);
    return gui_context<T, TWidgets..., TW2...>(
        std::tuple_cat(widgets_, std::tuple<TW2...>(std::forward<TW2>(ws)...)));
  }

  constexpr void render_direct(T &c) {
    auto r = cgui_renderer(c);
    tuple_for_each([&r](auto &&v) { call::render_direct(v, r); }, widgets_);
  }
};

template <typename T> gui_context(T &) -> gui_context<std::remove_cvref_t<T>>;

template <typename TDisplay = void, bounding_box TArea = cgui::default_rect>
class widget {
  TDisplay display_;
  TArea area_{};

public:
  constexpr widget()
    requires(std::is_default_constructible_v<TDisplay>)
  = default;
  constexpr explicit widget(TDisplay d) : display_(std::move(d)) {}
  constexpr widget(TDisplay d, TArea const &a)
      : display_(std::move(d)), area_(a) {}

  [[nodiscard]] TArea const &area() const { return area_; }
  template <bounding_box TArea2>
  widget<TDisplay, TArea2> area(TArea2 const &a) && {
    return {std::move(display_), a};
  }
  widget display(auto &&...vs) && requires(requires() {
    call::set_displayed(display_, std::forward<decltype(vs)>(vs)...);
  }) {
    call::set_displayed(display_, std::forward<decltype(vs)>(vs)...);
    return std::move(*this);
  } void render_direct(renderer auto &&r) {
    // display_.render_direct(std::forward<decltype(r)>(r));
    call::render_direct(display_, std::forward<decltype(r)>(r),
                        call::width(area()), call::height(area()));
  }
};

template <text2render Txt> class text_display {
  Txt t_;

public:
  constexpr explicit text_display(Txt t) : t_(std::move(t)) {}
  constexpr text_display()
    requires(std::is_default_constructible_v<Txt>)
  = default;

  void render_direct(renderer auto &&r, int width, int height) {
    call::render_text(t_, r, width, height);
  }
  void set_displayed(readable_textc auto &&s) {
    call::set_text(t_, std::forward<decltype(s)>(s));
  }
};

template <text2render Txt, bounding_box TArea = default_rect>
constexpr widget<text_display<Txt>, TArea> text_box_widget(Txt t,
                                                           TArea a = {}) {
  return widget<text_display<Txt>, TArea>(text_display<Txt>(std::move(t)),
                                          std::move(a));
}

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_HPP

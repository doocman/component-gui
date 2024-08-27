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

template <canvas T, bounding_box TB> class sub_renderer {
  T *c_;
  TB area_;

  using x_t = decltype(call::tl_x(area_));
  using y_t = decltype(call::tl_y(area_));

  constexpr auto relative_area() const {
    return call::box_from_xywh<TB>(x_t{}, y_t{}, call::width(area_),
                                   call::height(area_));
  }

  template <bounding_box TB2> constexpr auto relative_area(TB2 const &b) const {
    return call::box_from_xywh(call::tl_x(b) + call::tl_x(area_),
                               call::tl_y(b) + call::tl_y(area_),
                               call::width(b), call::height(b));
  }

public:
  constexpr sub_renderer(T &c, TB a) : c_(std::addressof(c)), area_(a) {}

  template <bounding_box TB2, pixel_draw_callback TCB>
  constexpr auto draw_pixels(TB2 const &dest, TCB &&cb) const {
    // assert(call::box_includes_box(area_, dest));
    return call::draw_pixels(
        *c_, dest,
        [cb = bp::as_forward(std::forward<decltype(cb)>(cb)),
         real_dest = call::box_intersection<TB2>(relative_area(dest), area_)](
            auto &&drawer) {
          std::invoke(*cb, real_dest, std::forward<decltype(drawer)>(drawer));
        });
  }
};

template <typename T, typename TRender>
concept widget_display = has_render<T, TRender>;

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

  constexpr void render(T &c) {
    auto r = sub_renderer(c);
    tuple_for_each([&r](auto &&v) { call::render(v, r); }, widgets_);
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
    call::set_displayed(display_, call::width(area()), call::height(area()),
                        std::forward<decltype(vs)>(vs)...);
  }) {
    call::set_displayed(display_, call::width(area()), call::height(area()),
                        std::forward<decltype(vs)>(vs)...);
    return std::move(*this);
  } void render(renderer auto &&r) {
    // display_.render(std::forward<decltype(r)>(r));
    call::render(display_, std::forward<decltype(r)>(r), call::width(area()),
                 call::height(area()));
  }
};

template <text2render Txt> class text_display {
  Txt t_;

public:
  constexpr explicit text_display(Txt t) : t_(std::move(t)) {}
  constexpr text_display()
    requires(std::is_default_constructible_v<Txt>)
  = default;

  void render(renderer auto &&r, int width, int height) {
    call::render_text(t_, r, width, height);
  }
  void set_displayed(int w, int h, readable_textc auto &&s) {
    call::set_text(t_, std::forward<decltype(s)>(s), w, h);
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

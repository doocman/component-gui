#ifndef COMPONENT_GUI_CGUI_HPP
#define COMPONENT_GUI_CGUI_HPP

#include <type_traits>
#include <tuple>

#include <cgui/fonts.hpp>
#include <cgui/cgui-types.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {
template <typename T>
concept renderer = true;

struct dummy_renderer {};
static_assert(renderer<dummy_renderer>);

template <typename T>
concept widgety = requires(T& t, dummy_renderer& r) {
  t.render(r);
};

template <typename T, widgety... TWidgets>
class gui_context {
  std::tuple<TWidgets...> widgets_;

public:
  explicit constexpr gui_context(T const&, TWidgets... ws) : widgets_(std::move(ws)...) {}
  explicit constexpr gui_context(TWidgets... ws) : widgets_(std::move(ws)...) {}
  explicit constexpr gui_context(std::tuple<TWidgets...> ws) : widgets_(std::move(ws)) {}

  [[nodiscard]] rect area() const {
    return {};
  }

  template <widgety... TW2>
  [[nodiscard]] constexpr gui_context<T, TWidgets..., TW2...> with(TW2... ws)&& {
    unused(ws...);
    return gui_context<T, TWidgets..., TW2...>(std::tuple_cat(std::move(widgets_), std::tuple(std::move(ws)...)));
  }
  template <widgety... TW2>
  [[nodiscard]] constexpr gui_context<T, TWidgets..., TW2...> with(TW2... ws) const& {
    unused(ws...);
    return gui_context<T, TWidgets..., TW2...>(std::tuple_cat(widgets_, std::tuple(std::move(ws)...)));
  }

  constexpr void render() {
    tuple_for_each([] (auto && v) {

    }, widgets_);

    //dooc::tuple_for_each([] (auto&& v) {
    //  dummy_renderer r;
    //  v.render(r);
    //}, widgets_);
  }
};

template <typename T>
gui_context(T&) -> gui_context<std::remove_cvref_t<T>>;

template <typename T>
concept widget_display = requires(T& t, dummy_renderer& r) {
  t.do_render(r);
};

template <typename TDisplay = void>
class widget {
  TDisplay display_;
public:
  [[nodiscard]] rect area() const {
    return {};
  }
  widget& area(rect) {
    return *this;
  }
  widget& display(auto&&) {
    return *this;
  }
  void render(renderer auto&& r) requires(widget_display<TDisplay>) {
    display_.do_render(std::forward<decltype(r)>(r));
  }
};

class text_display {
public:
  void do_render(renderer auto&& r) {

  }
};

constexpr widget<text_display> text_box_widget() {
  return {};
}

}

#endif // COMPONENT_GUI_CGUI_HPP

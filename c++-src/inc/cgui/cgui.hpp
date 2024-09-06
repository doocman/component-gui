#ifndef COMPONENT_GUI_CGUI_HPP
#define COMPONENT_GUI_CGUI_HPP

#include <algorithm>
#include <memory>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

#include <cgui/cgui-types.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/stl_extend.hpp>

namespace cgui {

template <canvas T, bounding_box TB> class sub_renderer {
  T *c_;
  TB area_;
  using x_t = decltype(call::tl_x(area_));
  using y_t = decltype(call::tl_y(area_));
  x_t offset_x{};
  y_t offset_y{};
  default_colour_t set_colour_{};

  constexpr auto relative_area() const {
    return call::box_from_xywh<TB>(x_t{}, y_t{}, call::width(area_),
                                   call::height(area_));
  }

  template <bounding_box TB2> constexpr auto absolute_area(TB2 const &b) const {
    return call::box_from_xywh<TB2>(call::tl_x(b) + offset_x,
                                    call::tl_y(b) + offset_y, call::width(b),
                                    call::height(b));
  }

public:
  constexpr sub_renderer(T &c, TB a, x_t x, y_t y, default_colour_t sc)
      : c_(std::addressof(c)), area_(a), offset_x(x), offset_y(y),
        set_colour_(sc) {}
  constexpr sub_renderer(T &c, TB a) : sub_renderer(c, a, 0, 0, {}) {}
  constexpr explicit sub_renderer(T &c) : sub_renderer(c, call::area(c)) {}

  template <bounding_box TB2, pixel_draw_callback TCB>
  constexpr auto draw_pixels(TB2 const &dest, TCB &&cb) const {
    auto relative_dest = call::box_intersection<TB2>(dest, area_);
    auto absolute_dest =
        call::nudge_right(call::nudge_down(relative_dest, offset_y), offset_x);
    return call::draw_pixels(
        *c_, absolute_dest,
        [cb = bp::as_forward(std::forward<decltype(cb)>(cb)), relative_dest,
         offset_x = offset_x, offset_y = offset_y](auto &&drawer) {
          std::invoke(
              *cb, relative_dest,
              [d = bp::as_forward(std::forward<decltype(drawer)>(drawer)),
               offset_x, offset_y](pixel_coord auto &&px, colour auto &&col) {
                auto absolute_pos =
                    call::nudge_right(call::nudge_down(px, offset_y), offset_x);
                std::invoke(*d, absolute_pos, col);
              });
        });
  }

  void draw_alpha(bounding_box auto &&b, auto &&cb) {
    draw_pixels(std::forward<decltype(b)>(b),
                [this, &cb](auto &&bbox, auto &&drawer) {
                  cb(bbox, [this, &drawer](auto &&point, auto &&alpha) {
                    drawer(point, multiply_alpha(set_colour_, alpha));
                  });
                });
  }

  constexpr sub_renderer sub(bounding_box auto &&b,
                             default_colour_t col) const {
    return {
        *c_,
        call::nudge_up(call::nudge_left(call::box_intersection<TB>(b, area_),
                                        call::tl_x(b)),
                       call::tl_y(b)),
        offset_x + call::tl_x(b), offset_y + call::tl_y(b), col};
  }

  constexpr sub_renderer sub(bounding_box auto &&b) const {
    return sub(b, set_colour_);
  }

  constexpr sub_renderer with(default_colour_t c) {
    auto res = *this;
    res.set_colour_ = c;
    return res;
  }

  constexpr TB const &area() const { return area_; }
};

template <typename T, typename TB> sub_renderer(T &, TB) -> sub_renderer<T, TB>;
template <typename T>
sub_renderer(T &t)
    -> sub_renderer<T, std::remove_cvref_t<decltype(call::area(t))>>;

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
  widget display(auto &&...vs) &&
    requires(requires() {
      call::set_displayed(display_, call::width(area()), call::height(area()),
                          std::forward<decltype(vs)>(vs)...);
    })
  {
    call::set_displayed(display_, call::width(area()), call::height(area()),
                        std::forward<decltype(vs)>(vs)...);
    return std::move(*this);
  }
  void render(renderer auto &&r) {
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

template <font_face TFont> class cached_text_renderer {
  TFont f_;
  using glyph_t = std::remove_cvref_t<
      decltype(call::glyph(std::declval<TFont &>(), 'a').value())>;

  default_colour_t colour_{};

  struct glyph_entry {
    glyph_t g;
  };
  struct newline_entry {
    int length{};
  };

  using token_t = std::variant<newline_entry, glyph_entry>;
  std::vector<token_t> tokens_;

  int line_count_{};

  std::string text_;

public:
  constexpr explicit cached_text_renderer(TFont &&f) : f_(std::move(f)) {}
  constexpr explicit cached_text_renderer(TFont const &f) : f_(f) {}

  constexpr void set_displayed(std::string_view t, int w, int h) {
    unused(h);
    tokens_.clear();
    line_count_ = 0;
    if (size(t) == 0) {
      return;
    }
    tokens_.reserve(std::ranges::ssize(t) +
                    1); // This may be slightly less than actual used depending
                        // on line breaks.
    std::size_t cur_line_index{};
    auto current_line = [this, &cur_line_index]() -> newline_entry & {
      assert(cur_line_index < size(tokens_));
      auto &val = tokens_[cur_line_index];
      assert(std::holds_alternative<newline_entry>(val));
      return std::get<newline_entry>(val);
    };
    auto set_line = [this, &cur_line_index](auto pos) -> newline_entry & {
      assert(pos < size(tokens_));
      cur_line_index = pos;
      return tokens_[pos].template emplace<newline_entry>();
    };
    auto add_line = [this, &cur_line_index](auto... pos)
        -> std::pair<newline_entry &, decltype(tokens_.begin())> {
      if constexpr (sizeof...(pos) == 0) {
        cur_line_index = size(tokens_);
        ++line_count_;
        return {std::get<newline_entry>(
                    tokens_.emplace_back(std::in_place_type<newline_entry>)),
                end(tokens_)};
      } else {
        static_assert(sizeof...(pos) == 1);
        ++line_count_;
        cur_line_index = std::distance(begin(tokens_), pos...);
        auto new_pos =
            tokens_.emplace(pos..., std::in_place_type<newline_entry>);
        return {std::get<newline_entry>(*new_pos), new_pos};
      }
    };
    add_line();
    std::size_t last_ws{};
    int last_ws_length{};
    int last_ws_size{};
    for (auto const &c : t) {
      if (c == '\n') {
        add_line();
      } else if (auto gexp = call::glyph(f_, c)) {
        auto gbox = call::pixel_area(*gexp);
        auto gl_adv = call::advance_x(*gexp);
        using int_t =
            std::common_type_t<decltype(call::width(gbox)), decltype(gl_adv)>;
        auto gl_w = std::max<int_t>(call::width(gbox), gl_adv);
        bool add_token = true;
        if (current_line().length + gl_w > w) {
          if (c == ' ') {
            add_line();
            add_token = false;
          } else if (last_ws == 0) {
            if (auto dgexp = call::glyph(f_, '-'); dgexp) {
              auto dash_box = call::pixel_area(*dgexp);
              auto dw = call::width(dash_box);
              if (current_line().length + dw <= w) {
                current_line().length += call::width(dash_box);
                tokens_.emplace_back(glyph_entry{std::move(*dgexp)});
                add_line();
              } else {
                using namespace std::views;
                decltype(tokens_.begin()) dash_pos_native;
                auto acc_width = 0;
                {
                  auto rev_toks = reverse(tokens_);
                  auto dash_pos = rev_toks.begin();
                  auto width_threshold = w - current_line().length + dw;
                  while (true) {
                    if (dash_pos == rev_toks.end()) {
                      break;
                    }
                    if (std::holds_alternative<newline_entry>(*dash_pos)) {
                      dash_pos = rev_toks.end();
                      break;
                    }
                    assert(std::holds_alternative<glyph_entry>(*dash_pos));
                    auto gw =
                        call::advance_x(std::get<glyph_entry>(*dash_pos).g);
                    acc_width += gw;

                    auto it_pos =
                        std::distance(begin(tokens_), dash_pos.base());

                    if (acc_width >= width_threshold) {
                      ++dash_pos;
                      if (dash_pos != rev_toks.end() &&
                          std::holds_alternative<newline_entry>(*dash_pos)) {
                        dash_pos = rev_toks.end();
                      }
                      break;
                    }
                    ++dash_pos;
                  }
                  dash_pos_native = dash_pos.base();
                }
                if (dash_pos_native != begin(tokens_)) {
                  current_line().length += dw - acc_width;
                  auto nl_pos =
                      tokens_.emplace(dash_pos_native,
                                      glyph_entry{std::move(*dgexp)}) +
                      1;
                  add_line(nl_pos).first.length = acc_width;
                } else {
                  add_line();
                }
              }
            } else {
              // No dash, then we just add a new line directly.
              add_line();
            }
          } else {
            auto length_left =
                current_line().length - last_ws_length - last_ws_size;
            current_line().length = last_ws_length;
            set_line(last_ws).length = length_left;
          }
          last_ws = 0;
          last_ws_size = 0;
        }
        if (add_token) {
          if (c == ' ') {
            last_ws = size(tokens_);
            last_ws_length = current_line().length;
            last_ws_size = call::advance_x(*gexp);
          }
          tokens_.emplace_back(glyph_entry{std::move(*gexp)});
          auto &last_tok = std::get<glyph_entry>(tokens_.back());
          current_line().length += call::advance_x(last_tok.g);
        }
      }
    }
  }
  constexpr void render_text(auto &&rorg, int w,
                             int h) /*requires(glyph render...)*/ {
    auto r = rorg.with(colour_);
    // auto tl_y = (h - call::full_height(f_) * line_count_) / 2;
    CGUI_DEBUG_ONLY(bool _area_initialised{};)
    auto fh = call::full_height(f_);
    auto do_new_area = [w, base_y2 = h + 2 * call::ascender(f_), fh,
                        count = line_count_](newline_entry nl) mutable {
      auto tl_y = (base_y2 - fh * count) / 2;
      count -= 2;
      auto x = (w - nl.length) / 2;
      return call::box_from_xywh<default_rect>(x, tl_y, nl.length, fh);
    };
    decltype(do_new_area(newline_entry{})) area;
    for (auto const &t : tokens_) {
      std::visit(
          [r = rorg.with(colour_), &area, &do_new_area, fh,
           &f = f_ CGUI_DEBUG_ONLY(, &_area_initialised)]<typename T>(
              T const &tok) {
            if constexpr (std::is_same_v<T, glyph_entry>) {
              assert(_area_initialised);
              auto a2 = area;
              call::tl_y(a2, call::tl_y(a2) - call::base_to_top(tok.g));
              call::render(tok.g, r.sub(a2));
              call::trim_from_left(&area, call::advance_x(tok.g));
              call::trim_from_above(&area, call::advance_y(tok.g));
            } else if constexpr (std::is_same_v<T, newline_entry>) {
              area = do_new_area(tok);
              CGUI_DEBUG_ONLY(_area_initialised = true;)
            }
          },
          t);
    }
  }
  constexpr auto const &font() const { return f_; }
  static constexpr auto &&
  text_colour(bp::cvref_type<cached_text_renderer<TFont>> auto &&r) {
    return std::forward<decltype(r)>(r).colour_;
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

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
#include <cgui/std-backport/array.hpp>
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

  static constexpr TB bound_area(TB a) {
    if (!call::valid_box(a)) {
      call::height(a, 0);
      call::width(a, 0);
      assert(call::valid_box(a));
    }
    return a;
  }

public:
  constexpr sub_renderer(T &c, TB a, x_t x, y_t y, default_colour_t sc)
      : c_(std::addressof(c)), area_(a), offset_x(x), offset_y(y),
        set_colour_(sc) {
    assert(call::valid_box(area_));
  }
  constexpr sub_renderer(T &c, TB a) : sub_renderer(c, a, 0, 0, {}) {}
  constexpr explicit sub_renderer(T &c) : sub_renderer(c, call::area(c)) {}

  template <bounding_box TB2, pixel_draw_callback TCB>
  constexpr auto draw_pixels(TB2 const &dest, TCB &&cb) const {
    if (empty_box(dest)) {
      using return_type =
          decltype(call::draw_pixels(*c_, dest, [](auto &&...) {}));
      if constexpr (std::is_void_v<return_type>) {
        return;
      } else {
        return return_type{};
      }
    }
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
    if (empty_box(b)) {
      return;
    }
    draw_pixels(std::forward<decltype(b)>(b),
                [this, &cb](auto &&bbox, auto &&drawer) {
                  cb(bbox, [this, &drawer](auto &&point, auto &&alpha) {
                    drawer(point, multiply_alpha(set_colour_, alpha));
                  });
                });
  }

  constexpr sub_renderer sub(bounding_box auto &&b,
                             default_colour_t col) const {
    auto intersection = call::box_intersection<TB>(b, area_);
    if (call::valid_box(intersection)) {
      return {*c_,
              call::nudge_up(call::nudge_left(intersection, call::tl_x(b)),
                             call::tl_y(b)),
              offset_x + call::tl_x(b), offset_y + call::tl_y(b), col};
    } else {
      auto x = call::tl_x(area_);
      auto y = call::tl_y(area_);
      return {*c_, call::box_from_xyxy<TB>(x, y, x, y), offset_x, offset_y,
              col};
    }
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

template <canvas T, widget_display... TWidgets> class gui_context {
  std::tuple<TWidgets...> widgets_;

public:
  explicit constexpr gui_context(T const &, TWidgets... ws)
      : widgets_(std::move(ws)...) {}
  explicit constexpr gui_context(TWidgets... ws) : widgets_(std::move(ws)...) {}
  explicit constexpr gui_context(std::tuple<TWidgets...> ws)
      : widgets_(std::move(ws)) {}

  [[nodiscard]] default_rect area() const { return {}; }

  template <widget_display... TW2>
  [[nodiscard]] constexpr gui_context<T, TWidgets..., TW2...>
  with(TW2 &&...ws) && {
    unused(ws...);
    return gui_context<T, TWidgets..., TW2...>(std::tuple_cat(
        std::move(widgets_), std::tuple<TW2...>(std::forward<TW2>(ws)...)));
  }
  template <widget_display... TW2>
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

template <widget_states_aspect T> struct widget_state_wrapper : private T {
  // Inherit just for the empty base optimisation.
  using T::T;
  constexpr explicit widget_state_wrapper(T&& t) noexcept requires(std::is_move_constructible_v<T>) : T(std::move(t)) {}
  constexpr explicit widget_state_wrapper(T const& t) requires(std::is_copy_constructible_v<T>) : T(t) {}

  static constexpr decltype(auto) base(auto &&self) {
    using type =
        decltype(bp::forward_like<decltype(self)>(std::declval<T &>()));
    return static_cast<type>(std::forward<decltype(self)>(self));
  }

  using _state_marker_t = decltype(call::state(std::declval<T const &>()));
  using all_states_t = all_states_in_marker_t<_state_marker_t>;
  static constexpr all_states_t all_states() noexcept { return all_states_t{}; }

  template <typename TWH>
  using arg_t = widget_render_args<TWH, _state_marker_t>;

  template <renderer TR, display_component<TR> TD, typename TWH>
  constexpr void operator()(TD &display, TR &&r, TWH w, TWH h) const {
    call::render(display, std::forward<TR>(r), arg_t<TWH>(w, h));
  }
  template <renderer TR, typename TWH>
  constexpr auto operator()(TR &&r, TWH w, TWH h) const {
    return
        [&r,
         arg = arg_t<TWH>(
             w, h,
             call::state(base(*this)))]<display_component<TR, arg_t<TWH>> TD>(
            TD &&display) { call::render(display, std::forward<TR>(r), arg); };
  }
  static constexpr decltype(auto)
  handle(bp::cvref_type<widget_state_wrapper> auto &&self, auto &&evt,
         auto &&cb)
    requires(has_handle<decltype(base(std::forward<decltype(self)>(self))),
                        decltype(evt), decltype(cb)>)
  {
    return call::handle(base(std::forward<decltype(self)>(self)),
                        std::forward<decltype(evt)>(evt),
                        std::forward<decltype(cb)>(cb));
  }
};

struct widget_mono_state {
  static constexpr no_state_t state() noexcept { return {}; }
};

struct widget_no_event_handler {};

template <bounding_box TArea, typename TDisplay, typename TState,
          typename TEventHandler>
class widget : bp::empty_structs_optimiser<TState, TEventHandler> {
  TArea area_{};
  TDisplay display_;
  using base_t = bp::empty_structs_optimiser<TState, TEventHandler>;
  static constexpr decltype(auto) state(auto &&self) noexcept {
    return base_t::get(
        static_cast<bp::copy_cvref_t<base_t, decltype(self)>>(self),
        std::type_identity<TState>{});
  }
  static constexpr decltype(auto) event_handler(auto &&self) noexcept {
    return base_t::get(
        static_cast<bp::copy_cvref_t<base_t, decltype(self)>>(self),
        std::type_identity<TEventHandler>{});
  }

  constexpr auto set_state_callback() {
    // At this point, the state handler can change its state and propagate the
    // state change to all affected display aspects.
    return [this]<typename TS>(TS const& state) {
      auto display_cb = [&state]<typename TD>(TD& display)
      {
        if constexpr(has_set_state<TD, TS>) {
          call::set_state(display, state);
        }
      };
      call::for_each(display_, display_cb);
    };
  }

public:
  constexpr widget()
    requires(std::is_default_constructible_v<TDisplay>)
  = default;

  template <typename... Ts>
    requires(std::constructible_from<TDisplay, Ts && ...>)
  constexpr explicit widget(Ts &&...d) : display_(std::forward<Ts>(d)...) {}
  template <typename TD, typename TS, typename TE>
    requires(std::constructible_from<TDisplay, TD &&> && std::constructible_from<TEventHandler, TE&&> && std::constructible_from<TState, TS&&>)
  constexpr widget(TArea const &a, TD && d, TS && s, TE&& e)
      : base_t(std::forward<TS>(s), std::forward<TE>(e)), area_(a), display_(std::forward<TD>(d)) {}

  [[nodiscard]] TArea const &area() const { return area_; }

  widget &area(TArea const &a) {
    area_ = a;
    return *this;
  }
  constexpr TDisplay &displays() noexcept { return display_; }
  constexpr TDisplay const &displays() const noexcept { return display_; }

  constexpr void render(renderer auto &&r) const {
    call::for_each(display_,
                   state(*this)(r, call::width(area_), call::height(area_)));
  }
  constexpr void handle(auto &&evt)
    requires(has_handle<TEventHandler, TArea const&, decltype(evt),
                        decltype(set_state_callback())>)
  {
    // First level: we call the event handler that takes input events and
    // translates it to a component state change.
    call::handle(event_handler(*this), area(), std::forward<decltype(evt)>(evt),
                 [this](auto &&state_event) {
                   // Second level: event handler has taken the event input and
                   // now translates it to a behaviour event that the state
                   // aspect can read.
                   call::handle(
                       state(*this),
                       std::forward<decltype(state_event)>(state_event),
                       set_state_callback());
                 });
  }
};

namespace impl {
template <typename> constexpr bool is_tuple = false;
template <typename... Ts> constexpr bool is_tuple<std::tuple<Ts...>> = true;

constexpr decltype(auto) widget_build_or_forward(auto &&v, auto const &states) {
  if constexpr (display_component<decltype(v)>) {
    return std::forward<decltype(v)>(v);
  } else {
    return call::build(std::forward<decltype(v)>(v), states);
  }
}

template <typename... Ts, typename TStates, std::size_t... tIs>
constexpr auto build_tuple(std::tuple<Ts...> &&t, TStates const &states,
                           std::index_sequence<tIs...>)
    -> std::tuple<bp::remove_rvalue_reference_t<
        decltype(widget_build_or_forward(std::declval<Ts &&>(), states))>...> {
  return {
      widget_build_or_forward(static_cast<Ts &&>(std::get<tIs>(t)), states)...};
}
#if CGUI_HAS_NAMED_ARGS
template <typename... Ts>
constexpr bool is_tuple<dooc::named_tuple<Ts...>> = true;

template <dooc::template_string... tNames, typename... Ts>
constexpr auto
build_tuple(dooc::named_tuple<dooc::named_arg_t<tNames, Ts>...> &&t,
            auto const &states, auto &&)
    -> dooc::named_tuple<dooc::named_arg_t<
        tNames, bp::remove_rvalue_reference_t<decltype(widget_build_or_forward(
                    std::declval<Ts &&>(), states))>>...> {
  return {widget_build_or_forward(static_cast<Ts &&>(dooc::get<tNames>(t)),
                                  states)...};
}
#endif
template <typename T, typename TStates>
constexpr auto build_displays(T &&t, TStates const &states) {
  if constexpr (is_tuple<std::remove_cvref_t<T>>) {
    return build_tuple(
        std::forward<T>(t), states,
        std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<T>>>{});
  } else {
    return std::forward<T>(t);
  }
}

} // namespace impl

template <typename TArea, typename TDisplay, widget_states_aspect TState,
          typename TEventHandler>
class widget_builder_impl {
  TArea area_;
  TDisplay displays_;
  TState state_;
  TEventHandler event_;

  using state_wrapper = widget_state_wrapper<TState>;
  using state_arg_t = typename state_wrapper::template arg_t<int>;

public:
  static constexpr bool contract_fulfilled = bounding_box<TArea>;

  constexpr widget_builder_impl() = default;
  template <typename TUs, typename TS, typename TE>
    requires(std::constructible_from<TDisplay, TUs &&> && std::constructible_from<TState, TS&&> && std::constructible_from<TEventHandler, TE&&>)
  constexpr explicit widget_builder_impl(TArea const &a, TUs && displ, TS&& s, TE&& e)
      : area_(a), displays_(std::forward<TUs>(displ)), state_(std::forward<TS>(s)), event_(std::forward<TE>(e)) {}

  template <widget_states_aspect TS2, typename TRes = widget_builder_impl<
                                          TArea, TDisplay, std::remove_cvref_t<TS2>, TEventHandler>>
  TRes state(TS2 && s) && {
    return TRes(std::move(area_), std::move(displays_), std::forward<TS2>(s), std::move(event_));
  }

  template <typename... TD2, typename TTuple = std::tuple<std::unwrap_ref_decay_t<TD2>...>,
            typename TRes = widget_builder_impl<
                TArea, TTuple, TState,
                TEventHandler>>
    requires((builder_display_args<TD2, dummy_renderer, state_arg_t>) && ...)
  TRes display(TD2 &&...d2) && {
    return TRes(std::move(area_), TTuple(std::forward<decltype(d2)>(d2)...), std::move(state_), std::move(event_));
  }

  template <display_component_range<dummy_renderer,
                                    typename state_wrapper::template arg_t<int>>
                TTupleLike,
            typename TRes = widget_builder_impl<
                TArea, std::unwrap_reference_t<std::remove_cvref_t<TTupleLike>>,
                state_wrapper, TEventHandler>>
  TRes display(TTupleLike &&displays) && {
    return TRes(std::move(area_), std::forward<TTupleLike>(displays), std::move(state_), std::move(event_));
  }

#if CGUI_HAS_NAMED_ARGS
  template <dooc::arg_with_any_name... TArgs>
    requires(
        builder_display_args<typename dooc::named_arg_properties<TArgs>::type,
                             dummy_renderer, state_arg_t> &&
        ...)
  constexpr auto display(TArgs &&...args) && {
    using tuple_t = dooc::named_tuple<std::remove_cvref_t<TArgs>...>;
    return widget_builder_impl<TArea,
                               tuple_t,
                               TState, TEventHandler>(
        std::move(area_), tuple_t(std::forward<TArgs>(args)...), std::move(state_), std::move(event_));
  }
#endif

  auto build() &&
    requires(contract_fulfilled)
  {
    static_assert(bounding_box<TArea>,
                  "You must set an area to the widget before constructing it!");
    static_assert(contract_fulfilled);
    using display_t = decltype(impl::build_displays(
        std::move(displays_), state_wrapper::all_states()));
    return widget<TArea, display_t, state_wrapper, TEventHandler>(
        std::move(area_), impl::build_displays(std::move(displays_),
                                               state_wrapper::all_states()), state_wrapper(std::move(state_)), std::move(event_));
  }
  template <typename TE2,
            typename TRes = widget_builder_impl<TArea, TDisplay, TState, std::remove_cvref_t<TE2>>>
  TRes event(TE2 && e) && {
    return TRes(std::move(area_), std::move(displays_), std::move(state_), std::forward<TE2>(e));
  }

  template <bounding_box TA, typename TRes = widget_builder_impl<
                                 TA, TDisplay, TState, TEventHandler>>
  TRes area(TA const &a) && {
    return TRes(a, std::move(displays_), std::move(state_), std::move(event_));
  }
};

constexpr widget_builder_impl<std::nullptr_t, std::tuple<>, widget_mono_state,
                              widget_no_event_handler>
widget_builder() {
  return {};
}

template <font_face TFont> class text_renderer {
  TFont f_;
  using glyph_t = std::remove_cvref_t<
      decltype(call::glyph(std::declval<TFont &>(), 'a').value())>;

  default_colour_t colour_{255, 255, 255, 255};

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
  template <typename... TU>
    requires(std::constructible_from<TFont, TU && ...>)
  constexpr explicit text_renderer(TU &&...f) : f_(std::forward<TU>(f)...) {}
  template <typename... TU>
    requires(std::constructible_from<TFont, TU && ...>)
  constexpr explicit text_renderer(std::in_place_type_t<TFont>, TU &&...f)
      : f_(std::forward<TU>(f)...) {}

  constexpr void set_displayed(int w, int, std::string_view t) {
    using iterator_t = decltype(tokens_.begin());
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
    auto add_line = [this, &cur_line_index]() -> newline_entry & {
      cur_line_index = size(tokens_);
      ++line_count_;
      return std::get<newline_entry>(
          tokens_.emplace_back(std::in_place_type<newline_entry>));
    };
    auto add_line_at =
        [this, &cur_line_index](
            iterator_t pos) -> std::pair<newline_entry &, iterator_t> {
      ++line_count_;
      cur_line_index = std::distance(begin(tokens_), pos);
      auto new_pos = tokens_.emplace(pos, std::in_place_type<newline_entry>);
      return {std::get<newline_entry>(*new_pos), new_pos};
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
                  add_line_at(nl_pos).first.length = acc_width;
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
          current_line().length += call::advance_x(*gexp);
          tokens_.emplace_back(glyph_entry{std::move(*gexp)});
        }
      }
    }
  }
  constexpr void render(auto &&rorg, int w, int h)
    requires(has_render<glyph_t, decltype(rorg)>)
  {
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
          [r = rorg.with(colour_), &area,
           &do_new_area CGUI_DEBUG_ONLY(, &_area_initialised)]<typename T>(
              T const &tok) {
            if constexpr (std::is_same_v<T, glyph_entry>) {
              assert(_area_initialised);
              auto a2 = area;
              call::tl_y(a2, call::tl_y(a2) - call::base_to_top(tok.g));
              assert(call::valid_box(a2));
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
  static constexpr auto &&text_colour(bp::cvref_type<text_renderer> auto &&r) {
    return std::forward<decltype(r)>(r).colour_;
  }
};

template <typename T>
text_renderer(T &&) -> text_renderer<std::unwrap_ref_decay_t<T>>;

class fill_rect {
  default_colour_t colour_{};

public:
  constexpr fill_rect() = default;
  constexpr explicit fill_rect(default_colour_t c) : colour_(c) {}

  [[nodiscard]] constexpr auto &colour() noexcept { return colour_; }
  [[nodiscard]] constexpr auto const &colour() const noexcept {
    return colour_;
  }
  constexpr void render(renderer auto &&r, render_args auto &&args) const {
    call::fill(r, default_rect{0, 0, call::width(args), call::height(args)},
               colour_);
  }
};

template <display_component T, typename TState, TState... tStates> class display_per_state_impl {
  static constexpr auto state_count = sizeof...(tStates);
  using arr_t = std::array<T, state_count>;
  arr_t d_;

public:
  constexpr explicit display_per_state_impl(auto &&...args)
    requires(std::constructible_from<T, decltype(args)...>)
      : d_(bp::array_from_args<T, state_count>(args...)) {}
  constexpr auto render(renderer auto &&r, render_args auto const &args) const {
    unused(r, args);
    auto disp_index = state2index(args.widget_state());
    CGUI_ASSERT(disp_index < size(d_));
    auto &active_display = d_[disp_index];
    call::render(active_display, std::forward<decltype(r)>(r), args);
  }
  template <TState tI>
    requires((tI == tStates) || ...)
  static constexpr auto &&
  get(bp::cvref_type<display_per_state_impl> auto &&v) noexcept {
    return std::get<state2index(widget_state_marker<TState, tStates...>(tI))>(std::forward<decltype(v)>(v).d_);
  }
};
template <bp::ct_value_wrapper val, typename T>
  requires(requires(bp::as_forward<T> t) {
    std::remove_cvref_t<T>::template get<val.raw()>(*t);
  })
constexpr auto &&get(T &&t) noexcept {
  return std::remove_cvref_t<T>::template get<val.raw()>(std::forward<T>(t));
}

template <display_component TDC, typename... TArgs> class display_per_state {
  std::tuple<TArgs &&...> args_;

public:
  constexpr explicit display_per_state(TArgs &&...args)
      : args_(std::forward<TArgs>(args)...) {}
  template <typename T, T... tStates>
  constexpr display_per_state_impl<TDC, T, tStates...>
  build(widget_states<T, tStates...>) && {
    return std::apply(
        [](auto &&...args) {
          return display_per_state_impl<TDC, T, tStates...>(
              std::forward<decltype(args)>(args)...);
        },
        args_);
  }
};
template <display_component T>
display_per_state(T &&) -> display_per_state<std::remove_cvref_t<T>, T>;

/*
template <typename Txt, bounding_box TArea = default_rect>
constexpr auto text_box_widget(Txt t, TArea a = {})
    -> widget<decltype(text_renderer(std::move(t))), TArea> {
  return {text_renderer(std::move(t)), std::move(a)};
}*/

namespace details {
template <typename T> constexpr auto get_error(T &&t) {
  if constexpr (requires() { t.error(); }) {
    return std::forward<T>(t).error();
  } else {
    return false;
  }
}
template <typename T> constexpr bool is_error(T const &t) {
  if constexpr (requires() { t.has_value(); }) {
    return !t.has_value();
  } else if constexpr (std::constructible_from<bool, T const &>) {
    return static_cast<bool>(t);
  } else {
    // assume has value;
    unused(t);
    return true;
  }
}
} // namespace details

template <font_face TF> class cached_font {
  TF f_;
  using exp_glyph_t = std::remove_cvref_t<decltype(call::glyph(f_, char{}))>;
  using glyph_t = std::remove_cvref_t<bp::dereferenced_t<exp_glyph_t>>;
  using error_t = decltype(details::get_error(call::glyph(f_, char{})));

  struct glyph_ref_t {
    glyph_t const *g_;
    constexpr explicit glyph_ref_t(glyph_t const &g) : g_(std::addressof(g)) {}
    constexpr decltype(auto) base_to_top() const {
      return call::base_to_top(*g_);
    }
    constexpr decltype(auto) advance_x() const { return call::advance_x(*g_); }
    constexpr decltype(auto) advance_y() const { return call::advance_y(*g_); }
    constexpr decltype(auto) render(auto &&r) const {
      return call::render(*g_, std::forward<decltype(r)>(r));
    }
    constexpr decltype(auto) pixel_area() const {
      return call::pixel_area(*g_);
    }
  };
  std::vector<std::unique_ptr<exp_glyph_t>> mutable glyphs_;
  std::vector<char> mutable chars_;
  auto map_begin() const noexcept { return begin(chars_); }
  auto map_end() const noexcept { return end(chars_); }

public:
  constexpr expected<glyph_ref_t, error_t> glyph(char c) const {
    auto pos = std::lower_bound(map_begin(), map_end(), c);
    constexpr auto transformer = [](auto &g) { return glyph_ref_t(g); };
    auto gdist = std::distance(map_begin(), pos);
    if (pos == map_end() || *pos != c) {
      auto gpos = glyphs_.emplace(begin(glyphs_) + gdist,
                                  new exp_glyph_t(call::glyph(f_, c)));
      chars_.emplace(pos, c);
      return (*gpos)->transform(transformer);
    }
    return glyphs_[gdist]->transform(transformer);
  }

  template <typename... Ts>
    requires(std::constructible_from<TF, Ts && ...> &&
             !(sizeof...(Ts) == 1 &&
               (std::is_same_v<cached_font, std::remove_cvref_t<Ts>> && ...)))
  constexpr explicit(sizeof...(Ts) > 1) cached_font(Ts &&...args)
      : f_(std::forward<Ts>(args)...) {}

  constexpr decltype(auto) full_height() const { return call::full_height(f_); }
  constexpr decltype(auto) ascender() const { return call::ascender(f_); }
};
template <typename T>
cached_font(T &&) -> cached_font<std::unwrap_ref_decay_t<T>>;

class buttonlike_trigger {
  bool mouse_inside_{};
  bool mouse_down_{};

public:
  struct hover_event {};
  struct exit_event {};
  struct hold_event {};
  struct click_event {
    mouse_buttons button;
  };

  void handle(
      bounding_box auto &&area,
      event_types<ui_events::mouse_move, ui_events::mouse_button_down,
                  ui_events::mouse_button_up, ui_events::mouse_exit> auto &&evt,
      auto &&trigger_callback) {
    using enum ui_events;
    using evt_t = decltype(evt);
    if constexpr (can_be_event<mouse_exit, evt_t>()) {
      if (is_event<mouse_exit>(evt) && mouse_inside_) {
        trigger_callback(exit_event{});
        mouse_inside_ = false;
      }
    }
    if constexpr (can_be_event<mouse_move, evt_t>()) {
      if (is_event<mouse_move>(evt)) {
        // do_stuff;
        auto is_now_inside = call::hit_box(area, call::position(evt));
        if (is_now_inside != mouse_inside_) {
          if (mouse_inside_) {
            trigger_callback(exit_event{});
          } else {
            trigger_callback(hover_event{});
          }
          mouse_inside_ = is_now_inside;
        }
        return;
      }
    }
    if constexpr (can_be_event<mouse_button_down, evt_t>()) {
      if (is_event<mouse_button_down>(evt)) {
        if (!mouse_down_ && call::hit_box(area, call::position(evt))) {
          trigger_callback(hold_event{});
        }
        mouse_down_ = true;
        return;
      }
    }
    if constexpr (can_be_event<mouse_button_up, evt_t>()) {
      if (is_event<mouse_button_up>(evt)) {
        if (call::hit_box(area, call::position(evt))) {
          trigger_callback(click_event{call::mouse_button(evt)});
        }
        mouse_down_ = false;
        return;
      }
    }
  }
};

enum class momentary_button_states {
  off,
  hover,
  hold
};

template <std::invocable TClick, std::invocable THover = bp::no_op_t, std::invocable THold = bp::no_op_t, std::invocable TExit = bp::no_op_t>
struct momentary_button {
  TClick on_click;
  THover on_hover;
  THold on_hold;
  TExit on_exit;

  using state_t = widget_state_marker<momentary_button_states, momentary_button_states::off, momentary_button_states::hover, momentary_button_states::hold>;
  state_t current_state_{momentary_button_states::off};

  [[nodiscard]] constexpr state_t state() const {
    return current_state_;
  }

  constexpr void handle(buttonlike_trigger::click_event const&, auto&&) {
    on_click();
    current_state_ = momentary_button_states::hover;
    on_hover();
  }
  constexpr void handle(buttonlike_trigger::exit_event const&, auto&&) {
    current_state_ = momentary_button_states::off;
    on_exit();
  }
  constexpr void handle(buttonlike_trigger::hold_event const&, auto&&) {
    current_state_ = momentary_button_states::hold;
    on_hold();
  }
  constexpr void handle(buttonlike_trigger::hover_event const&, auto&&) {
    current_state_ = momentary_button_states::hover;
    on_hover();
  }
};

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_HPP

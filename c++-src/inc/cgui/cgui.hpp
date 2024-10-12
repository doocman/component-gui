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

template <typename TX, typename TY> class nudger {
  TX x_;
  TY y_;

public:
  constexpr nudger(TX x, TY y) : x_(x), y_(y) {}

  constexpr pixel_coord auto operator()(auto &&in) const
    requires(requires() {
      nudge_down(in, y_);
      nudge_right(in, x_);
    })
  {
    return nudge_down(nudge_right(in, x_), y_);
  }
};

template <bounding_box TB = default_rect> class recursive_area_navigator {
  TB relative_area_;
  using x_t = std::remove_cvref_t<decltype(call::l_x(relative_area_))>;
  using y_t = std::remove_cvref_t<decltype(call::t_y(relative_area_))>;
  x_t offset_x_{};
  y_t offset_y_{};

  constexpr recursive_area_navigator(TB const &b, x_t ox, y_t oy)
      : relative_area_(b), offset_x_(ox), offset_y_(oy) {}

public:
  constexpr explicit recursive_area_navigator(TB const &b)
      : relative_area_(b) {}
  template <bounding_box TB2 = TB>
  constexpr recursive_area_navigator sub(TB2 const &b) const {
    auto intersection = box_intersection<TB>(b, relative_area_);
    if (valid_box(intersection)) {
      return {nudge_up(nudge_left(intersection, call::l_x(b)), call::t_y(b)),
              offset_x_ + call::l_x(b), offset_y_ + call::t_y(b)};
    } else {
      auto x = call::l_x(relative_area_);
      auto y = call::t_y(relative_area_);
      return {box_from_xyxy<TB>(x, y, x, y), offset_x_, offset_y_};
    }
  }
  constexpr TB relative_area() const { return relative_area_; }
  template <typename TB2 = TB>
  constexpr TB2 move_to_absolute(TB2 const &b) const {
    return box_from_xywh<TB2>(call::l_x(b) + offset_x_,
                              call::t_y(b) + offset_y_, call::width(b),
                              call::height(b));
  }
  constexpr TB absolute_area() const {
    return move_to_absolute(relative_area_);
  }

  constexpr nudger<x_t, y_t> relative_to_absolute_nudger() const noexcept {
    return {offset_x_, offset_y_};
  }
};

template <canvas T, bounding_box TB> class sub_renderer {
  T *c_;
  recursive_area_navigator<TB> area_;
  default_colour_t set_colour_{};

  static constexpr TB bound_area(TB a) {
    if (!valid_box(a)) {
      call::height(a, 0);
      call::width(a, 0);
      assert(valid_box(a));
    }
    return a;
  }

  template <typename TB2>
  constexpr TB2 to_relative_dest(TB2 const &input_dest) const {
    return box_intersection<TB2>(input_dest, area_.relative_area());
  }
  template <typename TB2>
  constexpr TB2 to_absolute(TB2 const &relative_dest) const {
    return area_.move_to_absolute(relative_dest);
  }
  constexpr sub_renderer(T &c, recursive_area_navigator<TB> a,
                         default_colour_t sc)
      : c_(std::addressof(c)), area_(a), set_colour_(sc) {
    assert(valid_box(area_.relative_area()));
  }

public:
  constexpr sub_renderer(T &c, TB a)
      : sub_renderer(c, recursive_area_navigator<TB>(a), {}) {}
  constexpr explicit sub_renderer(T &c) : sub_renderer(c, call::area(c)) {}

  template <bounding_box TB2, pixel_draw_callback TCB>
  constexpr auto draw_pixels(TB2 const &dest, TCB &&cb) const {
    CGUI_ASSERT(valid_box(dest));
    auto relative_dest = to_relative_dest(dest);
    CGUI_ASSERT(valid_box(relative_dest));
    if (empty_box(relative_dest)) {
      using return_type =
          decltype(call::draw_pixels(*c_, dest, [](auto &&...) {}));
      if constexpr (std::is_void_v<return_type>) {
        return;
      } else {
        return return_type{};
      }
    }
    auto absolute_dest = to_absolute(relative_dest);
    CGUI_ASSERT(valid_box(absolute_dest));
    return call::draw_pixels(
        *c_, absolute_dest,
        [cb = bp::as_forward(std::forward<decltype(cb)>(cb)), relative_dest,
         nudge = area_.relative_to_absolute_nudger()](auto &&drawer) {
          std::invoke(
              *cb, relative_dest,
              [d = bp::as_forward(std::forward<decltype(drawer)>(drawer)),
               &nudge](pixel_coord auto &&px, colour auto &&col) {
                auto absolute_pos = nudge(px);
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
                  CGUI_ASSERT(valid_box(bbox));
                  cb(bbox, [this, &drawer](auto &&point, auto &&alpha) {
                    drawer(point, multiply_alpha(set_colour_, alpha));
                  });
                });
  }

  constexpr void fill(bounding_box auto const &dest, colour auto const &c)
    requires(has_native_fill<decltype(*c_), decltype(dest), decltype(c)>)
  {
    auto absolute_dest = to_absolute(to_relative_dest(dest));
    if (!empty_box(absolute_dest)) {
      call::fill(*c_, absolute_dest, c);
    }
  }

  constexpr sub_renderer sub(bounding_box auto &&b,
                             default_colour_t col) const {
    return {*c_, area_.sub(b), col};
  }

  constexpr sub_renderer sub(bounding_box auto &&b) const {
    return sub(b, set_colour_);
  }

  constexpr sub_renderer with(default_colour_t c) {
    auto res = *this;
    res.set_colour_ = c;
    return res;
  }

  constexpr TB area() const { return area_.relative_area(); }
};

template <typename T, typename TB> sub_renderer(T &, TB) -> sub_renderer<T, TB>;
template <typename T>
sub_renderer(T &t)
    -> sub_renderer<T, std::remove_cvref_t<decltype(call::area(t))>>;

template <bounding_box TArea = default_rect>
class widget_display_state_callbacks {
  recursive_area_navigator<TArea> full_area_;
  TArea to_rerender_{};

  explicit(false) constexpr widget_display_state_callbacks(
      recursive_area_navigator<TArea> nav)
      : full_area_(nav) {}

public:
  explicit constexpr widget_display_state_callbacks(TArea full_area)
      : full_area_(full_area) {}

  constexpr void rerender() { to_rerender_ = full_area_.relative_area(); }
  constexpr void rerender(bounding_box auto part_area) {
    if (!empty_box(to_rerender_)) {
      part_area = box_union(part_area, to_rerender_);
    }
    to_rerender_ = box_intersection(part_area, full_area_.relative_area());
  }
  TArea result_area() const {
    return full_area_.move_to_absolute(to_rerender_);
  }
  constexpr bool empty_result() const { return empty_box(to_rerender_); }
  template <bounding_box TA2 = TArea>
  constexpr widget_display_state_callbacks sub(TA2 rel_area) const {
    return {full_area_.sub(rel_area)};
  }
  template <bounding_box TA2 = TArea>
  constexpr void merge_sub(widget_display_state_callbacks<TA2> const &s) {
    if (!s.empty_result()) {
      auto sub_area = s.result_area();
      rerender(sub_area);
    }
  }
};

namespace impl {
template <typename> constexpr bool is_tuple = false;
template <typename... Ts> constexpr bool is_tuple<std::tuple<Ts...>> = true;

constexpr decltype(auto) gui_context_build_or_forward(auto &&v) {
  auto vf = bp::as_forward<decltype(v)>(v);
  if constexpr (widget_display<decltype(v)>) {
    return *vf;
  } else {
    return (*vf).build();
  }
}

template <typename... Ts, std::size_t... tIs>
constexpr auto build_context_display_tuple(std::tuple<Ts...> &&t,
                                           std::index_sequence<tIs...>) {
  return std::tuple<bp::remove_rvalue_reference_t<
      decltype(gui_context_build_or_forward(std::declval<Ts &&>()))>...>{
      gui_context_build_or_forward(std::forward<Ts>(std::get<tIs>(t)))...};
}

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
build_tuple(dooc::named_tuple<dooc::named_arg_t<tNames, Ts>...> &&t, auto &&)
    -> dooc::named_tuple<dooc::named_arg_t<
        tNames,
        bp::remove_rvalue_reference_t<decltype(gui_context_build_or_forward(
            std::declval<Ts &&>()))>>...> {
  return {gui_context_build_or_forward(
      static_cast<Ts &&>(dooc::get<tNames>(t)))...};
}

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
template <typename T> constexpr auto build_gui_context_widgets(T &&t) {
  auto tf = bp::as_forward<T>(t);
  if constexpr (widget_display_builder<T>) {
    return call::build(*tf);
  } else if constexpr (is_tuple<std::remove_cvref_t<T>>) {
    return build_context_display_tuple(
        *tf,
        std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<T>>>{});
  } else {
    return *tf;
  }
}
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

template <bounding_box TArea, typename TOnResize = bp::no_op_t,
          widget_display_range TWidgets = std::tuple<>>
class gui_context : bp::empty_structs_optimiser<TOnResize> {
  using _base_t = bp::empty_structs_optimiser<TOnResize>;
  TWidgets widgets_;

  constexpr void call_on_resize(size_wh auto const &sz) {
    gui_context::get(std::type_identity<TOnResize>{})(sz, widgets_);
  }

public:
  using native_box_t = TArea;

  template <typename... TWs>
    requires(std::constructible_from<TWidgets, TWs && ...>)
  explicit constexpr gui_context(auto const &, TWs &&...ws)
      : widgets_(std::forward<TWs>(ws)...) {}

  template <typename... TWs>
    requires(std::constructible_from<TWidgets, TWs && ...>)
  explicit constexpr gui_context(TWs &&...ws)
      : widgets_(std::forward<TWs>(ws)...) {}

  template <typename TOnRsz, typename TWs>
    requires(std::constructible_from<TOnResize, TOnRsz &&> &&
             std::constructible_from<TWidgets, TWs &&>)
  constexpr gui_context(TOnRsz &&on_rsz, TWs &&ws,
                        bounding_box auto const &start_area)
      : _base_t(std::forward<TOnRsz>(on_rsz)), widgets_(std::forward<TWs>(ws)) {
    call_on_resize(
        default_size_wh{call::width(start_area), call::height(start_area)});
  }

  template <widget_display... TW2>
  [[nodiscard]] constexpr auto with(TW2 &&...ws) && {
    unused(ws...);
    return gui_context<
        TArea,
        decltype(std::tuple_cat(
            widgets_,
            std::declval<std::tuple<std::unwrap_ref_decay_t<TW2>...>>()))>(
        std::tuple_cat(std::move(widgets_),
                       std::tuple<std::unwrap_ref_decay_t<TW2>...>(
                           std::forward<TW2>(ws)...)));
  }

  template <typename... Ts> constexpr void render(sub_renderer<Ts...> &&r) {
    cgui::fill(r, r.area(), default_colour_t{0, 0, 0, 255});
    tuple_for_each([&r](auto &&v) { call::render(v, r); }, widgets_);
  }

  constexpr void render(canvas auto &&c, bounding_box auto const &rarea) {
    render(sub_renderer(c, rarea));
  }

  constexpr void render(canvas auto &&c) { render(sub_renderer(c)); }

  constexpr native_box_t handle(auto const &evt)
  // requires((has_handle<TWidgets, decltype(evt)> || ...) ||
  // can_be_event<ui_events::window_resized, decltype(evt)>())
  {
    if constexpr (can_be_event<ui_events::window_resized, decltype(evt)>()) {
      if (is_event<ui_events::window_resized>(evt)) {
        auto sz = call::size_of(evt);
        call_on_resize(sz);
        auto ret_box = box_from_xyxy<native_box_t>(0, 0, call::width(sz),
                                                   call::height(sz));
        return ret_box;
      }
    }
    auto b = widget_display_state_callbacks(
        box_from_xyxy<native_box_t>(0, 0, highest_possible, highest_possible));
    call::for_each(widgets_, [&evt, &b]<typename TW>(TW &w) {
      if constexpr (has_handle<TW &, decltype(evt)>) {
        auto s = b.sub(w.area());
        call::handle(w, evt, s);
        b.merge_sub(s);
      }
    });
    return b.result_area();
  }
};

template <typename T> gui_context(T &) -> gui_context<std::remove_cvref_t<T>>;

template <widget_display_range TWidgets, typename TOnResize>
class gui_context_builder_impl {
  TWidgets widgets_;
  TOnResize on_resize_;

public:
  constexpr gui_context_builder_impl() = default;
  constexpr gui_context_builder_impl(auto &&w, auto &&onrsz)
      : widgets_(std::forward<decltype(w)>(w)),
        on_resize_(std::forward<decltype(onrsz)>(onrsz)) {}
  template <widget_display_args... TWs,
            typename WT = std::tuple<std::unwrap_ref_decay_t<TWs>...>>
  constexpr gui_context_builder_impl<WT, TOnResize> widgets(TWs &&...ws) && {
    return {WT(std::forward<TWs>(ws)...), std::move(on_resize_)};
  }
  template <typename TORSZ2>
  constexpr gui_context_builder_impl<TWidgets, TORSZ2>
  on_resize(TORSZ2 &&onrsz) && {
    return {std::move(widgets_), std::forward<TORSZ2>(onrsz)};
  }
  template <bounding_box TArea = default_rect,
            typename TW =
                decltype(impl::build_gui_context_widgets(std::move(widgets_)))>
  constexpr gui_context<TArea, TOnResize, TW>
  build(TArea const &start_area) && {
    return {std::move(on_resize_),
            impl::build_gui_context_widgets(std::move(widgets_)), start_area};
  }
};

constexpr gui_context_builder_impl<std::tuple<>, bp::no_op_t>
gui_context_builder() {
  return {};
}

template <widget_states_aspect T> struct widget_state_wrapper : private T {
  // Inherit just for the empty base optimisation.
  using T::T;
  constexpr explicit widget_state_wrapper(T &&t) noexcept
    requires(std::is_move_constructible_v<T>)
      : T(std::move(t)) {}
  constexpr explicit widget_state_wrapper(T const &t)
    requires(std::is_copy_constructible_v<T>)
      : T(t) {}

  static constexpr decltype(auto) base(auto &&self) {
    using type =
        decltype(bp::forward_like<decltype(self)>(std::declval<T &>()));
    return static_cast<type>(std::forward<decltype(self)>(self));
  }

  using state_marker_t = decltype(call::state(std::declval<T const &>()));
  using all_states_t = all_states_in_marker_t<state_marker_t>;
  static constexpr all_states_t all_states() noexcept { return all_states_t{}; }
  [[nodiscard]] constexpr state_marker_t state() const noexcept {
    return call::state(base(*this));
  }

  template <typename TWH> using arg_t = widget_render_args<TWH, state_marker_t>;

  template <renderer TR, display_component<TR> TD, typename TWH>
  constexpr void operator()(TD &display, TR &&r, TWH w, TWH h) const {
    call::render(display, std::forward<TR>(r), arg_t<TWH>(w, h));
  }
  template <renderer TR, typename TWH>
  constexpr auto operator()(TR &&r, TWH w, TWH h) const {
    auto arg = arg_t<TWH>(w, h, call::state(base(*this)));
    return [r = std::forward<TR>(r),
            arg = std::move(arg)]<display_component<TR, arg_t<TWH>> TD>(
               TD &&display) mutable { call::render(display, r, arg); };
  }
  static constexpr decltype(auto)
  handle(bp::cvref_type<widget_state_wrapper> auto &&self, auto &&evt)
    requires(has_handle<decltype(base(std::forward<decltype(self)>(self))),
                        decltype(evt)>)
  {
    return call::handle(base(std::forward<decltype(self)>(self)),
                        std::forward<decltype(evt)>(evt));
  }
};

struct widget_mono_state {
  static constexpr no_state_t state() noexcept { return {}; }
};

struct widget_no_event_handler {};

template <typename T> class widget_ref_no_set_area {
  T *t_;

public:
  constexpr explicit widget_ref_no_set_area(T &t) : t_(&t) {}

  constexpr bounding_box decltype(auto) area() const { return t_->area(); }

  constexpr decltype(auto) subcomponents() const
    requires(requires() { t_->subcomponents(); })
  {
    return t_->subcomponents();
  }
  constexpr decltype(auto) subcomponent() const
    requires(requires() { t_->subcomponent(); })
  {
    return t_->subcomponent();
  }
};

template <bounding_box TArea, typename TDisplay, typename TState,
          typename TEventHandler, typename TSubs, typename TOnResize>
class widget
    : bp::empty_structs_optimiser<TState, TEventHandler, TSubs, TOnResize> {
  using display_state_callbacks_t = widget_display_state_callbacks<TArea>;
  using widget_ref_t = widget_ref_no_set_area<widget>;
  TArea area_{};
  TDisplay display_;
  using base_t =
      bp::empty_structs_optimiser<TState, TEventHandler, TSubs, TOnResize>;

  static constexpr decltype(auto) state(auto &&self) noexcept {
    return self.get(std::type_identity<TState>{});
  }
  static constexpr decltype(auto) event_handler(auto &&self) noexcept {
    return self.get(std::type_identity<TEventHandler>{});
  }
  static constexpr decltype(auto) subs(auto &&self) noexcept {
    return self.get(std::type_identity<TSubs>{});
  }
  static constexpr decltype(auto) on_resize(auto &&self) noexcept {
    return self.get(std::type_identity<TOnResize>{});
  }

  constexpr auto set_state_callback(display_state_callbacks auto &display_cb) {
    // At this point, the state handler can change its state and propagate the
    // state change to all affected display aspects.
    return [this, &display_cb]<typename TS>(TS const &state) {
      auto display_setter = [&state, &display_cb]<typename TD>(TD &display) {
        if constexpr (has_set_state<TD, TS, decltype(display_cb)>) {
          call::set_state(display, state, display_cb);
        } else {
          unused(display, state, display_cb);
        }
      };
      call::for_each(display_, display_setter);
    };
  }

  constexpr void call_resize() {
    on_resize (*this)(widget_ref_no_set_area(*this), area_);
  }

public:
  constexpr widget()
    requires(std::is_default_constructible_v<TDisplay>)
  = default;

  template <typename... Ts>
    requires(std::constructible_from<TDisplay, Ts && ...>)
  constexpr explicit widget(Ts &&...d) : display_(std::forward<Ts>(d)...) {}
  template <typename TD, typename TS, typename TE, typename TSC, typename TRSZ>
    requires(std::constructible_from<TDisplay, TD &&> &&
             std::constructible_from<TEventHandler, TE &&> &&
             std::constructible_from<TState, TS &&> &&
             std::constructible_from<TSubs, TSC &&> &&
             std::constructible_from<TOnResize, TRSZ &&>)
  constexpr widget(TArea const &a, TD &&d, TS &&s, TE &&e, TSC &&sc, TRSZ &&rsz)
      : base_t(std::forward<TS>(s), std::forward<TE>(e), std::forward<TSC>(sc),
               std::forward<TRSZ>(rsz)),
        area_(a), display_(std::forward<TD>(d)) {
    call_resize();
  }

  [[nodiscard]] TArea const &area() const { return area_; }

  widget &area(TArea const &a) {
    area_ = a;
    call_resize();
    return *this;
  }
  constexpr TDisplay &displays() noexcept { return display_; }
  constexpr TDisplay const &displays() const noexcept { return display_; }

  constexpr void render(renderer auto &&r) const {
    auto w = call::width(area_);
    auto h = call::height(area_);
    auto render_callback = state(*this)(r.sub(area_), w, h);
    call::for_each(display_, std::move(render_callback));
    if constexpr (!std::is_empty_v<TSubs>) {
      call::for_each(subs(*this),
                     [&r](auto &&sub_w) { sub_w.render(r.sub(sub_w.area())); });
    }
  }
  template <typename TEvt, display_state_callbacks TCallback>
  constexpr void handle(TEvt &&evt, TCallback &&display_callbacks)
    requires(has_handle<TEventHandler, widget_ref_t const &, TEvt,
                        decltype(set_state_callback(display_callbacks))>)
  {
    // We expect that all default-constructed areas are empty.
    // First level: we call the event handler that takes input events and
    // translates it to a component state change.
    call::handle(event_handler(*this), widget_ref_no_set_area(*this),
                 std::forward<decltype(evt)>(evt),
                 [this, &display_callbacks]<typename TStateEvent>(
                     TStateEvent &&state_event) {
                   // Second level: event handler has taken the event input and
                   // now translates it to a behaviour event that the state
                   // aspect can read.
                   auto prev_state = call::state(state(*this));
                   call::handle(state(*this),
                                std::forward<TStateEvent>(state_event));
                   auto new_state = call::state(state(*this));
                   if (prev_state != new_state) {
                     set_state_callback(display_callbacks)(new_state);
                   }
                 });
  }
  constexpr bounding_box auto handle(auto &&evt)
    requires(has_handle<TEventHandler, widget_ref_t const &, decltype(evt),
                        decltype(set_state_callback(
                            std::declval<display_state_callbacks_t &>()))>)
  {
    display_state_callbacks_t display_callbacks(area());
    handle(evt, display_callbacks);
    return display_callbacks.result_area();
  }
  constexpr TSubs &subcomponents()
    requires(!std::is_empty_v<TSubs>)
  {
    return subs(*this);
  }
  constexpr decltype(auto) subcomponent()
    requires(!std::is_empty_v<TSubs> && bp::has_tuple_size<TSubs> &&
             requires() { std::get<0>(subcomponents()); })
  {
    return std::get<0>(subcomponents());
  }
};

template <typename TArea, typename TDisplay, widget_states_aspect TState,
          typename TEventHandler, typename TSubs, typename TOnResize>
class widget_builder_impl {
  TArea area_;
  TDisplay displays_;
  TState state_;
  TEventHandler event_;
  TSubs subs_;
  TOnResize on_resize_;

  using state_wrapper = widget_state_wrapper<TState>;
  using state_arg_t = typename state_wrapper::template arg_t<int>;

public:
  static constexpr bool contract_fulfilled = bounding_box<TArea>;

  constexpr widget_builder_impl() = default;
  template <typename TUs, typename TS, typename TE, typename TSC, typename TRSZ>
    requires(std::constructible_from<TDisplay, TUs &&> &&
             std::constructible_from<TState, TS &&> &&
             std::constructible_from<TEventHandler, TE &&> &&
             std::constructible_from<TSubs, TSC> &&
             std::constructible_from<TOnResize, TRSZ>)
  constexpr widget_builder_impl(TArea const &a, TUs &&displ, TS &&s, TE &&e,
                                TSC &&sc, TRSZ &&rsz)
      : area_(a), displays_(std::forward<TUs>(displ)),
        state_(std::forward<TS>(s)), event_(std::forward<TE>(e)),
        subs_(std::forward<TSC>(sc)), on_resize_(std::forward<TRSZ>(rsz)) {}

  template <widget_states_aspect TS2,
            typename TRes =
                widget_builder_impl<TArea, TDisplay, std::remove_cvref_t<TS2>,
                                    TEventHandler, TSubs, TOnResize>>
  TRes state(TS2 &&s) && {
    return TRes(std::move(area_), std::move(displays_), std::forward<TS2>(s),
                std::move(event_), std::move(subs_), std::move(on_resize_));
  }

  template <typename... TD2,
            typename TTuple = std::tuple<std::unwrap_ref_decay_t<TD2>...>,
            typename TRes = widget_builder_impl<
                TArea, TTuple, TState, TEventHandler, TSubs, TOnResize>>
    requires((builder_display_args<TD2, dummy_renderer, state_arg_t>) && ...)
  TRes display(TD2 &&...d2) && {
    return TRes(std::move(area_), TTuple(std::forward<decltype(d2)>(d2)...),
                std::move(state_), std::move(event_), std::move(subs_),
                std::move(on_resize_));
  }

  template <display_component_range<dummy_renderer,
                                    typename state_wrapper::template arg_t<int>>
                TTupleLike,
            typename TRes = widget_builder_impl<
                TArea, std::unwrap_reference_t<std::remove_cvref_t<TTupleLike>>,
                state_wrapper, TEventHandler, TSubs, TOnResize>>
  TRes display(TTupleLike &&displays) && {
    return TRes(std::move(area_), std::forward<TTupleLike>(displays),
                std::move(state_), std::move(event_), std::move(subs_),
                std::move(on_resize_));
  }

#if CGUI_HAS_NAMED_ARGS
  template <dooc::arg_with_any_name... TArgs>
    requires(
        builder_display_args<typename dooc::named_arg_properties<TArgs>::type,
                             dummy_renderer, state_arg_t> &&
        ...)
  constexpr auto display(TArgs &&...args) && {
    using tuple_t = dooc::named_tuple<std::remove_cvref_t<TArgs>...>;
    return widget_builder_impl<TArea, tuple_t, TState, TEventHandler, TSubs,
                               TOnResize>(
        std::move(area_), tuple_t(std::forward<TArgs>(args)...),
        std::move(state_), std::move(event_), std::move(subs_),
        std::move(on_resize_));
  }
#endif

  auto build() &&
      requires(contract_fulfilled) {
        static_assert(
            bounding_box<TArea>,
            "You must set an area to the widget before constructing it!");
        static_assert(contract_fulfilled);
        using display_t = decltype(impl::build_displays(
            std::move(displays_), state_wrapper::all_states()));
        using subs_t =
            decltype(impl::build_gui_context_widgets(std::move(subs_)));
        return widget<TArea, display_t, state_wrapper, TEventHandler, subs_t,
                      TOnResize>(
            std::move(area_),
            impl::build_displays(std::move(displays_),
                                 state_wrapper::all_states()),
            state_wrapper(std::move(state_)), std::move(event_),
            impl::build_gui_context_widgets(std::move(subs_)),
            std::move(on_resize_));
      } template <typename TE2,
                  typename TRes = widget_builder_impl<TArea, TDisplay, TState,
                                                      std::remove_cvref_t<TE2>,
                                                      TSubs, TOnResize>>
      TRes event(TE2 &&e) && {
    return TRes(std::move(area_), std::move(displays_), std::move(state_),
                std::forward<TE2>(e), std::move(subs_), std::move(on_resize_));
  }

  template <bounding_box TA,
            typename TRes = widget_builder_impl<
                TA, TDisplay, TState, TEventHandler, TSubs, TOnResize>>
  TRes area(TA const &a) && {
    return TRes(a, std::move(displays_), std::move(state_), std::move(event_),
                std::move(subs_), std::move(on_resize_));
  }

  template <widget_display_args... Ts,
            typename TTuple = std::tuple<std::unwrap_ref_decay_t<Ts>...>,
            typename TRes = widget_builder_impl<
                TArea, TDisplay, TState, TEventHandler, TTuple, TOnResize>>
  TRes subcomponents(Ts &&...args) && {
    return TRes{std::move(area_),
                std::move(displays_),
                std::move(state_),
                std::move(event_),
                TTuple(std::forward<Ts>(args)...),
                std::move(on_resize_)};
  }
  template <typename T, typename TRes = widget_builder_impl<
                            TArea, TDisplay, TState, TEventHandler, TSubs,
                            std::unwrap_ref_decay_t<T>>>
  TRes on_resize(T &&rsz) && {
    return {std::move(area_),  std::move(displays_), std::move(state_),
            std::move(event_), std::move(subs_),     std::forward<T>(rsz)};
  }
};

constexpr widget_builder_impl<default_rect, std::tuple<>, widget_mono_state,
                              widget_no_event_handler, std::tuple<>,
                              bp::no_op_t>
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

  constexpr text_renderer &set_displayed(bounding_box auto const &area,
                                         std::string_view t) {
    return set_displayed(call::width(area), call::height(area), t);
  }
  constexpr text_renderer &set_displayed(int w, int, std::string_view t) {
    using iterator_t = decltype(tokens_.begin());
    tokens_.clear();
    line_count_ = 0;
    if (size(t) == 0) {
      return *this;
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
    return *this;
  }
  constexpr void render(auto &&rorg, render_args auto &&args) const
    requires(has_render<glyph_t, decltype(rorg)>)
  {
    CGUI_DEBUG_ONLY(bool _area_initialised{};)
    auto fh = call::full_height(f_);
    auto do_new_area = [w = call::width(args),
                        base_y2 = call::height(args) + 2 * call::ascender(f_),
                        fh, count = line_count_](newline_entry nl) mutable {
      auto t_y = (base_y2 - fh * count) / 2;
      count -= 2;
      auto x = (w - nl.length) / 2;
      return box_from_xywh<default_rect>(x, t_y, nl.length, fh);
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
              call::t_y(a2, call::t_y(a2) - call::base_to_top(tok.g));
              assert(valid_box(a2));
              call::render(tok.g, r.sub(a2));
              trim_from_left(&area, call::advance_x(tok.g));
              trim_from_above(&area, call::advance_y(tok.g));
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
  constexpr text_renderer &text_colour(default_colour_t const &c) {
    colour_ = c;
    return *this;
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
    fill(r, default_rect{{0, 0}, {call::width(args), call::height(args)}},
         colour_);
  }
};

template <display_component T, typename TState, TState... tStates>
class display_per_state_impl {
  static constexpr auto state_count =
      std::max<std::size_t>(sizeof...(tStates), 1u);
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
    return std::get<state2index(widget_state_marker<TState, tStates...>(tI))>(
        std::forward<decltype(v)>(v).d_);
  }

  static constexpr void set_state(auto const &,
                                  display_state_callbacks auto &&cb) {
    if constexpr (state_count > 1) {
      cb.rerender();
    } else {
      unused(cb);
    }
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

  template <typename T>
  void handle(
      widget_ref_no_set_area<T> const &widget,
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
        auto is_now_inside = hit_box(widget.area(), call::position(evt));
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
        if (!mouse_down_ && hit_box(widget.area(), call::position(evt))) {
          trigger_callback(hold_event{});
        }
        mouse_down_ = true;
        return;
      }
    }
    if constexpr (can_be_event<mouse_button_up, evt_t>()) {
      if (is_event<mouse_button_up>(evt)) {
        if (hit_box(widget.area(), call::position(evt))) {
          trigger_callback(click_event{call::mouse_button(evt)});
        }
        mouse_down_ = false;
        return;
      }
    }
  }
};

enum class momentary_button_states { off, hover, hold };

template <typename TState, bp::invocable_or_invocable_args<TState> TClick,
          bp::invocable_or_invocable_args<TState> THover,
          bp::invocable_or_invocable_args<TState> THold,
          bp::invocable_or_invocable_args<TState> TExit>
class momentary_button_impl
    : bp::empty_structs_optimiser<TState, TClick, THover, THold, TExit> {

  using state_t =
      widget_state_marker<momentary_button_states, momentary_button_states::off,
                          momentary_button_states::hover,
                          momentary_button_states::hold>;

  using base_t =
      bp::empty_structs_optimiser<TState, TClick, THover, THold, TExit>;
  template <typename TBase> constexpr void _call() {
    decltype(auto) b = bp::as_forward(this->get(std::type_identity<TBase>{}));
    if constexpr (std::invocable<TBase &>) {
      std::invoke(*b);
    } else {
      std::invoke(*b, this->get(std::type_identity<TState>{}));
    }
  }
  constexpr void on_click() noexcept { _call<TClick>(); }
  constexpr decltype(auto) on_hover() noexcept { _call<THover>(); }
  constexpr decltype(auto) on_hold() noexcept { _call<THold>(); }
  constexpr decltype(auto) on_exit() noexcept { _call<TExit>(); }
  state_t current_state_{momentary_button_states::off};

public:
  template <typename... Ts>
    requires(std::constructible_from<base_t, Ts && ...>)
  constexpr explicit(sizeof...(Ts) == 1) momentary_button_impl(Ts &&...args)
      : base_t(std::forward<Ts>(args)...) {}

  [[nodiscard]] constexpr state_t state() const { return current_state_; }

  constexpr void handle(buttonlike_trigger::click_event const &) {
    on_click();
    current_state_ = momentary_button_states::hover;
    on_hover();
  }
  constexpr void handle(buttonlike_trigger::exit_event const &) {
    current_state_ = momentary_button_states::off;
    on_exit();
  }
  constexpr void handle(buttonlike_trigger::hold_event const &) {
    current_state_ = momentary_button_states::hold;
    on_hold();
  }
  constexpr void handle(buttonlike_trigger::hover_event const &) {
    current_state_ = momentary_button_states::hover;
    on_hover();
  }
};

template <typename TState = empty_state, typename TClick = bp::no_op_t,
          typename THover = bp::no_op_t, typename THold = bp::no_op_t,
          typename TExit = bp::no_op_t>
struct momentary_button {
  TState state;
  TClick on_click;
  THover on_hover;
  THold on_hold;
  TExit on_exit;

  constexpr momentary_button_impl<TState, TClick, THover, THold, TExit>
  build() &&
    requires bp::invocable_or_invocable_args<TClick, TState &> && //
             bp::invocable_or_invocable_args<THover, TState &> && //
             bp::invocable_or_invocable_args<THold, TState &> &&  //
             bp::invocable_or_invocable_args<TExit, TState &>     //
  {
    auto self = bp::as_forward(std::move(*this));
    return {(*self).state, (*self).on_click, (*self).on_hover, (*self).on_hold,
            (*self).on_exit};
  }
  template <typename T2>
  constexpr momentary_button<TState, std::unwrap_ref_decay_t<T2>, THover, THold,
                             TExit>
  click(T2 &&c) && {
    auto self = bp::as_forward(std::move(*this));
    return {(*self).state, std::forward<T2>(c), (*self).on_hover,
            (*self).on_hold, (*self).on_exit};
  }
  template <typename T2>
  constexpr momentary_button<TState, TClick, std::unwrap_ref_decay_t<T2>, THold,
                             TExit>
  hover(T2 &&c) && {
    auto self = bp::as_forward(std::move(*this));
    return {(*self).state, (*self).on_click, std::forward<T2>(c),
            (*self).on_hold, (*self).on_exit};
  }
  template <typename T2>
  constexpr momentary_button<TState, TClick, THover,
                             std::unwrap_ref_decay_t<T2>, TExit>
  hold(T2 &&c) && {
    auto self = bp::as_forward(std::move(*this));
    return {(*self).state, (*self).on_click, (*self).on_hover,
            std::forward<T2>(c), (*self).on_exit};
  }
  template <typename T2>
  constexpr momentary_button<TState, TClick, THover, THold,
                             std::unwrap_ref_decay_t<T2>>
  exit(T2 &&c) && {
    auto self = bp::as_forward(std::move(*this));
    return {(*self).state, (*self).on_click, (*self).on_hover, (*self).on_hold,
            std::forward<T2>(c)};
  }
  template <typename T2>
  constexpr momentary_button<std::unwrap_ref_decay_t<T2>, TClick, THover, THold,
                             TExit>
  callback_state(T2 &&c) && {
    auto self = bp::as_forward(std::move(*this));
    return {std::forward<T2>(c), (*self).on_click, (*self).on_hover,
            (*self).on_hold, (*self).on_exit};
  }
};

enum class toggle_button_states {
  off = 0,
  hover_off = 1,
  hold_off = 2,
  on = 4,
  hover_on = 5,
  hold_on = 6
};
template <std::invocable TToOn, std::invocable TToOff>
class toggle_button_impl : bp::empty_structs_optimiser<TToOn, TToOff> {

  using state_t = widget_state_marker<
      toggle_button_states, toggle_button_states::off,
      toggle_button_states::hover_off, toggle_button_states::hold_off,
      toggle_button_states::on, toggle_button_states::hover_on,
      toggle_button_states::hold_on>;

  using base_t = bp::empty_structs_optimiser<TToOn, TToOff>;
  template <typename T> constexpr decltype(auto) call() noexcept {
    return this->get(std::type_identity<T>{})();
  }
  bool on_ : 1 = {};
  bool hover_ : 1 = {};
  bool hold_ : 1 = {};

  using underlying_int = std::underlying_type_t<toggle_button_states>;

public:
  template <typename... Ts>
    requires(std::constructible_from<base_t, Ts && ...>)
  constexpr explicit(sizeof...(Ts) == 1) toggle_button_impl(Ts &&...args)
      : base_t(std::forward<Ts>(args)...) {}

  [[nodiscard]] constexpr state_t state() const {
    using enum toggle_button_states;
    auto on_part = on_ ? static_cast<underlying_int>(on) : underlying_int{};
    auto hover_hold_part = underlying_int{};
    if (hold_) {
      hover_hold_part = static_cast<underlying_int>(hold_off);
    } else if (hover_) {
      hover_hold_part = static_cast<underlying_int>(hover_off);
    }
    return static_cast<toggle_button_states>(on_part + hover_hold_part);
  }

  constexpr void handle(buttonlike_trigger::click_event const &) {
    if (on_) {
      call<TToOff>();
    } else {
      call<TToOn>();
    }
    hold_ = false;
    on_ = !on_;
  }
  constexpr void handle(buttonlike_trigger::exit_event const &) {
    hover_ = false;
    hold_ = false;
  }
  constexpr void handle(buttonlike_trigger::hold_event const &) {
    hold_ = true;
  }
  constexpr void handle(buttonlike_trigger::hover_event const &) {
    hover_ = true;
  }
};

template <std::invocable TToOn = bp::no_op_t,
          std::invocable TToOff = bp::no_op_t>
class toggle_button {
  TToOn to_on_{};
  TToOff to_off_{};

public:
  constexpr toggle_button() = default;
  template <typename TON, typename TOFF>
    requires(std::constructible_from<TToOn, TON &&> &&
             std::constructible_from<TToOff, TOFF &&>)
  constexpr toggle_button(TON &&on, TToOff &&off)
      : to_on_(std::forward<TON>(on)), to_off_(std::forward<TOFF>(off)) {}

  constexpr toggle_button_impl<TToOn, TToOff> build() && {
    return {std::move(to_on_), std::move(to_off_)};
  }
};

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_HPP

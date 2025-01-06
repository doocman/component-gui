#ifndef COMPONENT_GUI_CGUI_HPP
#define COMPONENT_GUI_CGUI_HPP

#include <algorithm>
#include <memory>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

#include <cgui/auto_ref.hpp>
#include <cgui/build_utility.hpp>
#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/array.hpp>
#include <cgui/std-backport/ranges.hpp>
#include <cgui/stl_extend.hpp>
#include <cgui/ui_events.hpp>
#include <cgui/widget_algorithm.hpp>

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
  using x_t = decltype(remove_unit_ref(call::l_x(relative_area_)));
  using y_t = decltype(remove_unit_ref(call::t_y(relative_area_)));
  x_t offset_x_{};
  y_t offset_y_{};

  template <bounding_box A2> friend class recursive_area_navigator;

  constexpr recursive_area_navigator(TB const &b, x_t ox, y_t oy)
      : relative_area_(b), offset_x_(ox), offset_y_(oy) {}

public:
  constexpr explicit recursive_area_navigator(TB const &b)
      : relative_area_(b) {}
  template <same_unit_geometry_as<TB> TB2 = TB>
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
  template <same_unit_geometry_as<TB> TB2 = TB, pixel_coord C>
    requires(same_unit_as<C, TB>)
  constexpr TB2 relative_area(TB2 b, C const &rel_point) const {
    return box_from_xywh<TB2>(call::l_x(b) + offset_x_ - call::x_of(rel_point),
                              call::t_y(b) + offset_y_ - call::y_of(rel_point),
                              call::width(b), call::height(b));
  }

  template <same_unit_geometry_as<TB> TB2 = TB>
  constexpr TB2 move_to_absolute(TB2 const &b) const {
    return box_from_xywh<TB2>(call::l_x(b) + offset_x_,
                              call::t_y(b) + offset_y_, call::width(b),
                              call::height(b));
  }
  constexpr TB absolute_area() const {
    return move_to_absolute(relative_area_);
  }

  constexpr default_coordinate offset() const
    requires(!size_tagged<TB>)
  {
    return {offset_x_, offset_y_};
  }
  template <typename TB2 = TB, typename SizeTag = tag_t_of<TB2>,
            typename ResultT = pixelpoint_unit<SizeTag, default_coordinate>>
    requires(size_tagged<TB>)
  constexpr ResultT offset() const {
    return ResultT(offset_x_, offset_y_);
  }

  constexpr nudger<x_t, y_t> relative_to_absolute_nudger() const noexcept {
    return {offset_x_, offset_y_};
  }
  template <same_unit_geometry_as<TB> A2>
  constexpr explicit operator recursive_area_navigator<A2>() const {
    return {copy_box<A2>(relative_area_), offset_x_, offset_y_};
  }

  template <pixel_coord V>
    requires(same_unit_as<V, TB>)
  constexpr recursive_area_navigator translate(V const &v) const {
    return {
        nudge_down(nudge_right(relative_area_, call::x_of(v)), call::y_of(v)),
        offset_x_ - call::x_of(v), offset_y_ - call::y_of(v)};
  }
};

template <canvas T, pixel_rect TB, pixelpoint_scale Scale> class sub_renderer {
  T *c_;
  recursive_area_navigator<TB> area_;
  default_colour_t set_colour_{};
  Scale scale_;

  static_assert(!std::is_reference_v<Scale>);

public:
  constexpr auto pixel_scale() const { return scale_; }

private:
  static constexpr TB bound_area(TB a) {
    if (!valid_box(a)) {
      call::height(a, 0);
      call::width(a, 0);
      assert(valid_box(a));
    }
    return a;
  }

  template <size_tagged TB2> constexpr auto to_pixels(TB2 const &b) const {
    return convert_pixelpoint<pixel_size_tag>(b, pixel_scale());
  }

  template <pixel_rect TB2>
  constexpr TB2 to_relative_dest(TB2 const &input_dest) const {
    return box_intersection<TB2>(input_dest, area_.relative_area());
  }
  template <typename TB2>
  constexpr TB2 to_absolute(TB2 const &relative_dest) const {
    return area_.move_to_absolute(relative_dest);
  }

public:
  constexpr sub_renderer(T &c, recursive_area_navigator<TB> a,
                         default_colour_t sc, Scale s)
      : c_(std::addressof(c)), area_(a), set_colour_(sc), scale_(s) {
    assert(valid_box(area_.relative_area()));
  }

  constexpr sub_renderer(T &c, TB a)
      : sub_renderer(c, recursive_area_navigator<TB>(a), {},
                     call::pixel_scale(c)) {}
  constexpr explicit sub_renderer(T &c)
      : sub_renderer(c, call::pixel_area(c)) {}
  template <point_rect TB2>
  constexpr sub_renderer(T &c, TB2 const &a)
      : sub_renderer(
            c, convert_pixelpoint<pixel_size_tag>(a, call::pixel_scale(c))) {}

  template <pixel_or_point_rect_basic TB2, pixel_draw_callback TCB>
  constexpr auto draw_pixels(TB2 const &dest, TCB &&cb) const {
    CGUI_ASSERT(valid_box(dest.value()));
    auto relative_dest = to_relative_dest(to_pixels(dest));
    CGUI_ASSERT(valid_box(relative_dest.value()));
    constexpr auto get_autoconv_dest = [](auto &&dest, auto &&px_scaler) {
      return autoconverting_pixelpoint_unit(dest, call::pixel_scale(px_scaler));
    };
    if (empty_box(relative_dest)) {
      using return_type = decltype((*c_).draw_pixels(
          get_autoconv_dest(relative_dest, *c_), [](auto &&...) {}));
      if constexpr (std::is_void_v<return_type>) {
        return;
      } else {
        return return_type{};
      }
    }
    auto absolute_dest = to_absolute(relative_dest);
    CGUI_ASSERT(valid_box(absolute_dest));
    return call::draw_pixels(
        *c_, get_autoconv_dest(absolute_dest, *c_),
        [cb = bp::as_forward(std::forward<decltype(cb)>(cb)), relative_dest,
         nudge = area_.relative_to_absolute_nudger()](auto &&drawer) {
          std::invoke(
              *cb, relative_dest,
              [d = bp::as_forward(std::forward<decltype(drawer)>(drawer)),
               &nudge](pixel_coordinate auto &&px, colour auto &&col) {
                auto absolute_pos = nudge(px);
                std::invoke(*d, absolute_pos, col);
              });
        });
  }

  template <pixel_or_point_rect_basic B, typename F>
  void draw_alpha(B const &b, F &&cb) {
    if (empty_box(b)) {
      return;
    }
    using bpix = convert_pixelpoint_t<pixel_size_tag, B>;
    draw_pixels(std::forward<decltype(b)>(b), [this, &cb](bpix const &bbox,
                                                          auto &&drawer) {
      CGUI_ASSERT(valid_box(bbox));
      cb(bbox, [this, &drawer](pixel_coordinate auto &&point, auto &&alpha) {
        drawer(point, multiply_alpha(set_colour_, alpha));
      });
    });
  }

  constexpr void fill(pixel_or_point_rect_basic auto const &dest,
                      colour auto const &c) {
    auto absolute_dest_maker = [this](auto const &d) {
      return to_absolute(to_relative_dest(
          convert_pixelpoint<pixel_size_tag>(d, pixel_scale())));
    };
    if constexpr (has_native_fill<decltype(*c_),
                                  decltype(absolute_dest_maker(dest)),
                                  decltype(c)>) {
      auto absolute_dest = absolute_dest_maker(dest);
      if (empty_box(absolute_dest)) {
        return;
      }
      call::fill(*c_, absolute_dest, c);
    } else {
      draw_pixels(dest,
                  fill_on_draw_pixel<std::remove_cvref_t<decltype(c)>>{c});
    }
  }

  constexpr sub_renderer sub(pixel_rect auto &&b, default_colour_t col) const {
    return {*c_, area_.sub(b), col, scale_};
  }

  constexpr sub_renderer sub(pixel_rect auto &&b) const {
    return sub(b, set_colour_);
  }

  constexpr sub_renderer sub(point_rect auto const &b, auto &&...args) const {
    return sub(to_pixels(b), std::forward<decltype(args)>(args)...);
  }

  constexpr sub_renderer translate(pixel_coordinate auto const &p) const {
    return {*c_, area_.translate(p), set_colour_, scale_};
  }
  constexpr sub_renderer translate(point_coordinate auto const &p) const {
    return translate(to_pixels(p));
  }
  template <pixelpoint_scale S2, typename CT = decltype(S2{} * scale_)>
  constexpr sub_renderer<T, TB, CT> scale(S2 s) const {
    return {*c_, area_, set_colour_, s * scale_};
  }

  constexpr sub_renderer with(default_colour_t c) {
    auto res = *this;
    res.set_colour_ = c;
    return res;
  }

  constexpr TB area() const { return area_.relative_area(); }
};

template <typename T>
using pixelpoint_scale_from_t =
    std::remove_cvref_t<decltype(call::pixel_scale(std::declval<T &&>()))>;

template <typename T, pixel_rect TB>
sub_renderer(T &, TB) -> sub_renderer<T, TB, pixelpoint_scale_from_t<T>>;
template <typename T>
sub_renderer(T &t)
    -> sub_renderer<T, std::remove_cvref_t<decltype(call::pixel_area(t))>,
                    pixelpoint_scale_from_t<T>>;
template <typename T, point_rect TB>
sub_renderer(T &, TB const &)
    -> sub_renderer<T, convert_pixelpoint_t<pixel_size_tag, TB>,
                    pixelpoint_scale_from_t<T>>;

template <point_rect TArea = point_unit_t<default_rect>>
class basic_widget_back_propagater {
  recursive_area_navigator<TArea> full_area_;
  TArea to_rerender_{};

  template <point_rect A2> friend class basic_widget_back_propagater;

  explicit(false) constexpr basic_widget_back_propagater(
      recursive_area_navigator<TArea> nav, TArea const &to_re = {})
      : full_area_(nav), to_rerender_(to_re) {}

public:
  explicit constexpr basic_widget_back_propagater(TArea full_area)
      : full_area_(full_area) {}

  constexpr void rerender() { to_rerender_ = full_area_.relative_area(); }
  constexpr void rerender(point_rect auto part_area) {
    if (!empty_box(to_rerender_)) {
      part_area = box_union(part_area, to_rerender_);
    }
    to_rerender_ = box_intersection(part_area, full_area_.relative_area());
  }
  TArea result_area() const {
    return full_area_.move_to_absolute(to_rerender_);
  }
  TArea relative_position(point_coordinate auto const &p) const {
    return full_area_.relative_area(to_rerender_, p);
  }
  constexpr bool empty_result() const { return empty_box(to_rerender_); }

  template <bounding_box TA2 = TArea>
  constexpr basic_widget_back_propagater sub(TA2 rel_area) const {
    return {full_area_.sub(rel_area)};
  }

  template <bounding_box TA2 = TArea>
  constexpr void merge_sub(basic_widget_back_propagater<TA2> const &s) {
    if (!s.empty_result()) {
      auto sub_area = s.relative_position(full_area_.offset());
      rerender(sub_area);
      // rerender(s.relative_area());
    }
  }

  template <typename A2>
  constexpr explicit operator basic_widget_back_propagater<A2>() const {
    return {static_cast<recursive_area_navigator<A2>>(full_area_),
            copy_box<A2>(to_rerender_)};
  }
};

namespace impl {
struct widget_display_constraint {
  constexpr void operator()(widget_display auto &&) const {}
};

} // namespace impl

template <renderer TR = dummy_renderer>
struct builder_widget_element_constraint {
  constexpr void operator()(widget_display<TR> auto &&) const {}
};

class widget_event_querant {

public:
};

template <point_rect TArea, typename TOnResize = bp::no_op_t,
          widget_display_range TWidgets = std::tuple<>>
class gui_context : bp::empty_structs_optimiser<TOnResize> {
  using _base_t = bp::empty_structs_optimiser<TOnResize>;
  TWidgets widgets_;
  using time_point_t = std::chrono::steady_clock::time_point;
  default_event_interpreter<time_point_t> interpreter_{};

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
        basic_size_wh{call::width(start_area), call::height(start_area)});
  }

  template <typename... Ts> constexpr void render(sub_renderer<Ts...> &&r) {
    // cgui::fill(r, r.area(), default_colour_t{0, 0, 0, 255});
    tuple_for_each([&r](auto &&v) { call::render(v, r.sub(call::area(v))); },
                   widgets_);
  }

  constexpr void render(canvas auto &&c,
                        pixel_or_point_rect_basic auto const &rarea) {
    render(sub_renderer(c, rarea));
  }

  constexpr void render(canvas auto &&c) { render(sub_renderer(c)); }

  // TODO: Reintroduce a constraint here, it is badly needed.
  constexpr native_box_t handle(auto const &evt)
  // requires((has_handle<TWidgets, decltype(evt),
  // basic_widget_back_propagater<native_box_t>> || ...) ||
  // can_be_event<ui_events::window_resized, decltype(evt)>())
  {
    if constexpr (can_be_event<input_events::window_resized, decltype(evt)>()) {
      if (is_event<input_events::window_resized>(evt)) {
        auto sz = call::size_of(evt);
        call_on_resize(sz);
        auto ret_box = box_from_xyxy<native_box_t>(0, 0, call::width(sz),
                                                   call::height(sz));
        return ret_box;
      }
    }
    auto b = basic_widget_back_propagater(
        box_from_xyxy<native_box_t>(0, 0, highest_possible, highest_possible));
    call::for_each(widgets_, [&evt, &b]<typename TW>(TW &w) {
      if constexpr (has_handle<TW &, decltype(evt),
                               basic_widget_back_propagater<native_box_t>>) {
        auto s = b.sub(w.area());
        call::handle(w, evt, s);
        b.merge_sub(s);
      }
    });
    interpreter_.handle(
        evt,
        [this, &b]<typename Pred, typename OnFind, typename OnNoFind,
                   interpreted_events... events>(
            query_interpreted_events_t<Pred, OnFind, OnNoFind, events...> const
                &q) {
          auto eacher = [&]<typename W>(W &w) {
            if constexpr ((has_handle<
                               W &,
                               interpreted_event<events, time_point_t> const &,
                               basic_widget_back_propagater<native_box_t>> ||
                           ...)) {
              auto sb = b.sub(w.area());
              if (w.query(std::type_identity<time_point_t>{}, q, sb, &b)) {
                return true;
              }
            }
            return false;
          };
          call::apply_to(widgets_, [&eacher](auto &...ws) {
            // We are not expecting overlapping widgets here.
            unused((eacher(ws) || ...));
          });
          //
        });
    return b.result_area();
  }

  constexpr TWidgets &widgets()
    requires(!std::is_empty_v<TWidgets>)
  {
    return widgets_;
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
  template <point_rect TArea = point_unit_t<default_rect>,
            typename TW = decltype(build::build_group(
                impl::widget_display_constraint{}, std::move(widgets_)))>
  constexpr gui_context<TArea, TOnResize, TW>
  build(TArea const &start_area) && {
    return {std::move(on_resize_),
            build::build_group(impl::widget_display_constraint{},
                               std::move(widgets_)),
            start_area};
  }
  template <bounding_box TArea = default_rect,
            typename TW = decltype(build::build_group(
                impl::widget_display_constraint{}, std::move(widgets_)))>
    requires(!size_tagged<TArea>)
  constexpr gui_context<point_unit_t<TArea>, TOnResize, TW>
  build(TArea const &start_area) && {
    return {std::move(on_resize_),
            build::build_group(impl::widget_display_constraint{},
                               std::move(widgets_)),
            point_unit_t<TArea>(start_area)};
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

  template <point_scalar TWH>
  using arg_t = widget_render_args<TWH, state_marker_t>;

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

template <pixel_or_point_rect_basic TArea, typename TDisplay, typename TState,
          typename TEventHandler, typename TSubs, typename TOnResize>
class widget
    : bp::empty_structs_optimiser<TState, TEventHandler, TSubs, TOnResize> {
  using display_state_callbacks_t = basic_widget_back_propagater<TArea>;
  using widget_ref_t = widget_ref_no_set_area<widget>;
  using on_destruct_f_t =
      ignore_copy<bp::trivial_function<void(widget &&), sizeof(void *) * 3,
                                       alignof(void *)>, //
                  // std::function<void(widget&&)>,//
                  bp::return_constant_t<bp::no_op_t>>;
  TArea area_{};
  TDisplay display_;
  on_destruct_f_t on_destruct_;
  using base_t =
      bp::empty_structs_optimiser<TState, TEventHandler, TSubs, TOnResize>;

  static constexpr decltype(auto) event_handler(auto &&self) noexcept {
    return self.get(std::type_identity<TEventHandler>{});
  }
  static constexpr decltype(auto) subs(auto &&self) noexcept {
    return self.get(std::type_identity<TSubs>{});
  }
  static constexpr decltype(auto) on_resize(auto &&self) noexcept {
    return self.get(std::type_identity<TOnResize>{});
  }

  constexpr auto set_state(widget_back_propagater auto &display_cb,
                           auto const &state) {
    auto display_setter = [&state, &display_cb]<typename TD>(TD &display) {
      if constexpr (has_set_state<TD, decltype(state), decltype(display_cb)>) {
        call::set_state(display, state, display_cb);
      } else {
        unused(display, state, display_cb);
      }
    };
    call::for_each(display_, display_setter);
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

  constexpr widget(widget const &) = default;
  constexpr widget(widget &&) noexcept = default;
  constexpr widget &operator=(widget const &) = default;
  constexpr widget &operator=(widget &&) noexcept = default;
  constexpr ~widget() {
    CGUI_ASSERT(on_destruct_.value());
    on_destruct_.value()(std::move(*this));
  }

  [[nodiscard]] TArea const &area() const { return area_; }

  widget &area(TArea const &a) {
    area_ = a;
    call_resize();
    return *this;
  }
  constexpr TDisplay &displays() noexcept { return display_; }
  constexpr TDisplay const &displays() const noexcept { return display_; }

  constexpr auto state() const {
    if constexpr (stateful_aspect<TEventHandler>) {
      return call::state(event_handler(*this));
    } else {
      return no_state_t{};
    }
  }

  constexpr void render(renderer auto &&r) const {
    auto w = call::width(area_);
    auto h = call::height(area_);
    auto arg = widget_render_args(w, h, state());
    auto render_callback = [&r, &arg]<typename TD>(TD &&display) {
      call::render(display, r, arg);
    };
    call::for_each(display_, std::move(render_callback));
    if constexpr (has_render<TEventHandler const &, decltype(r) &,
                             decltype(arg) &>) {
      call::render(event_handler(*this), r, arg);
    }
    if constexpr (!std::is_empty_v<TSubs>) {
      call::for_each(subs(*this),
                     [&r](auto &&sub_w) { sub_w.render(r.sub(sub_w.area())); });
    }
  }
  template <typename TEvt, widget_back_propagater TCallback>
  constexpr void handle(TEvt &&evt, TCallback &&display_callbacks)
    requires has_handle<TEventHandler, TArea const &, decltype(evt)> ||
             has_handle<TEventHandler, TArea const &, decltype(evt), TCallback>
  {
    auto const do_handle = [this, &evt, &display_callbacks] {
      if constexpr (has_handle<TEventHandler, TArea const &, decltype(evt),
                               TCallback>) {
        call::handle(event_handler(*this), area(),
                     std::forward<decltype(evt)>(evt), display_callbacks);
      } else {
        call::handle(event_handler(*this), area(),
                     std::forward<decltype(evt)>(evt));
        unused(display_callbacks);
      }
    };
    if constexpr (stateful_aspect<TState>) {
      auto prev_state = state();
      do_handle();
      if (auto new_state = state(); prev_state != new_state) {
        set_state(display_callbacks, new_state);
      }
    } else {
      do_handle();
    }
  }
  constexpr bounding_box auto handle(auto &&evt)
    requires has_handle<widget &, decltype(evt), display_state_callbacks_t &>
  {
    display_state_callbacks_t display_callbacks(area());
    handle(std::forward<decltype(evt)>(evt), display_callbacks);
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
  constexpr TEventHandler &event_component()
    requires(!std::is_empty_v<TEventHandler>)
  {
    return event_handler(*this);
  }

  constexpr void set_on_destruct(auto &&f) {
    on_destruct_.value() = std::forward<decltype(f)>(f);
  }
  constexpr void reset_on_destruct() { on_destruct_.value() = bp::no_op; }

  template <typename TimePoint, typename Pred, typename OnFind,
            typename OnNoFind, interpreted_events... events,
            widget_back_propagater BP, widget_back_propagater BPMain>
  constexpr bool
  query(std::type_identity<TimePoint> tpi,
        query_interpreted_events_t<Pred, OnFind, OnNoFind, events...> const &q,
        BP &bp, BPMain *bp_main) {
    bool found{};
    if constexpr (!std::is_empty_v<TSubs>) {
      call::for_each(subcomponents(), [&found, &q, &bp, tpi,
                                       bp_main]<typename SW>(SW &&sw) {
        if constexpr ((has_handle<SW, interpreted_event<events, TimePoint>,
                                  BP> ||
                       ...)) {
          found = found || sw.query(tpi, q, bp.sub(sw.area()), bp_main);
        }
      });
    }
    if constexpr ((has_handle<widget &, interpreted_event<events, TimePoint>,
                              BP> ||
                   ...)) {
      if (!found) {
        found = q(*this, [&bp, bp_main]<typename E>(widget &self, E const &e)
                    requires has_handle<widget &, E const &, BP &>
                  {
                    self.handle(e, bp);
                    bp_main->merge_sub(bp);
                  });
      }
    }
    return found;
  }

  constexpr point_rect auto intrinsic_min_size() const
    requires(requires(TEventHandler const &eh) {
      call::intrinsic_min_size(eh);
    })
  {
    return call::intrinsic_min_size(event_handler(*this));
  }
};

template <renderer TR, typename TStateArgs>
struct builder_display_element_constraint {
  constexpr void
  operator()(builder_display_args<TR, TStateArgs> auto &&) const {}
};

template <renderer TR> struct display_element_constraint {
  constexpr void operator()(display_component<TR> auto &&) const {}
};

template <pixel_or_point_rect_basic TArea, typename TDisplay,
          widget_states_aspect TState, typename TEventHandler, typename TSubs,
          typename TOnResize>
class widget_builder_impl {
  using state_wrapper = widget_state_wrapper<TState>;
  using state_arg_t = typename state_wrapper::template arg_t<point_unit_t<int>>;
  using display_constraint_t =
      builder_display_element_constraint<dummy_renderer, state_arg_t>;

  TArea area_;
  TDisplay displays_;
  TState state_;
  TEventHandler event_;
  TSubs subs_;
  TOnResize on_resize_;

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

  template <typename... TD2> constexpr auto display(TD2 &&...vs) && {
    using TRes = widget_builder_impl<
        TArea, build::args_to_group_t<display_constraint_t, TD2 &&...>, TState,
        TEventHandler, TSubs, TOnResize>;
    auto s = bp::as_forward(std::move(*this));
    return TRes{
        (*s).area_,
        build::args_to_group(display_constraint_t{}, std::forward<TD2>(vs)...),
        (*s).state_,
        (*s).event_,
        (*s).subs_,
        (*s).on_resize_};
  }

  auto build() &&
    requires contract_fulfilled
  {
    static_assert(bounding_box<TArea>,
                  "You must set an area to the widget before constructing it!");
    static_assert(contract_fulfilled);
    using display_t = decltype(build::build_group(
        display_element_constraint<dummy_renderer>{},
        std::move(*this).displays_, all_states<TEventHandler>()));

    using subs_t = decltype(build::build_group(
        impl::widget_display_constraint{}, std::move(*this).subs_));
    auto s = bp::as_forward(std::move(*this));
    return widget<TArea, display_t, TState, TEventHandler, subs_t, TOnResize>(
        (*s).area_,
        build::build_group(display_element_constraint<dummy_renderer>{},
                           (*s).displays_, all_states<TEventHandler>()),
        (*s).state_, (*s).event_,
        build::build_group(impl::widget_display_constraint{},
                           std::move(*this).subs_),
        (*s).on_resize_);
  }

  template <typename TE2, typename TRes = widget_builder_impl<
                              TArea, TDisplay, TState, std::remove_cvref_t<TE2>,
                              TSubs, TOnResize>>
  TRes event(TE2 &&e) && {
    return TRes(std::move(area_), std::move(displays_), std::move(state_),
                std::forward<TE2>(e), std::move(subs_), std::move(on_resize_));
  }

  template <pixel_or_point_rect_basic TA,
            typename TRes = widget_builder_impl<
                TA, TDisplay, TState, TEventHandler, TSubs, TOnResize>>
  TRes area(TA const &a) && {
    return TRes(a, std::move(displays_), std::move(state_), std::move(event_),
                std::move(subs_), std::move(on_resize_));
  }

  template <typename TA>
    requires(std::constructible_from<TArea, TA const &> && !size_tagged<TA>)
  widget_builder_impl area(TA const &a) && {
    area_ = TArea(a);
    return std::move(*this);
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

constexpr widget_builder_impl<point_unit_t<default_rect>, std::tuple<>,
                              widget_mono_state, widget_no_event_handler,
                              std::tuple<>, bp::no_op_t>
widget_builder() {
  return {};
}

template <font_face TFont> class text_renderer {
  using glyph_t = std::remove_cvref_t<
      decltype(call::glyph(std::declval<TFont &>(), char{}).value())>;

  struct glyph_entry {
    glyph_t g;
  };
  struct newline_entry {
    pixel_unit_t<int> length{};
  };

  using token_t = std::variant<newline_entry, glyph_entry>;
  TFont f_;
  std::string text_;
  default_colour_t colour_{255, 255, 255, 255};
  // cached for faster rendering.
  std::vector<token_t> mutable tokens_;
  pixel_unit_t<int> mutable last_sz_{};
  int mutable line_count_{};

  constexpr void assert_displayed() const {
    pixel_unit_t<long long> cur_len = {{}, -1};
    pixel_unit_t<long long> measured_len{};
    long long measured_lines{};
    for (auto &tv : tokens_) {
      std::visit(
          [&]<typename T>(T &t) {
            if constexpr (std::is_same_v<T, glyph_entry>) {
              measured_len += call::advance_x(t.g);
              CGUI_ASSERT(cur_len >= measured_len);
            } else {
              if (cur_len != pixel_unit(-1)) {
                CGUI_ASSERT(cur_len == measured_len);
              }
              cur_len = t.length;
              measured_len = {};
              ++measured_lines;
            }
          },
          tv);
    }
    CGUI_ASSERT(measured_lines == line_count_);
  }

  constexpr void reset_glyphs() const {
    using iterator_t = decltype(tokens_.begin());
    auto const &t = text_;
    auto const &w = last_sz_;
    tokens_.clear();
    line_count_ = 0;
    if (size(text_) == 0) {
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
      ++line_count_;
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
    auto last_ws_length = pixel_unit_t<int>();
    auto last_ws_size = pixel_unit_t<int>();
    for (auto const &c : t) {
      if (c == 'd') {
        unused(c);
      }
      if (c == '\n') {
        add_line();
      } else if (auto gexp = call::glyph(f_, c)) {
        is_pixel_sized auto gbox = call::pixel_area(*gexp);
        is_pixel_sized auto gl_adv = call::advance_x(*gexp);
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
              auto dw = call::advance_x(*dgexp);
              if (current_line().length + dw <= w) {
                current_line().length += dw;
                tokens_.emplace_back(glyph_entry{std::move(*dgexp)});
                add_line();
              } else {
                using namespace std::views;
                decltype(tokens_.begin()) dash_pos_native;
                auto acc_width = pixel_unit(0);
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
          last_ws_size = {};
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
      unused(c);
    }
    assert_displayed();
  }

public:
  template <typename... TU>
    requires(std::constructible_from<TFont, TU && ...>)
  constexpr explicit text_renderer(TU &&...f) : f_(std::forward<TU>(f)...) {}
  template <typename... TU>
    requires(std::constructible_from<TFont, TU && ...>)
  constexpr explicit text_renderer(std::in_place_type_t<TFont>, TU &&...f)
      : f_(std::forward<TU>(f)...) {}

  constexpr text_renderer &set_text(std::string_view t) {
    text_.assign(t);
    return *this;
  }

  constexpr void render(auto &&rorg, render_args auto &&args) const
    requires(has_render<glyph_t, decltype(rorg)>)
  {
    CGUI_DEBUG_ONLY(bool _area_initialised{};)
    if (empty(text_)) {
      return;
    }
    auto new_w = convert_pixelpoint<pixel_size_tag>(call::width(args),
                                                    call::pixel_scale(rorg));
    if (empty(tokens_) || new_w != last_sz_) {
      last_sz_ = new_w;
      reset_glyphs();
    }
    auto fh = call::full_height(f_);
    auto do_new_area = [w = convert_pixelpoint<pixel_size_tag>(
                            call::width(args), rorg.pixel_scale()),
                        base_y2 = convert_pixelpoint<pixel_size_tag>(
                                      call::height(args), rorg.pixel_scale()) +
                                  2 * fh - call::ascender(f_),
                        fh, count = line_count_](newline_entry nl) mutable {
      auto t_y = (base_y2 - fh * count) / 2;
      count -= 2;
      auto x = (w - nl.length) / 2;
      return box_from_xywh<default_pixel_rect>(x, t_y, nl.length, fh);
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
    // call::fill(r,
    r.fill(box_from_xyxy<default_point_rect>(0, 0, call::width(args),
                                             call::height(args)
                                             // args.width(),
                                             // args.height()
                                             ),
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
                                  widget_back_propagater auto &&cb) {
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

namespace button_state_events {
struct hover {};
struct exit {};
struct hold {};
struct click {};
template <typename T>
concept state_event_handler =
    bp::can_be_operand_for_all<T, decltype(call::handle), hover, exit, hold,
                               click>;
} // namespace button_state_events

template <typename T>
concept button_state =
    button_state_events::state_event_handler<T> && widget_states_aspect<T>;

/// @brief Event handling class for button-like widgets that manage state
/// transitions.
///
/// The `buttonlike_trigger` class template handles mouse-related events, such
/// as clicks, holds, and hovering, for button widgets and translates them into
/// button state transition events. It provides a convenient way to implement
/// different types of buttons (e.g., momentary or toggle) by allowing a
/// `TState` type to be injected, which manages the button's state transitions.
///
/// @tparam TState Type parameter representing the state transition logic of the
/// button. This must satisfy the `button_state` concept requirements.
template <button_state TState>
class buttonlike_trigger : bp::empty_structs_optimiser<TState> {
  // Perfect forwarding access to the state object for this button.
  // May return a reference or a value depending on the empty_structs_optimiser
  // implementation and wheter TState is empty or not.
  static constexpr decltype(auto) state_impl(auto &&self) {
    return std::forward<decltype(self)>(self).get(bp::index_constant<0>{});
  }

  // Pass a state change event to the TState object.
  constexpr void state_change(auto &&event) {
    call::handle(state_impl(*this), event);
  }

  // Convenient type aliases
  using hover_event = button_state_events::hover;
  using exit_event = button_state_events::exit;
  using hold_event = button_state_events::hold;
  using click_event = button_state_events::click;

  constexpr auto interp_evt_switch() {
    return saved_ui_event_switch(
        std::ref(*this),
        event_case<interpreted_events::primary_click>(
            [](auto const &, auto &self) { self.state_change(click_event{}); }),
        event_case<interpreted_events::pointer_hold>(
            [](auto const &, auto &self) { self.state_change(hold_event{}); }),
        event_case<interpreted_events::pointer_hover>(
            [](auto const &, auto &self) { self.state_change(hover_event{}); }),
        event_case<interpreted_events::pointer_exit>(
            [](auto const &, auto &self) {
              self.state_change(exit_event{});
            }) //
    );
  }

public:
  using bp::empty_structs_optimiser<TState>::empty_structs_optimiser;

  constexpr auto state() const
    requires requires(TState const &s) { call::state(s); }
  {
    return call::state(state_impl(*this));
  }

  template <
      typename Area,
      interpreted_event_types<
          interpreted_events::pointer_hover, interpreted_events::pointer_exit,
          interpreted_events::pointer_hold, interpreted_events::primary_click>
          Evt>
  constexpr void handle(Area const &, Evt const &e) {
    interp_evt_switch()(e);
  }
};
template <typename T>
buttonlike_trigger(T &&) -> buttonlike_trigger<std::unwrap_ref_decay_t<T>>;

enum class momentary_button_states { off, hover, hold };

template <typename TState, bp::invocable_or_invocable_args<TState> TClick,
          bp::invocable_or_invocable_args<TState> THover,
          bp::invocable_or_invocable_args<TState> THold,
          bp::invocable_or_invocable_args<TState> TExit>
class momentary_button_impl
    : bp::empty_structs_optimiser<TState, TClick, THover, THold, TExit> {
  static constexpr std::size_t state_i = 0;
  static constexpr std::size_t click_i = 1;
  static constexpr std::size_t hover_i = 2;
  static constexpr std::size_t hold_i = 3;
  static constexpr std::size_t exit_i = 4;

  using state_t =
      widget_state_marker<momentary_button_states, momentary_button_states::off,
                          momentary_button_states::hover,
                          momentary_button_states::hold>;

  using base_t =
      bp::empty_structs_optimiser<TState, TClick, THover, THold, TExit>;

  template <std::size_t tI> constexpr void _call() {
    decltype(auto) b = bp::as_forward(this->get(bp::index_constant<tI>{}));
    if constexpr (std::invocable<decltype(*b)>) {
      std::invoke(*b);
    } else {
      std::invoke(*b, this->get(bp::index_constant<state_i>{}));
    }
  }
  constexpr void on_click() noexcept { _call<click_i>(); }
  constexpr decltype(auto) on_hover() noexcept { _call<hover_i>(); }
  constexpr decltype(auto) on_hold() noexcept { _call<hold_i>(); }
  constexpr decltype(auto) on_exit() noexcept { _call<exit_i>(); }
  state_t current_state_{momentary_button_states::off};

public:
  template <typename... Ts>
    requires(std::constructible_from<base_t, Ts && ...>)
  constexpr explicit(sizeof...(Ts) == 1) momentary_button_impl(Ts &&...args)
      : base_t(std::forward<Ts>(args)...) {}

  [[nodiscard]] constexpr state_t state() const { return current_state_; }

  constexpr void handle(button_state_events::click const &) {
    on_click();
    current_state_ = momentary_button_states::hover;
    on_hover();
  }
  constexpr void handle(button_state_events::exit const &) {
    current_state_ = momentary_button_states::off;
    on_exit();
  }
  constexpr void handle(button_state_events::hold const &) {
    current_state_ = momentary_button_states::hold;
    on_hold();
  }
  constexpr void handle(button_state_events::hover const &) {
    current_state_ = momentary_button_states::hover;
    on_hover();
  }
};

template <typename TState = empty_placeholder_t, typename TClick = bp::no_op_t,
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
  relaxed_off = 0,
  hover_off = 1,
  hold_off = 2,
  relaxed_on = 3,
  hover_on = 4,
  hold_on = 5
};
constexpr toggle_button_states
combine_toggled_hover_states(bool is_on, bool hovered, bool hold) noexcept {
  using enum toggle_button_states;
  using underlying_int = std::underlying_type_t<toggle_button_states>;
  auto on_part =
      is_on ? static_cast<underlying_int>(relaxed_on) : underlying_int{};
  auto hover_hold_part = underlying_int{};
  if (hovered) {
    if (hold) {
      hover_hold_part = static_cast<underlying_int>(hold_off);
    } else {
      hover_hold_part = static_cast<underlying_int>(hover_off);
    }
  }
  return static_cast<toggle_button_states>(on_part + hover_hold_part);
}

template <typename TState, bp::invocable_or_invocable_args<TState &> TToOn,
          bp::invocable_or_invocable_args<TState &> TToOff>
class toggle_button_impl : bp::empty_structs_optimiser<TState, TToOn, TToOff> {

  using state_t = widget_state_marker<
      toggle_button_states, toggle_button_states::relaxed_off,
      toggle_button_states::hover_off, toggle_button_states::hold_off,
      toggle_button_states::relaxed_on, toggle_button_states::hover_on,
      toggle_button_states::hold_on>;

  static constexpr std::size_t state_i = 0;
  static constexpr std::size_t activate_i = 1;
  static constexpr std::size_t deactivate_i = 2;

  using base_t = bp::empty_structs_optimiser<TState, TToOn, TToOff>;
  template <std::size_t tI> constexpr void call() noexcept {
    decltype(auto) f = this->get(bp::index_constant<tI>{});
    if constexpr (std::invocable<decltype(f)>) {
      f();
    } else {
      f(this->get(bp::index_constant<state_i>{}));
    }
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
    return combine_toggled_hover_states(on_, hover_, hold_);
  }

  constexpr void handle(button_state_events::click const &) {
    if (on_) {
      call<activate_i>();
    } else {
      call<deactivate_i>();
    }
    hold_ = false;
    on_ = !on_;
  }
  constexpr void handle(button_state_events::exit const &) { hover_ = false; }
  constexpr void handle(button_state_events::hold const &) {
    hover_ = true;
    hold_ = true;
  }
  constexpr void handle(button_state_events::hover const &) {
    hover_ = true;
    hold_ = false;
  }
};

/// @brief A builder for a widget state to generate a toggle button. Works with
/// the @ref buttonlike_trigger event aspect.
///
/// The `toggle_button` class template allows for constructing a toggle button
/// with two different actions: one for turning "on" and one for turning "off".
/// The actions are specified as invocable types, allowing customization of
/// the toggle button's behavior upon each toggle.
///
/// @tparam TState Common state that the callbacks can use and manipulate. It is
///                used as an optional argument to the activation and
///                deactivation callbacks
/// @tparam TToOn Type of the action to perform when the button is toggled to
///               "on" state.
///               Defaults to a no-operation.
/// @tparam TToOff Type of the action to perform when the button is toggled to
///                "off" state.
///                Defaults to a no-operation.
template <typename TState = empty_placeholder_t,
          std::invocable TToOn = bp::no_op_t,
          std::invocable TToOff = bp::no_op_t>
class toggle_button_state {
  TState cb_state_{};
  TToOn to_on_{};
  TToOff to_off_{};

public:
  /// @brief Default constructor.
  constexpr toggle_button_state() = default;

  /// @brief Parameterized constructor.
  ///
  /// Constructs a `toggle_button` with specified actions for "on" and "off"
  /// states.
  ///
  /// @tparam TS Type of the callback state.
  /// @tparam TON Type of the action for the "on" state.
  /// @tparam TOFF Type of the action for the "off" state.
  /// @param s Callback state.
  /// @param on Action to perform when the button is toggled to "on" state.
  /// @param off Action to perform when the button is toggled to "off" state.
  template <typename TS, typename TON, typename TOFF>
    requires(std::constructible_from<TState, TS &&> &&
             std::constructible_from<TToOn, TON &&> &&
             std::constructible_from<TToOff, TOFF &&>)
  constexpr toggle_button_state(TS &&s, TON &&on, TOFF &&off)
      : cb_state_(std::forward<TS>(s)), to_on_(std::forward<TON>(on)),
        to_off_(std::forward<TOFF>(off)) {}

  /// @brief Builds the toggle button state aspect.
  ///
  /// Converts this builder into an implementation of a toggle button with the
  /// specified actions for "on" and "off" states.
  ///
  /// @return toggle_button_impl<TToOn, TToOff> An implementation of the toggle
  /// button.
  ///
  /// @note The builder is invalidated after this operation, as it transfers
  ///       ownership of the actions to the returned implementation.
  constexpr toggle_button_impl<TState, TToOn, TToOff> build() && {
    auto s = bp::as_forward(std::move(*this));
    return {(*s).cb_state_, (*s).to_on_, (*s).to_off_};
  }

  /// @brief Add a function to be called when the button activates.
  ///
  /// The function should either take no arguments or have a signature in which
  /// a TState& can be passed in as the first argument.
  ///
  /// @tparam TOn Type of input parameter.
  /// @tparam T Helper type
  /// @param in New function to be called when the final object is activated.
  /// Use std::ref to create a reference object.
  /// @return Modified toggle_button builder.
  ///
  /// @note Invalidates the original builder.
  template <typename TOn, typename T = std::unwrap_ref_decay_t<TOn>>
  constexpr toggle_button_state<TState, T, TToOff> on_active(TOn &&in) && {
    auto s = bp::as_forward(std::move(*this));
    return {(*s).cb_state_, std::forward<TOn>(in), (*s).to_off_};
  }

  /// @brief Add a function to be called when the button deactivates (reverts to
  /// its initial state).
  ///
  /// The function should either take no arguments or have a signature in which
  /// a TState& can be passed in as the first argument.
  ///
  /// @tparam TOff Type of input parameter.
  /// @tparam T Helper type
  /// @param in New function to be called when the final object is deactivated.
  /// Use std::ref to create a reference object.
  /// @return Modified toggle_button builder.
  ///
  /// @note Invalidates the original builder.
  template <typename TOff, typename T = std::unwrap_ref_decay_t<TOff>>
  constexpr toggle_button_state<TState, TToOn, T> on_unactive(TOff &&in) && {
    auto s = bp::as_forward(std::move(*this));
    return {(*s).cb_state_, (*s).to_on_, std::forward<TOff>(in)};
  }

  /// @brief Add a shared state object accessible to all state change callbacks
  /// during execution.
  ///
  /// This function allows you to specify a state object that all toggle button
  /// callbacks (both on and off) can access. This is useful when callbacks
  /// require shared data or configuration throughout the toggle button's
  /// lifetime. It will appear as an optional argument to the state change
  /// callbacks as a mutable reference (unless std::ref/std::cref is used to
  /// generate a constant reference).
  ///
  /// @tparam TIn Type of input parameter.
  /// @tparam T Helper type
  /// @param in New state object that can be used by the callbacks. Use std::ref
  ///           to create a reference object.
  /// @return Modified toggle_button builder.
  ///
  /// @note Invalidates the original builder.
  template <typename TIn, typename T = std::unwrap_ref_decay_t<TIn>>
  constexpr toggle_button_state<TState, TToOn, T> callback_state(TIn &&in) && {
    auto s = bp::as_forward(std::move(*this));
    return {std::forward<TIn>(in), (*s).to_on_, (*s).to_off_};
  }
};

template <typename TState, typename TWH>
  requires(requires(TState const &s, std::size_t i) {
    { std::invoke(s, i) } -> state_marker;
  })
class basic_button_list_args : bp::empty_structs_optimiser<TState> {
  using _base_t = bp::empty_structs_optimiser<TState>;
  TWH w_;
  TWH h_;

public:
  template <bounding_box B, typename T>
    requires(std::constructible_from<TState, T>)
  constexpr basic_button_list_args(B const &b, T &&s)
      : _base_t(std::forward<T>(s)), w_(call::width(b)), h_(call::height(b)) {}
  template <typename T>
    requires(std::constructible_from<TState, T>)
  constexpr basic_button_list_args(TWH w, TWH h, T &&s)
      : _base_t(std::forward<T>(s)), w_(w), h_(h) {}

  constexpr TWH width() const { return w_; }
  constexpr TWH height() const { return h_; }
  constexpr state_marker decltype(auto) button_state(std::size_t i) const {
    return std::invoke(this->get_first(), i);
  }
};

template <bounding_box B, typename T>
basic_button_list_args(B const &, T &&) -> basic_button_list_args<
    std::unwrap_ref_decay_t<T>,
    std::remove_cvref_t<decltype(call::width(std::declval<B const &>()))>>;
template <typename TWH, typename T>
basic_button_list_args(TWH const &, TWH const &, T &&)
    -> basic_button_list_args<std::unwrap_ref_decay_t<T>, TWH>;

template <typename StateVal = widget_state_marker<int>>
using dummy_button_list_args =
    basic_button_list_args<bp::return_constant_t<StateVal>, int>;

template <typename T>
concept button_list_args = requires(T const &t, std::size_t i) {
  t.width();
  t.height();
  { t.button_state(i) } -> state_marker;
};

namespace radio_button {
/// Enum class to define the various render-related states for a radio button.
using element_state = toggle_button_states;

static constexpr auto max_element_state = element_state::hold_on;

/// Struct to signal that a radio button is activated.
struct trigger_on {};

/// Struct to signal that a radio button is deactivated.
struct trigger_off {};

/// State marker containing all states for the radio button.
using state_marker_t = make_widget_state_marker_sequence_t<
    element_state, element_state::relaxed_off, max_element_state>;

using all_states_t = all_states_in_marker_t<state_marker_t>;
using all_triggers_t = triggers<trigger_on, trigger_off>;

template <typename T,
          typename BP =
              basic_widget_back_propagater<point_unit_t<default_rect>>>
concept can_trigger =
    has_handle<T, trigger_on, BP &&> && has_handle<T, trigger_off, BP &&>;

template <typename T, typename TRender = dummy_renderer,
          typename Position = default_coordinate,
          typename BP =
              basic_widget_back_propagater<point_unit_t<default_rect>>>
concept element =
    has_render<
        T, TRender,
        basic_button_list_args<
            bp::return_constant_t<state_marker_t, element_state{}>, int>> &&
    requires(T &&t, Position const &p) {
      call::find_sub(t, bp::false_predicate, bp::no_op);
      { call::intrinsic_min_size(t) } -> point_rect;
    };
struct sub_constraint {
  constexpr void operator()(element auto &&) const {}
};
} // namespace radio_button

/// @brief Trigger for widgets that acts like a container of multiple buttons
/// where at most one button should be enabled.
///
template <radio_button::element TElements>
class radio_button_trigger_impl : bp::empty_structs_optimiser<TElements> {
  using base_t = bp::empty_structs_optimiser<TElements>;
  using state_marker_t = radio_button::state_marker_t;

  template <typename Sub, subable_widget_back_propagator BP>
  static constexpr void do_set_state(state_marker_t state, Sub &&sub,
                                     BP &&back_prop) {
    using namespace radio_button;
    if constexpr (has_set_state<Sub, state_marker_t, BP>) {
      call::set_state(sub, state, back_prop);
    } else {
      // If the group itself does not provide a handler to tell what really
      // needs to be re-rendered, we have to assume all will be.
      back_prop.rerender();
    }
  }

  struct do_trigger_off {
    basic_widget_back_propagater<point_unit_t<default_rect>> bp;
    bool currently_hovered{};

    template <typename Sub> constexpr void operator()(Sub &&s, auto &&...) {
      if constexpr (has_handle<Sub, radio_button::trigger_off, decltype(bp)>) {
        call::handle(std::forward<Sub>(s), radio_button::trigger_off{},
                     std::move(bp));
      }
      do_set_state(currently_hovered ? radio_button::element_state::hover_off
                                     : radio_button::element_state::relaxed_off,
                   s, bp);
    }
  };
  using element_id_t = std::size_t;
  using reset_active_f =
      decltype(call::sub_accessor(std::declval<TElements &>(), element_id_t{},
                                  arguments_marker<do_trigger_off>));

  std::optional<reset_active_f> reset_active_;
  element_id_t current_element_ = highest_possible;
  element_id_t hovered_element_ = highest_possible;
  bool mouse_down_{};

  static constexpr decltype(auto) elements(auto &&self) noexcept {
    using t = bp::copy_cvref_t<base_t, decltype(self)>;
    return get<0>(static_cast<t>(self));
  }
  constexpr decltype(auto) elements() noexcept { return elements(*this); }
  constexpr decltype(auto) elements() const noexcept { return elements(*this); }

  constexpr void reset_active(widget_back_propagater auto &&back_prop,
                              bool is_hovered) {
    if (reset_active_) {
      (*reset_active_)(
          elements(),
          do_trigger_off{
              static_cast<
                  basic_widget_back_propagater<point_unit_t<default_rect>>>(
                  back_prop),
              is_hovered});
    }
  }
  template <widget_back_propagater BP>
  constexpr void reset_hovered(BP &&back_prop) {
    if (hovered_element_ != static_cast<element_id_t>(highest_possible)) {
      call::find_sub_id(
          elements(), hovered_element_,
          [this, &back_prop]<typename S>(S &&s, element_id_t const &i) {
            using enum radio_button::element_state;
            if constexpr (has_set_state<S, state_marker_t, BP>) {
              call::set_state(s,
                              state_marker_t(combine_toggled_hover_states(
                                  i == current_element_, false, false)),
                              std::forward<BP>(back_prop));
            }
          });
    }
  }

  template <typename Sub, widget_back_propagater BP>
  constexpr void activate_element(Sub &&sub, element_id_t sub_index,
                                  BP &&back_prop) {
    if constexpr (has_handle<Sub, radio_button::trigger_on, BP>) {
      call::handle(sub, radio_button::trigger_on{}, back_prop);
    }
    do_set_state(radio_button::element_state::hover_on, sub, back_prop);
    reset_active_.emplace(call::sub_accessor(elements(*this), sub_index,
                                             arguments_marker<do_trigger_off>));
    current_element_ = sub_index;
  }

  template <typename Sub, typename BP>
  constexpr void hover_element(Sub &&sub, element_id_t sub_index,
                               BP &&back_prop) {
    hovered_element_ = sub_index;
    using namespace radio_button;
    if constexpr (has_set_state<Sub, state_marker_t, BP>) {
      call::set_state(sub,
                      state_marker_t(combine_toggled_hover_states(
                          sub_index == current_element_, true, mouse_down_)),
                      back_prop);
    } else {
      back_prop.rerender(sub.area());
    }
  }

  template <typename BP, typename Sub, typename SubIndex>
  constexpr auto intr_event_switch(BP &bp, Sub &sub, SubIndex &sub_index) {
    using enum interpreted_events;

    return saved_ui_event_switch(
        std::forward_as_tuple(*this, bp, sub, sub_index),
        event_case<pointer_exit>([](auto const &, auto &&data) {
          auto &[self, back_prop, sub, sub_index] = data;
          self.reset_hovered(back_prop);
        }),
        event_case<pointer_hover>([](auto const &, auto &&data) {
          auto &[self, back_prop, sub, sub_index] = data;
          bool is_correct_state = !self.mouse_down_;
          self.mouse_down_ = false;
          if (sub_index != self.hovered_element_) {
            self.reset_hovered(back_prop);
            self.hover_element(sub, sub_index, back_prop);
          } else if (!is_correct_state) {
            do_set_state(combine_toggled_hover_states(false, false, true), sub,
                         back_prop);
          }
        }),
        event_case<pointer_hold>([](auto const &, auto &&data) {
          auto &[self, back_prop, sub, sub_index] = data;
          bool is_correct_state = self.mouse_down_;
          self.mouse_down_ = true;
          if (sub_index != self.hovered_element_) {
            self.reset_hovered(back_prop);
            self.hover_element(sub, sub_index, back_prop);
          } else if (!is_correct_state) {
            do_set_state(combine_toggled_hover_states(false, true, true), sub,
                         back_prop);
          }
        }),
        event_case<primary_click>([](auto const &, auto &&data) {
          auto &[self, back_prop, sub, sub_index] = data;
          self.reset_active(back_prop, sub_index == self.current_element_);
          if (sub_index != self.current_element_) {
            self.activate_element(std::forward<Sub>(sub), sub_index,
                                  std::forward<BP>(back_prop));
          } else {
            self.reset_active_ = std::nullopt;
            self.current_element_ = highest_possible;
          }
          self.mouse_down_ = false;
        }) //
    );
  }

public:
  using base_t::base_t;
  template <
      typename A,
      interpreted_event_types<
          interpreted_events::pointer_exit, interpreted_events::pointer_hover,
          interpreted_events::pointer_hold, interpreted_events::primary_click>
          Evt,
      subable_widget_back_propagator BP>
  constexpr void handle(A const &, Evt &&evt, BP &&back_prop) {
    if constexpr (can_be_event<interpreted_events::pointer_exit, Evt>()) {
      if (is_event<interpreted_events::pointer_exit>(evt)) {
        reset_hovered(back_prop);
        hovered_element_ = highest_possible;
        return;
      }
    }
    if constexpr (interpreted_event_types<Evt,
                                          interpreted_events::pointer_hover,
                                          interpreted_events::pointer_hold,
                                          interpreted_events::primary_click>) {
      if (!call::find_sub_at_location(elements(), call::position(evt),
                                      [this, &back_prop, &evt]<typename Sub>(
                                          Sub &&s, element_id_t index) {
                                        this->intr_event_switch(back_prop, s,
                                                                index)(
                                            std::forward<Evt>(evt));
                                      })) {
        reset_hovered(back_prop);
        hovered_element_ = highest_possible;
        event_case<interpreted_events::pointer_hover>(
            [this](auto &&...) { mouse_down_ = false; })(evt);
        event_case<interpreted_events::pointer_hold>(
            [this](auto &&...) { mouse_down_ = true; })(evt);
      }
    }
  }

  constexpr void render(renderer auto &&r, render_args auto &&args) const {
    auto bargs = basic_button_list_args(
        call::width(args), call::height(args), [this](element_id_t const &i) {
          return radio_button::state_marker_t(combine_toggled_hover_states(
              i == current_element_, i == hovered_element_, mouse_down_));
        });
    call::render(elements(), std::forward<decltype(r)>(r), bargs);
  }
  /// @brief Gets mutable access to elements object.
  ///
  /// Makes sure that the button list states are valid after the mutation.
  /// Provides strong exception guarantee under the following restrictions:
  ///  - cb provides strong exception guarantee
  ///  - all activation functions and deactivation functions are perfect
  ///  inversions of each others (i.e. calling an activation function and
  ///  immediately the corresponding deactivation function or vice-versa will
  ///  have no effects on the rest of the program)
  /// \param cb
  /// \return
  constexpr void mutate_elements(std::invocable<TElements &> auto &&cb) {
    auto backprop = basic_widget_back_propagater(point_unit_t<default_rect>{});
    if (reset_active_) {
      reset_active(backprop, false);
      reset_active_ = std::nullopt;
    }
    if (hovered_element_ != highest_possible) {
      reset_hovered(backprop);
    }
    auto def = bp::deferred([this, &backprop] {
      if (!call::find_sub_id(
              elements(), current_element_,
              [this, &backprop]<typename S>(S &&sub, element_id_t id) {
                this->activate_element(std::forward<S>(sub), id, backprop);
              })) {
        current_element_ = highest_possible;
      }
      if (!call::find_sub_id(
              elements(), hovered_element_,
              [this, &backprop]<typename S>(S &&sub, element_id_t id) {
                this->hover_element(std::forward<S>(sub), id, backprop);
              })) {
        hovered_element_ = highest_possible;
      }
    });
    cb(elements());
  }

  constexpr point_rect decltype(auto) intrinsic_min_size() const {
    return call::intrinsic_min_size(this->get_first());
  }
};

class subs_group {
public:
  constexpr void render(auto &&...) const {}
  constexpr bool find_sub(auto &&...) { return false; }
  constexpr void handle(auto &&...) {}
  constexpr default_point_rect intrinsic_min_size() const { return {}; }
};

template <typename TElements = subs_group>
class radio_button_trigger : bp::empty_structs_optimiser<TElements> {
  using base_t = bp::empty_structs_optimiser<TElements>;

  constexpr base_t &&move_base() {
    return std::move(static_cast<base_t &>(*this));
  }

  static constexpr auto do_build_group(auto &&g) {
    return build::build_group(
        radio_button::sub_constraint{}, std::forward<decltype(g)>(g),
        radio_button::all_states_t{}, radio_button::all_triggers_t{});
  }

  using built_elements_t =
      decltype(do_build_group(std::declval<TElements &&>()));
  static_assert(!std::is_void_v<built_elements_t>);

  constexpr auto build_group() { return do_build_group(get<0>(move_base())); }

public:
  using base_t::base_t;

  template <typename TE, typename TR = std::unwrap_ref_decay_t<TE>>
  constexpr radio_button_trigger<TR> elements(TE &&e) && {
    return radio_button_trigger<TR>(std::forward<TE>(e));
  }

  constexpr auto build() && -> radio_button_trigger_impl<built_elements_t>
    requires(!std::is_void_v<built_elements_t>)
  {
    return radio_button_trigger_impl<built_elements_t>(build_group());
  }
};

radio_button_trigger() -> radio_button_trigger<subs_group>;

struct zoom_args_t {};
struct pan_args_t {};

namespace view_port_trigger {

template <typename T, typename TRender = dummy_renderer,
          typename RenderArgs = widget_render_args<>>
concept sub_widget =
    has_render<T, TRender &&, RenderArgs const &> && requires(T const &t) {
      { call::area(t) } -> point_rect;
    };

template <renderer TRender = dummy_renderer,
          render_args RenderArgs = widget_render_args<>>
struct sub_widget_constraint {
  template <sub_widget<TRender, RenderArgs> T>
  constexpr void operator()(T &&) const noexcept {}
};

template <typename V, bool zoomable>
class impl : bp::empty_structs_optimiser<V> {
  using _base_t = bp::empty_structs_optimiser<V>;
  constexpr decltype(auto) viewed_area() const {
    return call::area(_base_t::get_first());
  }

  default_point_coordinate pan_{};
  float scale_ = 1.f;

  constexpr default_point_coordinate
  clamped_pan(default_point_coordinate p0, point_scalar auto const &w) const {
    using scalar_t = std::remove_cvref_t<decltype(call::x_of(p0))>;
    auto lx = call::l_x(viewed_area());
    auto ty = call::t_y(viewed_area());
    return {std::clamp<scalar_t>(
                call::x_of(p0), lx,
                std::max<scalar_t>(call::r_x(viewed_area()) - w, lx)),
            std::clamp<scalar_t>(
                call::y_of(p0), ty,
                std::max<scalar_t>(call::b_y(viewed_area()) - w, ty))};
  }

  constexpr auto evt_switch() {
    return saved_ui_event_switch(
        std::ref(*this),
        event_case<interpreted_events::scroll>([](auto const &e, impl &self,
                                                  auto &&bp,
                                                  bounding_box auto const &a) {
          auto naive_x = call::x_of(self.pan_) + point_unit(call::delta_x(e));
          auto naive_y = call::y_of(self.pan_) + point_unit(call::delta_y(e));
          self.pan_ = self.clamped_pan({naive_x, naive_y}, call::width(a));
          bp.rerender();
        }),
        event_case<interpreted_events::zoom>(
            [](auto const &e, impl &self, auto &&bp, auto &&) {
              if constexpr (zoomable) {
                float new_scale = (call::scale_x(e) + call::scale_y(e)) / 2.f;
                if (new_scale != self.scale_) {
                  self.scale_ = new_scale;
                  bp.rerender();
                }
              }
            }));
  }

public:
  using _base_t::_base_t;

  constexpr void render(renderer auto &&r, render_args auto &&args) const {
    call::render(
        this->get_first(),
        r.translate(clamped_pan(pan_, call::width(args))).scale(scale_), args);
  }
  template <bounding_box A, typename E, widget_back_propagater BP>
    requires(

        interpreted_event_types<E,
                                interpreted_events::scroll //
                                > ||
        (zoomable && interpreted_event_types<E, interpreted_events::zoom>) ||
        has_handle<V &, A, E, BP>)
  constexpr void handle(A const &area, E const &event, BP &&bp) {
    if (!evt_switch()(event, std::forward<BP>(bp), area)) {
      if constexpr (has_handle<V &, A, E, BP>) {
        call::handle(this->get_first(), area, event, bp);
      }
    }
  }
};

template <typename V, bool zoomable>
class builder_impl : bp::empty_structs_optimiser<V> {
  using _base_t = bp::empty_structs_optimiser<V>;

  friend class builder_impl<V, !zoomable>;

public:
  constexpr builder_impl() = default;
  constexpr explicit(false) builder_impl(builder_impl<V, !zoomable> &&o)
      : _base_t(static_cast<_base_t &&>(o)) {}

  template <typename V2>
    requires(std::constructible_from<_base_t, V2>)
  constexpr explicit builder_impl(V2 &&v) : _base_t(std::forward<V2>(v)) {}

  constexpr auto build() && {
    using v_t = decltype(build::return_or_build<sub_widget_constraint<>>(
        std::move(*this).get_first()));
    using result_t = impl<v_t, zoomable>;
    static_assert(sub_widget<v_t>);
    // using result_t = impl<
    return result_t(build::return_or_build<sub_widget_constraint<>>(
        std::move(*this).get_first()));
  }
  template <build::fulfill_or_after_build<sub_widget_constraint<>> SW>
  constexpr builder_impl<SW, zoomable> view(SW &&sw) && {
    return builder_impl<SW, zoomable>(std::forward<SW>(sw));
  }
  constexpr builder_impl<V, true> enable_zoom() && { return std::move(*this); }
};

constexpr builder_impl<empty_placeholder_t, false> builder() { return {}; }
} // namespace view_port_trigger

} // namespace cgui

#endif // COMPONENT_GUI_CGUI_HPP

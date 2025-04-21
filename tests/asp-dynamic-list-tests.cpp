
#include <iterator>
#include <utility>
#include <vector>

#include <gmock/gmock.h>

#include <asp/asp-types.hpp>
#include <asp/list_layouts.hpp>
#include <asp_test_utils.hpp>

namespace asp {
using namespace testing;
template <typename> inline constexpr bool handle_on_all_cond = false;

template <typename T>
concept handle_on_all = handle_on_all_cond<std::remove_cvref_t<T>>;

namespace uniform_list {
template <typename T>
concept element = !std::same_as<T, void>;
template <element T, is_layout L> class impl : bp::empty_structs_optimiser<L> {
  using _base_t = bp::empty_structs_optimiser<L>;
  std::vector<T> items_;

  constexpr is_layout decltype(auto) _layout() const {
    return this->get_first();
  }

public:
  using _base_t::_base_t;
  constexpr void add_item(auto &&...args)
    requires(std::constructible_from<T, decltype(args)...>)
  {
    items_.emplace_back(std::forward<decltype(args)>(args)...);
  }
  constexpr void handle(auto &&e, point_rect auto const &a,
                        subable_widget_back_propagator<> auto &&bp)
    requires(has_handle<T, decltype(e), decltype(a), decltype(bp)>)
  {
    if constexpr (handle_on_all<decltype(e)>) {
      for (auto &item : items_) {
        auto item_index = std::distance(items_.data(), std::addressof(item));
        call::handle(item, e, _layout().area_for_index(item_index, a), bp);
      }
    } else {
      static_assert(has_point_position<decltype(e)>,
                    "All events sent to the list must either have the "
                    "'handle_on_all_cond' specialised to 'true' or have a "
                    "valid point position");
      auto item_index = _base_t::get_first().index_at(call::position(e), a);
      if (item_index >= 0 && item_index < ssize(items_)) {
        call::handle(items_[item_index], e,
                     _layout().area_for_index(item_index, a), bp);
      }
    }
  }
};
template <element T, typename Layout>
class builder_impl : bp::empty_structs_optimiser<Layout> {
  using _base_t = bp::empty_structs_optimiser<Layout>;

  constexpr _base_t &&moved_base() { return static_cast<_base_t &&>(*this); }

public:
  constexpr builder_impl() = default;
  using _base_t::_base_t;

  template <typename L>
    requires(is_layout<std::remove_cvref_t<L>>)
  constexpr builder_impl<T, std::remove_cvref_t<L>> layout(L &&l) && {
    return builder_impl<T, std::remove_cvref_t<L>>(std::forward<L>(l));
  }
  constexpr auto build() && requires(is_layout<Layout>) {
    return call::apply_to(moved_base(), []<typename... Ts>(Ts &&...args) {
      return impl<T, Layout>(std::forward<Ts>(args)...);
    });
  }
};

template <element T> constexpr builder_impl<T, empty_placeholder_t> builder() {
  return {};
}
} // namespace uniform_list

struct dummy_event {};
template <> inline constexpr bool handle_on_all_cond<dummy_event> = true;
struct positioned_dummy_event {
  default_point_coordinate pos;
  constexpr default_point_coordinate position() const { return pos; }
};

struct dummy_element_t {
  std::function<void(default_point_rect const &)> on_handle_dummy = bp::no_op;
  dummy_element_t() = default;
  explicit dummy_element_t(std::invocable auto &&cb)
      : on_handle_dummy([cb](default_point_rect const &) { cb(); }) {}
  explicit dummy_element_t(std::invocable<default_point_rect const &> auto &&cb)
      : on_handle_dummy(cb) {}

  void handle(dummy_event const &, auto &&a, auto &&) { on_handle_dummy(a); }
  void handle(positioned_dummy_event const &, auto &&a, auto &&) {
    on_handle_dummy(a);
  }
};

struct UniformListWidget : Test {
  static auto constexpr default_full_size =
      default_point_rect{{{0, 0}, {16, 16}}};
  static auto constexpr default_layout = vertical_list_layout(point_unit(1));
  static inline auto default_builder() {
    return uniform_list::builder<dummy_element_t>().layout(default_layout);
  }
  basic_widget_back_propagater<> bp{default_full_size};
};

TEST_F(UniformListWidget, SendEventToSingleElement) // NOLINT
{
  int calls{};
  auto l = default_builder().build();
  l.add_item(dummy_element_t([&calls] { ++calls; }));
  EXPECT_THAT(calls, Eq(0));
  l.handle(dummy_event{}, default_full_size, bp);
  EXPECT_THAT(calls, Eq(1));
}
TEST_F(UniformListWidget, SendEventToTwoElements) // NOLINT
{
  std::vector<int> calls;
  auto l = default_builder().build();
  l.add_item(dummy_element_t([&calls] { calls.emplace_back(0); }));
  l.add_item(dummy_element_t([&calls] { calls.emplace_back(1); }));
  EXPECT_THAT(calls, IsEmpty());
  l.handle(dummy_event{}, default_full_size, bp);
  EXPECT_THAT(calls, ElementsAre(0, 1));
}
TEST_F(UniformListWidget, SendDummyEventCalledWithCorrectAreaForFirstElement) {
  default_point_rect called_area{};
  auto l = default_builder().build();
  l.add_item(dummy_element_t(
      [&called_area](default_point_rect const &r) { called_area = r; }));
  l.handle(dummy_event{}, default_full_size, bp);
  tests::expect_box_equal(
      called_area, default_point_rect{
                       {{0, 0}, {call::width(default_full_size).value(), 1}}});
}
TEST_F(UniformListWidget, SendDummyEventCalledWithCorrectAreaForSecondElement) {
  default_point_rect called_area{};
  auto l = default_builder().build();
  l.add_item(dummy_element_t([]() {}));
  l.add_item(dummy_element_t(
      [&called_area](default_point_rect const &r) { called_area = r; }));
  l.handle(dummy_event{}, default_full_size, bp);
  tests::expect_box_equal(
      called_area, default_point_rect{
                       {{0, 1}, {call::width(default_full_size).value(), 2}}});
}
TEST_F(UniformListWidget,
       SendPositionedEventToZeroZeroInvokesFirstElement) // NOLINT
{
  std::vector<int> calls;
  auto l = default_builder().build();
  l.add_item(dummy_element_t([&calls] { calls.emplace_back(0); }));
  l.add_item(dummy_element_t([&calls] { calls.emplace_back(1); }));
  EXPECT_THAT(calls, IsEmpty());
  l.handle(positioned_dummy_event{default_point_coordinate{}},
           default_full_size, bp);
  EXPECT_THAT(calls, ElementsAre(0));
}
TEST_F(UniformListWidget,
       SendPositionedDummyEventCalledWithCorrectAreaForFirstElement) {
  default_point_rect called_area{};
  auto l = default_builder().build();
  l.add_item(dummy_element_t(
      [&called_area](default_point_rect const &r) { called_area = r; }));
  l.handle(positioned_dummy_event{default_point_coordinate{}},
           default_full_size, bp);
  tests::expect_box_equal(
      called_area, default_point_rect{
                       {{0, 0}, {call::width(default_full_size).value(), 1}}});
}
TEST_F(UniformListWidget,
       SendPositionedEventToZeroOneInvokesSecondElement) // NOLINT
{
  std::vector<int> calls;
  auto l = default_builder().build();
  l.add_item(dummy_element_t([&calls] { calls.emplace_back(0); }));
  l.add_item(dummy_element_t([&calls] { calls.emplace_back(1); }));
  EXPECT_THAT(calls, IsEmpty());
  l.handle(positioned_dummy_event{default_point_coordinate{{0, 1}}},
           default_full_size, bp);
  EXPECT_THAT(calls, ElementsAre(1));
}
TEST_F(UniformListWidget,
       SendPositionedDummyEventCalledWithCorrectAreaForSecondElement) {
  default_point_rect called_area{};
  auto l = default_builder().build();
  l.add_item(dummy_element_t([&](default_point_rect const &) {}));
  l.add_item(dummy_element_t(
      [&called_area](default_point_rect const &r) { called_area = r; }));
  l.handle(positioned_dummy_event{default_point_coordinate{{0, 1}}},
           default_full_size, bp);
  tests::expect_box_equal(
      called_area, default_point_rect{
                       {{0, 1}, {call::width(default_full_size).value(), 2}}});
}
} // namespace asp

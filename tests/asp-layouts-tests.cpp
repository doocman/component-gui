
#include <asp/list_layouts.hpp>

#include <gmock/gmock.h>

#include <asp/asp.hpp>

namespace asp::tests {
enum class my_button_states { on, off };
struct trigger_on {};
struct trigger_off {};

static_assert(is_layout<vertical_list_layout>);
using namespace ::testing;
struct VerticalListLayout : Test {
  static auto constexpr wrapped_area = default_point_size_wh{{100, 200}};
};

TEST_F(VerticalListLayout, FirstElementIsPlacedAtXY00) // NOLINT
{
  auto l = vertical_list_layout(point_unit(16));
  auto item_area = l.area_for_index(0, wrapped_area);
  EXPECT_THAT(call::l_x(item_area).value(), Eq(0));
  EXPECT_THAT(call::t_y(item_area).value(), Eq(0));
}
TEST_F(VerticalListLayout, SecondElementIsPlacedAtXYZeroAndSiz) // NOLINT
{
  auto l = vertical_list_layout(point_unit(16));
  auto item_area = l.area_for_index(1, wrapped_area);
  EXPECT_THAT(call::l_x(item_area).value(), Eq(0));
  EXPECT_THAT(call::t_y(item_area).value(), Eq(16));
}
TEST_F(VerticalListLayout, SecondElementAreaIsValid) // NOLINT
{
  auto l = vertical_list_layout(point_unit(16));
  EXPECT_THAT(valid_box(l.area_for_index(1, wrapped_area)), IsTrue());
}
TEST_F(VerticalListLayout, FirstElementHasCTorInputHeightAsHeight) // NOLINT
{
  auto constexpr element_height = point_unit(7);
  auto l = vertical_list_layout(element_height);
  EXPECT_THAT(call::height(l.area_for_index(0, wrapped_area)).value(),
              Eq(element_height.value()));
}
TEST_F(VerticalListLayout, FirstElementHasAreaInputWidthAsWidth) // NOLINT
{
  auto constexpr element_height = point_unit(7);
  auto l = vertical_list_layout(element_height);
  EXPECT_THAT(call::width(l.area_for_index(0, wrapped_area)).value(),
              Eq(call::width(wrapped_area).value()));
}
TEST_F(VerticalListLayout, IndexAtLocZeroZeroIsZero) // NOLINT
{
  auto constexpr element_height = point_unit(10);
  auto l = vertical_list_layout(element_height);
  EXPECT_THAT(l.index_at(default_point_coordinate{}, wrapped_area), Eq(0));
}
TEST_F(VerticalListLayout, IndexAtLocTenAndElementHeightIsOne) // NOLINT
{
  auto constexpr element_height = point_unit(10);
  auto l = vertical_list_layout(element_height);
  EXPECT_THAT(l.index_at(default_point_coordinate{{0, element_height.value()}},
                         wrapped_area),
              Eq(1));
}

} // namespace asp::tests


#include <cgui/std-backport/expected.hpp>
#include <cgui/std-backport/tuple.hpp>
#include <cgui/std-backport/utility.hpp>
#include <cgui/stl_extend.hpp>

#include <type_traits>

#include <gmock/gmock.h>

namespace cgui::tests {
static_assert(std::is_rvalue_reference_v<
              decltype(cgui::bp::details::expected_member<int, bool>::exp(
                  std::declval<
                      cgui::bp::details::expected_member<int, bool> &&>()))>);
static_assert(std::is_rvalue_reference_v<
              decltype(cgui::bp::details::expected_member<int, bool>::err(
                  std::declval<
                      cgui::bp::details::expected_member<int, bool> &&>()))>);

using namespace ::testing;

struct empty_struct {};

TEST(TupleForEach, Order) // NOLINT
{
  auto vals = std::tuple(1, 2, 3);
  std::vector<int> vals_res;
  tuple_for_each([&vals_res](auto a) { vals_res.emplace_back(a); }, vals);
  EXPECT_THAT(vals_res, ElementsAre(Eq(1), Eq(2), Eq(3)));
}
TEST(EtdExpected, Basics) // NOLINT
{
  using namespace cgui::bp;
  auto val = expected<bool, int>(true);
  EXPECT_THAT(val.has_value(), Eq(true));
  EXPECT_THAT(val.value(), Eq(true));
  val = expected<bool, int>(unexpected(2));
  EXPECT_THAT(val.has_value(), Eq(false));
}
TEST(EtdExpected, SameTypes) // NOLINT
{
  using namespace cgui::bp;
  auto val = expected<int, int>(1);
  EXPECT_THAT(val.has_value(), Eq(true));
  EXPECT_THAT(val.value(), Eq(1));
  val = expected<int, int>(unexpected(2));
  EXPECT_THAT(val.has_value(), Eq(false));
  EXPECT_THAT(val.error(), Eq(2));
}
TEST(EtdExpected, VoidTypes) // NOLINT
{
  using namespace cgui::bp;
  auto val = expected<void, int>();
  EXPECT_THAT(val.has_value(), Eq(true));
  val = expected<void, int>(unexpected(2));
  EXPECT_THAT(val.has_value(), Eq(false));
  EXPECT_THAT(val.error(), Eq(2));
}
TEST(EtdEmptyBaseOptimiser, Empty) // NOLINT
{
  EXPECT_THAT(sizeof(bp::empty_structs_optimiser<>), Eq(1));
  EXPECT_THAT(sizeof(bp::empty_structs_optimiser<empty_struct>), Eq(1));
  EXPECT_THAT(sizeof(bp::empty_structs_optimiser<empty_struct, empty_struct>),
              Eq(1));
}
TEST(EtdEmptyBaseOptimiser, GetType) // NOLINT
{
  auto tested = bp::empty_structs_optimiser<int, float, double>();
  tested.get(std::type_identity<int>{}) = 1;
  tested.get(std::type_identity<float>{}) = 2.f;
  tested.get(std::type_identity<double>{}) = 3.;
  EXPECT_THAT(tested.get(std::type_identity<int>{}), Eq(1));
  EXPECT_THAT(tested.get(std::type_identity<float>{}), Eq(2.f));
  EXPECT_THAT(tested.get(std::type_identity<double>{}), Eq(3.));
}
TEST(EtdEmptyBaseOptimiser, GetIndex) // NOLINT
{
  auto tested = bp::empty_structs_optimiser<int, int, empty_struct, int>();
  get<0>(tested) = 1;
  get<1>(tested) = 2;
  get<3>(tested) = 3;
  EXPECT_THAT(sizeof(tested), Eq(sizeof(int) * 3));
  EXPECT_THAT(get<0>(tested), Eq(1));
  EXPECT_THAT(get<1>(tested), Eq(2));
  EXPECT_THAT(get<3>(tested), Eq(3));
}

} // namespace cgui::tests

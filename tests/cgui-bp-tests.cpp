
#include <cgui/std-backport/expected.hpp>
#include <cgui/std-backport/functional.hpp>
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

TEST(EtdTrivialFunction, EmptyLambdaTarget) // NOLINT
{
  int my_val{};
  auto the_func =
      bp::trivial_function<void(int &), 1, 1>([](int &i) { i += 1; });
  EXPECT_THAT(my_val, Eq(0));
  the_func(my_val);
  EXPECT_THAT(my_val, Eq(1));
  auto f2 = the_func;
  EXPECT_THAT(my_val, Eq(1));
  f2(my_val);
  EXPECT_THAT(my_val, Eq(2));
  static_assert(std::is_nothrow_move_constructible_v<decltype(the_func)>);
  static_assert(std::is_nothrow_move_assignable_v<decltype(the_func)>);
  static_assert(std::is_nothrow_swappable_v<decltype(the_func)>);
}

TEST(EtdTrivialFunction, StatefulLambdaTarget) // NOLINT
{
  int my_val{};
  auto the_func = bp::trivial_function<void(), sizeof(int *), alignof(int *)>(
      [&my_val]() { my_val += 1; });
  EXPECT_THAT(my_val, Eq(0));
  the_func();
  EXPECT_THAT(my_val, Eq(1));
  auto f2 = the_func;
  EXPECT_THAT(my_val, Eq(1));
  f2();
  EXPECT_THAT(my_val, Eq(2));
  static_assert(std::is_nothrow_move_constructible_v<decltype(the_func)>);
  static_assert(std::is_nothrow_move_assignable_v<decltype(the_func)>);
  static_assert(std::is_nothrow_swappable_v<decltype(the_func)>);
}

TEST(EtdTrivialFunction, ExcessSize) // NOLINT
{
  int my_val{};
  int unused_val{};
  auto the_func =
      bp::trivial_function<void(), sizeof(int *) * 3, alignof(int *)>(
          [unused_val, &my_val]() {
            my_val += 1;
            unused(unused_val);
          });
  EXPECT_THAT(my_val, Eq(0));
  the_func();
  EXPECT_THAT(my_val, Eq(1));
  auto f2 = the_func;
  EXPECT_THAT(my_val, Eq(1));
  f2();
  EXPECT_THAT(my_val, Eq(2));
  static_assert(std::is_nothrow_move_constructible_v<decltype(the_func)>);
  static_assert(std::is_nothrow_move_assignable_v<decltype(the_func)>);
  static_assert(std::is_nothrow_swappable_v<decltype(the_func)>);
}

TEST(EtdTrivialFunction, IsAssignable) // NOLINT
{
  auto the_func = bp::trivial_function<void(int &), 1, 1>();
  the_func = [](int &i) { ++i; };
  int i = 0;
  EXPECT_THAT(i, Eq(0));
  the_func(i);
  EXPECT_THAT(i, Eq(1));
  the_func(i);
  EXPECT_THAT(i, Eq(2));
}

struct large_functionlike {
  char b[64];

  void operator()() const {}
};

struct alignas(16) heavy_align_functionlike {
  void operator()() const {}
};

static_assert(!std::constructible_from<bp::trivial_function<void(), 1, 8>,
                                       large_functionlike>);
static_assert(std::constructible_from<bp::trivial_function<void(), 64, 1>,
                                      large_functionlike>);
static_assert(!std::constructible_from<bp::trivial_function<void(), 1, 8>,
                                       heavy_align_functionlike>);
static_assert(std::constructible_from<bp::trivial_function<void(), 1, 32>,
                                      heavy_align_functionlike>);

TEST(EtdTrivialFunction, OperatorBool) // NOLINT
{
  auto f = bp::trivial_function<void(), 1, 1>();
  EXPECT_FALSE(f);
  f = bp::no_op;
  EXPECT_TRUE(f);
}

} // namespace cgui::tests

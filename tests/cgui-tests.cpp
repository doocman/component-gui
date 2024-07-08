
#include <cgui/stl_extend.hpp>

#include <tuple>

#include <gmock/gmock.h>

namespace cgui::tests {
using namespace ::testing;
TEST(TupleForEach, Order) //NOLINT
{
  auto vals = std::tuple(1, 2, 3);
  std::vector<int> vals_res;
  tuple_for_each([&vals_res] (auto a) {
    vals_res.emplace_back(a);
  }, vals);
  EXPECT_THAT(vals_res, ElementsAre(Eq(1), Eq(2), Eq(3)));
}
}


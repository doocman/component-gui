
#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/expected.hpp>
#include <cgui/stl_extend.hpp>

#include <tuple>

#include <gmock/gmock.h>

#include <cgui/std-backport/concepts.hpp>

namespace cgui::tests {
using namespace ::testing;

static_assert(std::is_rvalue_reference_v<
              decltype(cgui::bp::details::expected_member<int, bool>::exp(
                  std::declval<
                      cgui::bp::details::expected_member<int, bool> &&>()))>);
static_assert(std::is_rvalue_reference_v<
              decltype(cgui::bp::details::expected_member<int, bool>::err(
                  std::declval<
                      cgui::bp::details::expected_member<int, bool> &&>()))>);

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

TEST(XxWh, BasicXy2Wh) // NOLINT
{
  int x{};
  int w{};
  auto w2x = cgui::xxyy_xwyh_conv(x, w, wh2xy);
  EXPECT_THAT(int(w2x), Eq(0));
  w = 2;
  EXPECT_THAT(int(w2x), Eq(2));
  x = 1;
  EXPECT_THAT(int(w2x), Eq(3));
  w2x = 4;
  EXPECT_THAT(int(w2x), Eq(4));
  EXPECT_THAT(int(w), Eq(4 - x));
}
TEST(XxWh, BasicWh2Xy) // NOLINT
{
  int xl{};
  int xr{};
  auto w2x = cgui::xxyy_xwyh_conv(xl, xr, xy2wh);
  EXPECT_THAT(int(w2x), Eq(0));
  xr = 2;
  EXPECT_THAT(int(w2x), Eq(2));
  xl = 1;
  EXPECT_THAT(int(w2x), Eq(1));
  w2x = 4;
  EXPECT_THAT(int(w2x), Eq(4));
  EXPECT_THAT(int(xr), Eq(4 + xl));
}

namespace apitests {
struct mut_pix_coord {
  int xmut, ymut;
};
struct set_pix_coord {
  int xset, yset;
};
template <typename T>
concept pix_mut_type = bp::cvref_type<T, mut_pix_coord>;
template <typename T>
concept pix_set_type = bp::cvref_type<T, set_pix_coord>;
constexpr auto &x_of(pix_mut_type auto &&v) { return v.xmut; }
constexpr auto &y_of(pix_mut_type auto &&v) { return v.ymut; }
constexpr auto x_of(pix_set_type auto const &v) { return v.xset; }
constexpr auto y_of(pix_set_type auto const &v) { return v.yset; }
constexpr void x_of(pix_set_type auto &&v, auto &&val) { v.xset = val; }
constexpr void y_of(pix_set_type auto &&v, auto &&val) { v.yset = val; }

struct xxyy_mut {
  int x1, x2, y1, y2;
};
template <bp::cvref_type<xxyy_mut> T> constexpr auto &&tl_x(T &&v) {
  return v.x1;
}
template <bp::cvref_type<xxyy_mut> T> constexpr auto &&tl_y(T &&v) {
  return v.y1;
}
template <bp::cvref_type<xxyy_mut> T> constexpr auto &&br_x(T &&v) {
  return v.x2;
}
template <bp::cvref_type<xxyy_mut> T> constexpr auto &&br_y(T &&v) {
  return v.y2;
}

struct xxyy_set {
  int x1, x2, y1, y2;
};
template <bp::cvref_type<xxyy_set> T> constexpr auto tl_x(T const &v) {
  return v.x1;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto tl_x(T &&v, auto &&val) {
  return v.x1 = val;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto tl_y(T const &v) {
  return v.y1;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto tl_y(T &&v, auto &&val) {
  return v.y1 = val;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto br_x(T const &v) {
  return v.x2;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto br_x(T &&v, auto &&val) {
  return v.x2 = val;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto br_y(T const &v) {
  return v.y2;
}
template <bp::cvref_type<xxyy_set> T> constexpr auto br_y(T &&v, auto &&val) {
  return v.y2 = val;
}

struct xwyh_mut {
  int x, y, w, h;
};
template <bp::cvref_type<xwyh_mut> T> constexpr auto &&tl_x(T &&v) {
  return v.x;
}
template <bp::cvref_type<xwyh_mut> T> constexpr auto &&tl_y(T &&v) {
  return v.y;
}
template <bp::cvref_type<xwyh_mut> T> constexpr auto &&width(T &&v) {
  return v.w;
}
template <bp::cvref_type<xwyh_mut> T> constexpr auto &&height(T &&v) {
  return v.h;
}
struct xwyh_set {
  int x,y,w,h;
  constexpr int tl_x() const {
    return x;
  }
  constexpr int tl_y() const {
    return y;
  }
  constexpr int width() const {
    return w;
  }
  constexpr int height() const {
    return h;
  }

  constexpr void tl_x(int v) {
    x = v;
  }
  constexpr void tl_y(int v) {
    y = v;
  }
  constexpr void width(int v) {
    w = v;
  }
  constexpr void height(int v) {
    h = v;
  }
};

struct tlbr_mut {
  mut_pix_coord tl;
  set_pix_coord br;
};

template <bp::cvref_type<tlbr_mut> T> constexpr auto &&top_left(T &&t) {
  return t.tl;
}
template <bp::cvref_type<tlbr_mut> T> constexpr auto &&bottom_right(T &&t) {
  return t.br;
}

template <typename T> class PixCoordFixture : public ::testing::Test {
public:
  T value{};
};

using PixCoordTypes = ::testing::Types<mut_pix_coord, set_pix_coord>;

template <typename T> class RectApiFixture : public ::testing::Test {
public:
  T value{};
};

using RectApiTypes = ::testing::Types<xxyy_set, xxyy_mut, xwyh_set, xwyh_mut, tlbr_mut>;

TYPED_TEST_SUITE(PixCoordFixture, PixCoordTypes);
TYPED_TEST_SUITE(RectApiFixture, RectApiTypes);

TYPED_TEST(PixCoordFixture, AssignAndFetch) // NOLINT
{
  EXPECT_THAT(call::x_of(this->value), Eq(0));
  EXPECT_THAT(call::y_of(this->value), Eq(0));
  call::x_of(this->value, 4);
  EXPECT_THAT(call::x_of(this->value), Eq(4));
  EXPECT_THAT(call::y_of(this->value), Eq(0));
  call::y_of(this->value, 3);
  EXPECT_THAT(call::x_of(this->value), Eq(4));
  EXPECT_THAT(call::y_of(this->value), Eq(3));
  static_assert(extend::ns_lookup::has_set_x_of<decltype(this->value), int>);
}

TYPED_TEST(RectApiFixture, AssignAndFetchXxyy) // NOLINT
{
  static_assert(bounding_box<decltype(this->value)>);
  EXPECT_THAT(call::tl_x(this->value), Eq(0));
  EXPECT_THAT(call::tl_y(this->value), Eq(0));
  EXPECT_THAT(call::br_x(this->value), Eq(0));
  EXPECT_THAT(call::br_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(0));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::tl_x(this->value, 1);
  call::br_x(this->value, 2);
  EXPECT_THAT(call::tl_x(this->value), Eq(1));
  EXPECT_THAT(call::tl_y(this->value), Eq(0));
  EXPECT_THAT(call::br_x(this->value), Eq(2));
  EXPECT_THAT(call::br_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(1));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(2));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::tl_y(this->value, 3);
  call::br_y(this->value, 5);
  EXPECT_THAT(call::tl_x(this->value), Eq(1));
  EXPECT_THAT(call::tl_y(this->value), Eq(3));
  EXPECT_THAT(call::br_x(this->value), Eq(2));
  EXPECT_THAT(call::br_y(this->value), Eq(5));
  EXPECT_THAT(call::width(this->value), Eq(1));
  EXPECT_THAT(call::height(this->value), Eq(2));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(3));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(2));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(5));
}

TYPED_TEST(RectApiFixture, AssignAndFetchXwyh) // NOLINT
{
  static_assert(bounding_box<decltype(this->value)>);
  EXPECT_THAT(call::tl_x(this->value), Eq(0));
  EXPECT_THAT(call::tl_y(this->value), Eq(0));
  EXPECT_THAT(call::br_x(this->value), Eq(0));
  EXPECT_THAT(call::br_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(0));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::tl_x(this->value, 1);
  call::width(this->value, 2);
  EXPECT_THAT(call::tl_x(this->value), Eq(1));
  EXPECT_THAT(call::tl_y(this->value), Eq(0));
  EXPECT_THAT(call::br_x(this->value), Eq(3));
  EXPECT_THAT(call::br_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::tl_y(this->value, 3);
  call::height(this->value, 5);
  EXPECT_THAT(call::tl_x(this->value), Eq(1));
  EXPECT_THAT(call::tl_y(this->value), Eq(3));
  EXPECT_THAT(call::br_x(this->value), Eq(3));
  EXPECT_THAT(call::br_y(this->value), Eq(8));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(5));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(3));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(8));
}

TYPED_TEST(RectApiFixture, AssignAndFetchTlBr) // NOLINT
{
  static_assert(bounding_box<decltype(this->value)>);
  EXPECT_THAT(call::tl_x(this->value), Eq(0));
  EXPECT_THAT(call::tl_y(this->value), Eq(0));
  EXPECT_THAT(call::br_x(this->value), Eq(0));
  EXPECT_THAT(call::br_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(0));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::x_of(call::top_left(this->value), 1);
  call::x_of(call::bottom_right(this->value), 3);
  EXPECT_THAT(call::tl_x(this->value), Eq(1));
  EXPECT_THAT(call::tl_y(this->value), Eq(0));
  EXPECT_THAT(call::br_x(this->value), Eq(3));
  EXPECT_THAT(call::br_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::y_of(call::top_left(this->value), 3);
  call::y_of(call::bottom_right(this->value), 8);
  EXPECT_THAT(call::tl_x(this->value), Eq(1));
  EXPECT_THAT(call::tl_y(this->value), Eq(3));
  EXPECT_THAT(call::br_x(this->value), Eq(3));
  EXPECT_THAT(call::br_y(this->value), Eq(8));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(5));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(3));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(8));
}

} // namespace apitests
} // namespace cgui::tests

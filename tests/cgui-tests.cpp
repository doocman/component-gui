
#include <cgui/cgui-types.hpp>
#include <cgui/cgui.hpp>
#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/expected.hpp>
#include <cgui/stl_extend.hpp>
#include <cgui/widget_algorithm.hpp>

#include <array>
#include <optional>
#include <source_location>
#include <string_view>
#include <tuple>
#include <typeinfo>

#include <gmock/gmock.h>

#include <dooc/named_args_tuple.hpp>

namespace cgui::tests {
static_assert(std::convertible_to<not_null<int *>, int *>);
static_assert(std::convertible_to<int *, not_null<int *>>);
static_assert(!std::convertible_to<std::nullptr_t, not_null<int *>>);
static_assert(bounding_box<default_rect>);
static_assert(requires(std::tuple<int> t) {
  call::impl::do_apply_to{}(t, bp::no_op);
});

struct type_test_base {};
struct type_test_derived : type_test_base {};

static_assert(std::is_same_v<
              type_test_base &,
              decltype(bp::forward_cast<type_test_base, type_test_derived &>(
                  std::declval<type_test_derived &>()))>);
static_assert(std::is_same_v<
              type_test_base &&,
              decltype(bp::forward_cast<type_test_base, type_test_derived &&>(
                  std::declval<type_test_derived &>()))>);

using namespace ::testing;

static_assert(std::is_rvalue_reference_v<
              decltype(cgui::bp::details::expected_member<int, bool>::exp(
                  std::declval<
                      cgui::bp::details::expected_member<int, bool> &&>()))>);
static_assert(std::is_rvalue_reference_v<
              decltype(cgui::bp::details::expected_member<int, bool>::err(
                  std::declval<
                      cgui::bp::details::expected_member<int, bool> &&>()))>);

static_assert(canvas<dummy_canvas>);
static_assert(renderer<dummy_renderer>);

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
  EXPECT_THAT(sizeof(bp::empty_structs_optimiser<empty_state>), Eq(1));
  EXPECT_THAT(sizeof(bp::empty_structs_optimiser<empty_state, empty_state>),
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
  auto tested = bp::empty_structs_optimiser<int, int, empty_state, int>();
  get<0>(tested) = 1;
  get<1>(tested) = 2;
  get<3>(tested) = 3;
  EXPECT_THAT(sizeof(tested), Eq(sizeof(int) * 3));
  EXPECT_THAT(get<0>(tested), Eq(1));
  EXPECT_THAT(get<1>(tested), Eq(2));
  EXPECT_THAT(get<3>(tested), Eq(3));
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
constexpr void x_of(pix_set_type auto &&v, int val) { v.xset = val; }
constexpr void y_of(pix_set_type auto &&v, int val) { v.yset = val; }

enum class access_type {
  member_set,
  member_mut,
  static_set,
  static_mut,
  free_set,
  free_mut,
  extend_set,
  extend_mut
};

template <typename T>
inline constexpr access_type access_of = std::remove_cvref_t<T>::access;
template <typename T>
concept has_access_type = requires() { std::remove_cvref_t<T>::access; };
template <typename T>
concept free_set_access =
    has_access_type<T> && access_of<T> == access_type::free_set;
template <typename T>
concept free_mut_access =
    has_access_type<T> && access_of<T> == access_type::free_mut;

template <template <access_type> typename T>
using for_each_access = ::testing::Types< //
    T<access_type::member_set>, T<access_type::member_mut>,
    T<access_type::static_set>, T<access_type::static_mut>,
    T<access_type::free_set>, T<access_type::free_mut>,
    T<access_type::extend_set>,
    T<access_type::extend_mut> //
    >;

template <typename> inline constexpr bool is_gtest_types = false;
template <typename... Ts>
inline constexpr bool is_gtest_types<::testing::Types<Ts...>> = true;
template <typename T>
concept gtest_types = is_gtest_types<T>;

template <gtest_types...> struct concat_types;
template <typename... Ts> using concat_types_t = concat_types<Ts...>::type;
template <typename... Ts1, typename... Ts2>
struct concat_types<::testing::Types<Ts1...>, ::testing::Types<Ts2...>> {
  using type = ::testing::Types<Ts1..., Ts2...>;
};
template <gtest_types T> struct concat_types<T> {
  using type = T;
};
template <gtest_types T1, gtest_types T2, gtest_types... Ts>
struct concat_types<T1, T2, Ts...> {
  using type = concat_types_t<concat_types_t<T1, T2>, Ts...>;
};

template <access_type tA> struct xyxy_bbox {
  int x1, x2, y1, y2;
  static constexpr auto access = tA;

  constexpr auto l_x() const
    requires(tA == access_type::member_set || tA == access_type::member_mut)
  {
    return x1;
  }
  constexpr auto t_y() const
    requires(tA == access_type::member_set || tA == access_type::member_mut)
  {
    return y1;
  }
  constexpr auto r_x() const
    requires(tA == access_type::member_set || tA == access_type::member_mut)
  {
    return x2;
  }
  constexpr auto b_y() const
    requires(tA == access_type::member_set || tA == access_type::member_mut)
  {
    return y2;
  }
  constexpr auto &l_x()
    requires(tA == access_type::member_mut)
  {
    return x1;
  }
  constexpr auto &t_y()
    requires(tA == access_type::member_mut)
  {
    return y1;
  }
  constexpr auto &r_x()
    requires(tA == access_type::member_mut)
  {
    return x2;
  }
  constexpr auto &b_y()
    requires(tA == access_type::member_mut)
  {
    return y2;
  }

  constexpr xyxy_bbox &l_x(int v)
    requires(tA == access_type::member_set)
  {
    x1 = v;
    return *this;
  }
  constexpr xyxy_bbox &t_y(int v)
    requires(tA == access_type::member_set)
  {
    y1 = v;
    return *this;
  }
  constexpr xyxy_bbox &r_x(int v)
    requires(tA == access_type::member_set)
  {
    x2 = v;
    return *this;
  }
  constexpr xyxy_bbox &b_y(int v)
    requires(tA == access_type::member_set)
  {
    y2 = v;
    return *this;
  }

  static constexpr auto l_x(xyxy_bbox const &t)
    requires(tA == access_type::static_set)
  {
    return t.x1;
  }
  static constexpr auto t_y(xyxy_bbox const &t)
    requires(tA == access_type::static_set)
  {
    return t.y1;
  }
  static constexpr auto r_x(xyxy_bbox const &t)
    requires(tA == access_type::static_set)
  {
    return t.x2;
  }
  static constexpr auto b_y(xyxy_bbox const &t)
    requires(tA == access_type::static_set)
  {
    return t.y2;
  }
  static constexpr auto &&l_x(bp::cvref_type<xyxy_bbox> auto &&t, int v)
    requires(tA == access_type::static_set)
  {
    t.x1 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&t_y(bp::cvref_type<xyxy_bbox> auto &&t, int v)
    requires(tA == access_type::static_set)
  {
    t.y1 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&r_x(bp::cvref_type<xyxy_bbox> auto &&t, int v)
    requires(tA == access_type::static_set)
  {
    t.x2 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&b_y(bp::cvref_type<xyxy_bbox> auto &&t, int v)
    requires(tA == access_type::static_set)
  {
    t.y2 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&l_x(bp::cvref_type<xyxy_bbox> auto &&t)
    requires(tA == access_type::static_mut)
  {
    return std::forward<decltype(t)>(t).x1;
  }
  static constexpr auto &&t_y(bp::cvref_type<xyxy_bbox> auto &&t)
    requires(tA == access_type::static_mut)
  {
    return std::forward<decltype(t)>(t).y1;
  }
  static constexpr auto &&r_x(bp::cvref_type<xyxy_bbox> auto &&t)
    requires(tA == access_type::static_mut)
  {
    return std::forward<decltype(t)>(t).x2;
  }
  static constexpr auto &&b_y(bp::cvref_type<xyxy_bbox> auto &&t)
    requires(tA == access_type::static_mut)
  {
    return std::forward<decltype(t)>(t).y2;
  }
};
template <typename T>
concept xyxy_type = has_access_type<T> && requires(T &&t) {
  t.x1;
  t.y1;
  t.x2;
  t.y2;
};
template <typename T>
concept xyxy_free_set = xyxy_type<T> && free_set_access<T>;
template <typename T>
concept xyxy_free_mut = xyxy_type<T> && free_mut_access<T>;

constexpr auto l_x(xyxy_free_set auto const &a) { return a.x1; }
constexpr auto t_y(xyxy_free_set auto const &a) { return a.y1; }
constexpr auto r_x(xyxy_free_set auto const &a) { return a.x2; }
constexpr auto b_y(xyxy_free_set auto const &a) { return a.y2; }
constexpr auto &&l_x(xyxy_free_set auto &&a, int v) {
  a.x1 = v;
  return std::forward<decltype(a)>(a);
}
constexpr auto &&t_y(xyxy_free_set auto &&a, int v) {
  a.y1 = v;
  return std::forward<decltype(a)>(a);
}
constexpr auto &&r_x(xyxy_free_set auto &&a, int v) {
  a.x2 = v;
  return std::forward<decltype(a)>(a);
}
constexpr auto &&b_y(xyxy_free_set auto &&a, int v) {
  a.y2 = v;
  return std::forward<decltype(a)>(a);
}

constexpr auto &&l_x(xyxy_free_mut auto &&a) { return a.x1; }
constexpr auto &&r_x(xyxy_free_mut auto &&a) { return a.x2; }
constexpr auto &&t_y(xyxy_free_mut auto &&a) { return a.y1; }
constexpr auto &&b_y(xyxy_free_mut auto &&a) { return a.y2; }

template <access_type tA> struct xywh_bbox {
  int x, y, w, h;
  static constexpr auto access = tA;
  static constexpr bool is_set = access == access_type::member_set;
  static constexpr bool is_mut = access == access_type::member_mut;
  static constexpr bool stat_set = access == access_type::static_set;
  static constexpr bool stat_mut = access == access_type::static_mut;

  constexpr auto l_x() const
    requires(is_set || is_mut)
  {
    return x;
  }
  constexpr auto t_y() const
    requires(is_set || is_mut)
  {
    return y;
  }
  constexpr auto width() const
    requires(is_set || is_mut)
  {
    return w;
  }
  constexpr auto height() const
    requires(is_set || is_mut)
  {
    return h;
  }
  constexpr xywh_bbox &l_x(int v)
    requires(is_set)
  {
    x = v;
    return *this;
  }
  constexpr xywh_bbox &t_y(int v)
    requires(is_set)
  {
    y = v;
    return *this;
  }
  constexpr xywh_bbox &width(int v)
    requires(is_set)
  {
    w = v;
    return *this;
  }
  constexpr xywh_bbox &height(int v)
    requires(is_set)
  {
    h = v;
    return *this;
  }
  constexpr auto &l_x()
    requires(is_mut)
  {
    return x;
  }
  constexpr auto &t_y()
    requires(is_mut)
  {
    return y;
  }
  constexpr auto &width()
    requires(is_mut)
  {
    return w;
  }
  constexpr auto &height()
    requires(is_mut)
  {
    return h;
  }

  static constexpr auto l_x(xywh_bbox const &v)
    requires(stat_set)
  {
    return v.x;
  }
  static constexpr auto t_y(xywh_bbox const &v)
    requires(stat_set)
  {
    return v.y;
  }
  static constexpr auto width(xywh_bbox const &v)
    requires(stat_set)
  {
    return v.w;
  }
  static constexpr auto height(xywh_bbox const &v)
    requires(stat_set)
  {
    return v.h;
  }
  static constexpr auto &&l_x(bp::cvref_type<xywh_bbox> auto &&v, int val)
    requires(stat_set)
  {
    v.x = val;
    return std::forward<decltype(v)>(v);
  }
  static constexpr auto &&t_y(bp::cvref_type<xywh_bbox> auto &&v, int val)
    requires(stat_set)
  {
    v.y = val;
    return std::forward<decltype(v)>(v);
  }
  static constexpr auto &&width(bp::cvref_type<xywh_bbox> auto &&v, int val)
    requires(stat_set)
  {
    v.w = val;
    return std::forward<decltype(v)>(v);
  }
  static constexpr auto &&height(bp::cvref_type<xywh_bbox> auto &&v, int val)
    requires(stat_set)
  {
    v.h = val;
    return std::forward<decltype(v)>(v);
  }
  static constexpr auto &&l_x(bp::cvref_type<xywh_bbox> auto &&v)
    requires(stat_mut)
  {
    return std::forward<decltype(v)>(v).x;
  }
  static constexpr auto &&t_y(bp::cvref_type<xywh_bbox> auto &&v)
    requires(stat_mut)
  {
    return std::forward<decltype(v)>(v).y;
  }
  static constexpr auto &&width(bp::cvref_type<xywh_bbox> auto &&v)
    requires(stat_mut)
  {
    return std::forward<decltype(v)>(v).w;
  }
  static constexpr auto &&height(bp::cvref_type<xywh_bbox> auto &&v)
    requires(stat_mut)
  {
    return std::forward<decltype(v)>(v).h;
  }
};

template <typename T>
concept xywh_type = has_access_type<T> && requires(T &&t) {
  t.x;
  t.y;
  t.w;
  t.h;
};
template <typename T>
concept xywh_free_set = xywh_type<T> && free_set_access<T>;
template <typename T>
concept xywh_free_mut = xywh_type<T> && free_mut_access<T>;

constexpr auto l_x(xywh_free_set auto const &v) { return v.x; }
constexpr auto t_y(xywh_free_set auto const &v) { return v.y; }
constexpr auto width(xywh_free_set auto const &v) { return v.w; }
constexpr auto height(xywh_free_set auto const &v) { return v.h; }
constexpr auto &&l_x(xywh_free_set auto &&v, int val) {
  v.x = val;
  return std::forward<decltype(v)>(v);
}
constexpr auto &&t_y(xywh_free_set auto &&v, int val) {
  v.y = val;
  return std::forward<decltype(v)>(v);
}
constexpr auto &&width(xywh_free_set auto &&v, int val) {
  v.w = val;
  return std::forward<decltype(v)>(v);
}
constexpr auto &&height(xywh_free_set auto &&v, int val) {
  v.h = val;
  return std::forward<decltype(v)>(v);
}
constexpr auto &&l_x(xywh_free_mut auto &&v) {
  return std::forward<decltype(v)>(v).x;
}
constexpr auto &&t_y(xywh_free_mut auto &&v) {
  return std::forward<decltype(v)>(v).y;
}
constexpr auto &&width(xywh_free_mut auto &&v) {
  return std::forward<decltype(v)>(v).w;
}
constexpr auto &&height(xywh_free_mut auto &&v) {
  return std::forward<decltype(v)>(v).h;
}

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

struct tlbr_static_set {
  set_pix_coord tl;
  mut_pix_coord br;
  static constexpr auto &&top_left(bp::cvref_type<tlbr_static_set> auto &&t) {
    return std::forward<decltype(t)>(t).tl;
  }
  static constexpr auto &&
  bottom_right(bp::cvref_type<tlbr_static_set> auto &&t) {
    return std::forward<decltype(t)>(t).br;
  }
};
} // namespace apitests
} // namespace cgui::tests
namespace cgui {
using cgui::tests::apitests::access_of;
using cgui::tests::apitests::access_type;
template <tests::apitests::access_type at>
struct extend_api<tests::apitests::xyxy_bbox<at>> {
  using _type = tests::apitests::xyxy_bbox<at>;
  static constexpr auto access = access_of<_type>;
  static constexpr bool _is_set = access == access_type::extend_set;
  static constexpr bool _is_mut = access == access_type::extend_mut;
  static constexpr _type from_xyxy(int x1, int y1, int x2, int y2) {
    return {x1, x2, y1, y2};
  }

  static constexpr auto l_x(_type const &v)
    requires(_is_set)
  {
    return v.x1;
  }
  static constexpr auto t_y(_type const &v)
    requires(_is_set)
  {
    return v.y1;
  }
  static constexpr auto r_x(_type const &v)
    requires(_is_set)
  {
    return v.x2;
  }
  static constexpr auto b_y(_type const &v)
    requires(_is_set)
  {
    return v.y2;
  }
  static constexpr auto &&l_x(auto &&t, int v)
    requires(_is_set)
  {
    t.x1 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&r_x(auto &&t, int v)
    requires(_is_set)
  {
    t.x2 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&t_y(auto &&t, int v)
    requires(_is_set)
  {
    t.y1 = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&b_y(auto &&t, int v)
    requires(_is_set)
  {
    t.y2 = v;
    return std::forward<decltype(t)>(t);
  }

  static constexpr auto &&l_x(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).x1;
  }
  static constexpr auto &&r_x(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).x2;
  }
  static constexpr auto &&t_y(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).y1;
  }
  static constexpr auto &&b_y(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).y2;
  }
};
template <tests::apitests::access_type at>
struct extend_api<tests::apitests::xywh_bbox<at>> {
  using _type = tests::apitests::xywh_bbox<at>;
  static constexpr auto access = at;
  static constexpr bool _is_set = access == access_type::extend_set;
  static constexpr bool _is_mut = access == access_type::extend_mut;
  static constexpr _type from_xywh(int x, int y, int w, int h) {
    return {x, y, w, h};
  }

  static constexpr auto l_x(_type const &v)
    requires(_is_set)
  {
    return v.x;
  }
  static constexpr auto t_y(_type const &v)
    requires(_is_set)
  {
    return v.y;
  }
  static constexpr auto width(_type const &v)
    requires(_is_set)
  {
    return v.w;
  }
  static constexpr auto height(_type const &v)
    requires(_is_set)
  {
    return v.h;
  }
  static constexpr auto &&l_x(auto &&t, int v)
    requires(_is_set)
  {
    t.x = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&width(auto &&t, int v)
    requires(_is_set)
  {
    t.w = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&t_y(auto &&t, int v)
    requires(_is_set)
  {
    t.y = v;
    return std::forward<decltype(t)>(t);
  }
  static constexpr auto &&height(auto &&t, int v)
    requires(_is_set)
  {
    t.h = v;
    return std::forward<decltype(t)>(t);
  }

  static constexpr auto &&l_x(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).x;
  }
  static constexpr auto &&width(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).w;
  }
  static constexpr auto &&t_y(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).y;
  }
  static constexpr auto &&height(auto &&v)
    requires(_is_mut)
  {
    return std::forward<decltype(v)>(v).h;
  }
};
template <> struct extend_api<tests::apitests::tlbr_mut> {
  static constexpr tests::apitests::tlbr_mut
  from_tlbr(tests::apitests::mut_pix_coord tl,
            tests::apitests::set_pix_coord br) {
    return {tl, br};
  }
};

template <> struct extend_api<tests::apitests::tlbr_static_set> {
  static constexpr tests::apitests::tlbr_static_set
  from_tlbr(tests::apitests::set_pix_coord tl,
            tests::apitests::mut_pix_coord br) {
    return tests::apitests::tlbr_static_set{tl, br};
  }
};
} // namespace cgui

namespace cgui::tests {
namespace apitests {

template <typename T> class PixCoordFixture : public ::testing::Test {
public:
  T value{};
};

using PixCoordTypes = ::testing::Types<mut_pix_coord, set_pix_coord>;

template <typename T> class BoxApiFixture : public ::testing::Test {
public:
  T value{};
};
using RectApiTypes = concat_types_t<::testing::Types<tlbr_mut, tlbr_static_set>,
                                    apitests::for_each_access<xyxy_bbox>,
                                    apitests::for_each_access<xywh_bbox>>;
static_assert(mutable_bounding_box<tlbr_mut, int>);
static_assert(mutable_bounding_box<tlbr_static_set, int>);

static_assert(has_assignable_get<xywh_bbox<access_type::extend_mut> &,
                                 call::impl::_do_width, int>);

TYPED_TEST_SUITE(PixCoordFixture, PixCoordTypes);
TYPED_TEST_SUITE(BoxApiFixture, RectApiTypes);

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
}

TYPED_TEST(BoxApiFixture, AssignAndFetchXxyy) // NOLINT
{
  static_assert(bounding_box<decltype(this->value)>);
  EXPECT_THAT(call::l_x(this->value), Eq(0));
  EXPECT_THAT(call::t_y(this->value), Eq(0));
  EXPECT_THAT(call::r_x(this->value), Eq(0));
  EXPECT_THAT(call::b_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(0));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::l_x.call(this->value, 1);
  call::r_x(this->value, 2);
  EXPECT_THAT(call::l_x(this->value), Eq(1));
  EXPECT_THAT(call::t_y(this->value), Eq(0));
  EXPECT_THAT(call::r_x(this->value), Eq(2));
  EXPECT_THAT(call::b_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(1));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(2));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::t_y(this->value, 3);
  call::b_y(this->value, 5);
  EXPECT_THAT(call::l_x(this->value), Eq(1));
  EXPECT_THAT(call::t_y(this->value), Eq(3));
  EXPECT_THAT(call::r_x(this->value), Eq(2));
  EXPECT_THAT(call::b_y(this->value), Eq(5));
  EXPECT_THAT(call::width(this->value), Eq(1));
  EXPECT_THAT(call::height(this->value), Eq(2));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(3));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(2));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(5));
}

TYPED_TEST(BoxApiFixture, AssignAndFetchXwyh) // NOLINT
{
  static_assert(bounding_box<decltype(this->value)>);
  EXPECT_THAT(call::l_x(this->value), Eq(0));
  EXPECT_THAT(call::t_y(this->value), Eq(0));
  EXPECT_THAT(call::r_x(this->value), Eq(0));
  EXPECT_THAT(call::b_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(0));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::l_x(this->value, 1);
  call::width(this->value, 2);
  EXPECT_THAT(call::l_x(this->value), Eq(1));
  EXPECT_THAT(call::t_y(this->value), Eq(0));
  EXPECT_THAT(call::r_x(this->value), Eq(3));
  EXPECT_THAT(call::b_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::t_y(this->value, 3);
  call::height(this->value, 5);
  EXPECT_THAT(call::l_x(this->value), Eq(1));
  EXPECT_THAT(call::t_y(this->value), Eq(3));
  EXPECT_THAT(call::r_x(this->value), Eq(3));
  EXPECT_THAT(call::b_y(this->value), Eq(8));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(5));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(3));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(8));
}

TYPED_TEST(BoxApiFixture, AssignAndFetchTlBr) // NOLINT
{
  static_assert(bounding_box<decltype(this->value)>);
  EXPECT_THAT(call::l_x(this->value), Eq(0));
  EXPECT_THAT(call::t_y(this->value), Eq(0));
  EXPECT_THAT(call::r_x(this->value), Eq(0));
  EXPECT_THAT(call::b_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(0));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(0));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::x_of(call::top_left(this->value), 1);
  call::x_of(call::bottom_right(this->value), 3);
  EXPECT_THAT(call::l_x(this->value), Eq(1));
  EXPECT_THAT(call::t_y(this->value), Eq(0));
  EXPECT_THAT(call::r_x(this->value), Eq(3));
  EXPECT_THAT(call::b_y(this->value), Eq(0));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(0));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(0));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(0));
  call::y_of(call::top_left(this->value), 3);
  call::y_of(call::bottom_right(this->value), 8);
  EXPECT_THAT(call::l_x(this->value), Eq(1));
  EXPECT_THAT(call::t_y(this->value), Eq(3));
  EXPECT_THAT(call::r_x(this->value), Eq(3));
  EXPECT_THAT(call::b_y(this->value), Eq(8));
  EXPECT_THAT(call::width(this->value), Eq(2));
  EXPECT_THAT(call::height(this->value), Eq(5));
  EXPECT_THAT(call::x_of(call::top_left(this->value)), Eq(1));
  EXPECT_THAT(call::y_of(call::top_left(this->value)), Eq(3));
  EXPECT_THAT(call::x_of(call::bottom_right(this->value)), Eq(3));
  EXPECT_THAT(call::y_of(call::bottom_right(this->value)), Eq(8));
}

TYPED_TEST(BoxApiFixture, ConstructXYXY) // NOLINT
{
  using box_t = std::remove_cvref_t<decltype(this->value)>;
  auto v = box_from_xyxy<box_t>(1, 2, 3, 4);
  EXPECT_TRUE((std::is_same_v<box_t, decltype(v)>));
  EXPECT_THAT(call::l_x(v), Eq(1));
  EXPECT_THAT(call::t_y(v), Eq(2));
  EXPECT_THAT(call::r_x(v), Eq(3));
  EXPECT_THAT(call::b_y(v), Eq(4));
}

TYPED_TEST(BoxApiFixture, ConstructXYWH) // NOLINT
{
  using box_t = std::remove_cvref_t<decltype(this->value)>;
  auto v = box_from_xywh<box_t>(1, 2, 3, 4);
  EXPECT_TRUE((std::is_same_v<box_t, decltype(v)>));
  EXPECT_THAT(call::l_x(v), Eq(1));
  EXPECT_THAT(call::t_y(v), Eq(2));
  EXPECT_THAT(call::width(v), Eq(3));
  EXPECT_THAT(call::height(v), Eq(4));
}

TYPED_TEST(BoxApiFixture, ConstructTLBR) // NOLINT
{
  using box_t = std::remove_cvref_t<decltype(this->value)>;
  auto v = box_from_tlbr<box_t>(default_pixel_coord{1, 2},
                                default_pixel_coord{3, 4});
  EXPECT_TRUE((std::is_same_v<box_t, decltype(v)>));
  EXPECT_THAT(call::l_x(v), Eq(1));
  EXPECT_THAT(call::t_y(v), Eq(2));
  EXPECT_THAT(call::r_x(v), Eq(3));
  EXPECT_THAT(call::b_y(v), Eq(4));
}

TYPED_TEST(BoxApiFixture, SplitBoxX) // NOLINT
{
  auto &box = this->value;
  auto org_ty = 2;
  auto org_by = 55;
  call::t_y(box, org_ty);
  call::b_y(box, org_by);
  call::l_x(box, 1);
  call::r_x(box, 5);
  auto b2 = split_x(&box, 3);
  EXPECT_THAT(call::l_x(box), Eq(1));
  EXPECT_THAT(call::r_x(box), Eq(3));
  EXPECT_THAT(call::t_y(box), Eq(org_ty));
  EXPECT_THAT(call::b_y(box), Eq(org_by));
  EXPECT_THAT(call::l_x(b2), Eq(3));
  EXPECT_THAT(call::r_x(b2), Eq(5));
  EXPECT_THAT(call::t_y(b2), Eq(org_ty));
  EXPECT_THAT(call::b_y(b2), Eq(org_by));
}

TYPED_TEST(BoxApiFixture, SplitBoxY) // NOLINT
{
  auto &box = this->value;
  auto org_lx = 2;
  auto org_rx = 55;
  call::l_x(box, org_lx);
  call::r_x(box, org_rx);
  call::t_y(box, 1);
  call::b_y(box, 5);
  auto b2 = split_y(&box, 3);
  EXPECT_THAT(call::t_y(box), Eq(1));
  EXPECT_THAT(call::b_y(box), Eq(3));
  EXPECT_THAT(call::l_x(box), Eq(org_lx));
  EXPECT_THAT(call::r_x(box), Eq(org_rx));
  EXPECT_THAT(call::t_y(b2), Eq(3));
  EXPECT_THAT(call::b_y(b2), Eq(5));
  EXPECT_THAT(call::l_x(b2), Eq(org_lx));
  EXPECT_THAT(call::r_x(b2), Eq(org_rx));
}

TYPED_TEST(BoxApiFixture, TrimLeft) // NOLINT
{
  auto &box = this->value;
  call::t_y(box, 4);
  call::b_y(box, 70);
  call::l_x(box, 4);
  call::r_x(box, 15);
  auto b2 = trim_from_left(&box, 4);
  EXPECT_THAT(call::t_y(box), Eq(4));
  EXPECT_THAT(call::b_y(box), Eq(70));
  EXPECT_THAT(call::l_x(box), Eq(4 + 4));
  EXPECT_THAT(call::r_x(box), Eq(15));
  EXPECT_THAT(call::t_y(b2), Eq(4));
  EXPECT_THAT(call::b_y(b2), Eq(70));
  EXPECT_THAT(call::l_x(b2), Eq(4));
  EXPECT_THAT(call::r_x(b2), Eq(4 + 4));
}
TYPED_TEST(BoxApiFixture, TrimUp) // NOLINT
{
  auto &box = this->value;
  call::l_x(box, 4);
  call::r_x(box, 70);
  call::t_y(box, 4);
  call::b_y(box, 15);
  auto b2 = trim_from_above(&box, 4);
  EXPECT_THAT(call::l_x(box), Eq(4));
  EXPECT_THAT(call::r_x(box), Eq(70));
  EXPECT_THAT(call::t_y(box), Eq(4 + 4));
  EXPECT_THAT(call::b_y(box), Eq(15));
  EXPECT_THAT(call::l_x(b2), Eq(4));
  EXPECT_THAT(call::r_x(b2), Eq(70));
  EXPECT_THAT(call::t_y(b2), Eq(4));
  EXPECT_THAT(call::b_y(b2), Eq(4 + 4));
}

TYPED_TEST(BoxApiFixture, TrimRight) // NOLINT
{
  auto &box = this->value;
  call::t_y(box, 4);
  call::b_y(box, 70);
  call::l_x(box, 4);
  call::r_x(box, 15);
  auto b2 = trim_from_right(&box, 4);
  EXPECT_THAT(call::t_y(box), Eq(4));
  EXPECT_THAT(call::b_y(box), Eq(70));
  EXPECT_THAT(call::l_x(box), Eq(4));
  EXPECT_THAT(call::r_x(box), Eq(15 - 4));
  EXPECT_THAT(call::t_y(b2), Eq(4));
  EXPECT_THAT(call::b_y(b2), Eq(70));
  EXPECT_THAT(call::l_x(b2), Eq(15 - 4));
  EXPECT_THAT(call::r_x(b2), Eq(15));
}
TYPED_TEST(BoxApiFixture, TrimDown) // NOLINT
{
  auto &box = this->value;
  call::l_x(box, 4);
  call::r_x(box, 70);
  call::t_y(box, 4);
  call::b_y(box, 15);
  auto b2 = trim_from_below(&box, 4);
  EXPECT_THAT(call::l_x(box), Eq(4));
  EXPECT_THAT(call::r_x(box), Eq(70));
  EXPECT_THAT(call::t_y(box), Eq(4));
  EXPECT_THAT(call::b_y(box), Eq(15 - 4));
  EXPECT_THAT(call::l_x(b2), Eq(4));
  EXPECT_THAT(call::r_x(b2), Eq(70));
  EXPECT_THAT(call::t_y(b2), Eq(15 - 4));
  EXPECT_THAT(call::b_y(b2), Eq(15));
}
TYPED_TEST(BoxApiFixture, BoxUnion) // NOLINT
{
  using box_t = decltype(this->value);
  auto result = box_union(box_from_xyxy<box_t>(1, 2, 3, 4),
                          box_from_xyxy<box_t>(1, 2, 3, 4));
  EXPECT_THAT(call::l_x(result), Eq(1));
  EXPECT_THAT(call::t_y(result), Eq(2));
  EXPECT_THAT(call::r_x(result), Eq(3));
  EXPECT_THAT(call::b_y(result), Eq(4));

  result = box_union(box_from_xyxy<box_t>(1, 2, 3, 4),
                     box_from_xyxy<box_t>(2, 3, 4, 5));
  EXPECT_THAT(call::l_x(result), Eq(1));
  EXPECT_THAT(call::t_y(result), Eq(2));
  EXPECT_THAT(call::r_x(result), Eq(4));
  EXPECT_THAT(call::b_y(result), Eq(5));
}
TYPED_TEST(BoxApiFixture, BoxIntersection) // NOLINT
{
  using box_t = decltype(this->value);
  auto result = box_intersection(box_from_xyxy<box_t>(1, 2, 3, 4),
                                 box_from_xyxy<box_t>(1, 2, 3, 4));
  EXPECT_THAT(call::l_x(result), Eq(1));
  EXPECT_THAT(call::t_y(result), Eq(2));
  EXPECT_THAT(call::r_x(result), Eq(3));
  EXPECT_THAT(call::b_y(result), Eq(4));

  result = box_intersection(box_from_xyxy<box_t>(1, 2, 3, 4),
                            box_from_xyxy<box_t>(2, 3, 4, 5));
  EXPECT_THAT(call::l_x(result), Eq(2));
  EXPECT_THAT(call::t_y(result), Eq(3));
  EXPECT_THAT(call::r_x(result), Eq(3));
  EXPECT_THAT(call::b_y(result), Eq(4));

  result = box_intersection(box_from_xyxy<box_t>(1, 1, 2, 2),
                            box_from_xyxy<box_t>(3, 3, 4, 4));
  EXPECT_TRUE(empty_box(result));
}
TYPED_TEST(BoxApiFixture, HitTest) // NOLINT
{
  using box_t = decltype(this->value);
  auto b = box_from_xyxy<box_t>(1, 2, 3, 4);
  EXPECT_TRUE(hit_box(b, {1, 2}));
  EXPECT_TRUE(hit_box(b, {2, 3}));
  EXPECT_FALSE(hit_box(b, {0, 3}));
  EXPECT_FALSE(hit_box(b, {2, 1}));
  EXPECT_FALSE(hit_box(b, {3, 4}));
}
TYPED_TEST(BoxApiFixture, BoxIncludesBox) // NOLINT
{
  using box_t = decltype(this->value);
  auto b = box_from_xyxy<box_t>(1, 2, 4, 5);
  EXPECT_TRUE(box_includes_box(b, box_from_xyxy<box_t>(1, 2, 2, 3)));
  EXPECT_FALSE(box_includes_box(b, box_from_xyxy<box_t>(0, 2, 2, 3)));
  EXPECT_TRUE(box_includes_box(b, box_from_xyxy<default_rect>(1, 2, 2, 3)));
  EXPECT_TRUE(box_includes_box(b, b));
}
TYPED_TEST(BoxApiFixture, NudgeLeft) // NOLINT
{
  using box_t = decltype(this->value);
  auto b = box_from_xyxy<box_t>(1, 2, 4, 5);
  auto b2 = nudge_left(b, 1);
  EXPECT_THAT(call::l_x(b2), Eq(0));
  EXPECT_THAT(call::t_y(b2), Eq(2));
  EXPECT_THAT(call::r_x(b2), Eq(3));
  EXPECT_THAT(call::b_y(b2), Eq(5));
}
TYPED_TEST(BoxApiFixture, NudgeDown) // NOLINT
{
  using box_t = decltype(this->value);
  auto b = box_from_xyxy<box_t>(1, 2, 4, 5);
  auto b2 = nudge_down(b, 1);
  EXPECT_THAT(call::l_x(b2), Eq(1));
  EXPECT_THAT(call::t_y(b2), Eq(3));
  EXPECT_THAT(call::r_x(b2), Eq(4));
  EXPECT_THAT(call::b_y(b2), Eq(6));
}
TYPED_TEST(BoxApiFixture, MoveTlTo) // NOLINT
{
  using box_t = decltype(this->value);
  auto b = box_from_xywh<box_t>(1, 2, 4, 5);
  auto b2 = move_tl_to(b, {0, -1});
  EXPECT_THAT(call::l_x(b2), Eq(0));
  EXPECT_THAT(call::t_y(b2), Eq(-1));
  EXPECT_THAT(call::width(b2), Eq(4));
  EXPECT_THAT(call::height(b2), Eq(5));
}

} // namespace apitests

TEST(SubFind, FindInTuple) // NOLINT
{
  auto v = std::tuple(1, 2, 3, 3);
  int const *res = nullptr;
  int calls{};
  std::size_t last_index = -1;
  auto cb = [&res, &calls, &last_index](int const &i, std::size_t index) {
    res = &i;
    last_index = index;
    ++calls;
  };
  auto found = find_first_cb(equals(2), cb, v);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&std::get<1>(v)));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(1));

  auto reset = [&] {
    res = nullptr;
    calls = 0;
    last_index = -1;
  };
  reset();
  found = find_first_cb(equals(3), cb, v);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&std::get<2>(v)));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(2));

  reset();
  found = find_first_cb(equals(4), cb, v);
  EXPECT_THAT(found, IsFalse());
  EXPECT_THAT(res, IsNull());
  EXPECT_THAT(calls, Eq(0));
  EXPECT_THAT(last_index, Eq(std::size_t{highest_possible}));
}

struct dummy_for_eachable {
  int i1, i2, i3, i4;

  constexpr void for_each(auto &&cb) {
    cb(i1, 0);
    cb(i2, 1);
    cb(i3, 2);
    cb(i4, 3);
  }
};

TEST(SubFind, FindInForEach) // NOLINT
{
  auto v = dummy_for_eachable{1, 2, 3, 3};
  int const *res = nullptr;
  int calls{};
  std::size_t last_index = -1;
  auto cb = [&res, &calls, &last_index](int const &i, std::size_t index) {
    res = &i;
    last_index = index;
    ++calls;
  };
  auto found = find_first_cb(equals(2), cb, v);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&v.i2));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(1));

  auto reset = [&] {
    res = nullptr;
    calls = 0;
    last_index = highest_possible;
  };
  reset();
  found = find_first_cb(equals(3), cb, v);
  EXPECT_THAT(found, IsTrue());
  EXPECT_THAT(res, Eq(&v.i3));
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_index, Eq(2));

  reset();
  found = find_first_cb(equals(4), cb, v);
  EXPECT_THAT(found, IsFalse());
  EXPECT_THAT(res, IsNull());
  EXPECT_THAT(calls, Eq(0));
  EXPECT_THAT(last_index, Eq(std::size_t{highest_possible}));
}

TEST(UiEventsMatcher, Basics) // NOLINT
{
  auto f = MockFunction<void(std::string_view)>();
  InSequence s;
  EXPECT_CALL(f, Call(StrEq("Move")));
  EXPECT_CALL(f, Call(StrEq("Down")));
  EXPECT_CALL(f, Call(StrEq("Up")));

  using enum ui_events;
  auto my_switch = [&f](auto &&evt) {
    ui_event_switch(evt,
                    event_case<mouse_button_down>([&f] { f.Call("Down"); }),
                    event_case<mouse_button_up>([&f] { f.Call("Up"); }),
                    event_case<mouse_move>([&f] { f.Call("Move"); }));
  };
  my_switch(dummy_mouse_move_event{});
  my_switch(dummy_mouse_down_event{});
  my_switch(dummy_mouse_up_event{});
}

TEST(UiEventsMatcher, BasicsState) // NOLINT
{
  auto f = MockFunction<void(std::string_view)>();
  InSequence s;
  EXPECT_CALL(f, Call(StrEq("Move")));
  EXPECT_CALL(f, Call(StrEq("Down")));
  EXPECT_CALL(f, Call(StrEq("Up")));

  using enum ui_events;
  auto my_switch = [&f](auto &&evt) {
    ui_event_switch(
        evt, f, event_case<mouse_button_down>([](auto &&, auto &f2) {
          f2.Call("Down");
        }),
        event_case<mouse_button_up>([](auto &&, auto &f2) { f2.Call("Up"); }),
        event_case<mouse_move>([&f] { f.Call("Move"); }));
  };
  my_switch(dummy_mouse_move_event{});
  my_switch(dummy_mouse_down_event{});
  my_switch(dummy_mouse_up_event{});
}

struct test_renderer {
  struct individual_colours_t {
    std::vector<std::uint_least8_t> red, green, blue, alpha;
    explicit individual_colours_t(std::vector<default_colour_t> const &pix) {
      red.reserve(size(pix));
      green.reserve(size(pix));
      blue.reserve(size(pix));
      alpha.reserve(size(pix));
      for (auto const &c : pix) {
        red.push_back(c.red);
        green.push_back(c.green);
        blue.push_back(c.blue);
        alpha.push_back(c.alpha);
      }
    }
  };

  default_rect a_;
  std::vector<default_colour_t> drawn_pixels;
  std::vector<default_rect> failed_calls;
  std::vector<default_pixel_coord> failed_pixel_draws;

  explicit test_renderer(default_rect a)
      : a_(a), drawn_pixels(call::width(a) * call::height(a)) {}

  auto &at(int x, int y) {
    auto index = x + y * call::width(area());
    return drawn_pixels.at(index);
  }

  void draw_pixels(bounding_box auto &&b, auto &&cb) {
    if (!box_includes_box(area(), b)) {
      failed_calls.push_back(box_from_xyxy<default_rect>(
          call::l_x(b), call::t_y(b), call::r_x(b), call::b_y(b)));
      return;
    }
    cb([this, &b](auto &&pos, auto &&col) {
      if (!hit_box(b, pos)) {
        failed_pixel_draws.emplace_back(call::x_of(pos), call::y_of(pos));
        return;
      }
      at(call::x_of(pos), call::y_of(pos)) = {
          call::red(col), call::green(col), call::blue(col), call::alpha(col)};
    });
  }

  default_rect area() const { return a_; }

  individual_colours_t individual_colours() const {
    return individual_colours_t(drawn_pixels);
  }
};

inline void expect_box_equal(
    bounding_box auto const &to_test, bounding_box auto const &to_expect,
    std::source_location const &sl = std::source_location::current()) {
  EXPECT_THAT(call::l_x(to_test), Eq(call::l_x(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
  EXPECT_THAT(call::t_y(to_test), Eq(call::t_y(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
  EXPECT_THAT(call::r_x(to_test), Eq(call::r_x(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
  EXPECT_THAT(call::b_y(to_test), Eq(call::b_y(to_expect)))
      << "At " << sl.file_name() << ':' << sl.line();
}

TEST(RecursiveAreaNavigator, NavigateSimple) // NOLINT
{
  auto nav = recursive_area_navigator({{0, 0}, {5, 5}});
  expect_box_equal(nav.relative_area(), default_rect{{0, 0}, {5, 5}});
  expect_box_equal(nav.absolute_area(), default_rect{{0, 0}, {5, 5}});
  auto sub = nav.sub({{0, 0}, {4, 4}});
  expect_box_equal(sub.relative_area(), default_rect{{0, 0}, {4, 4}});
  expect_box_equal(sub.absolute_area(), default_rect{{0, 0}, {4, 4}});
  sub = nav.sub({{1, 1}, {2, 2}});
  expect_box_equal(sub.relative_area(), default_rect{{0, 0}, {1, 1}});
  expect_box_equal(sub.absolute_area(), default_rect{{1, 1}, {2, 2}});
  sub = nav.sub({{0, 0}, {6, 6}});
  expect_box_equal(sub.relative_area(), default_rect{{0, 0}, {5, 5}});
  expect_box_equal(sub.absolute_area(), default_rect{{0, 0}, {5, 5}});

  nav = recursive_area_navigator({{1, 1}, {5, 5}});
  sub = nav.sub({{0, 2}, {4, 4}});
  expect_box_equal(sub.relative_area(), default_rect{{1, 0}, {4, 2}});
  expect_box_equal(sub.absolute_area(), default_rect{{1, 2}, {4, 4}});
  auto sub2 = sub.sub({{2, 0}, {4, 3}});
  expect_box_equal(sub2.relative_area(), default_rect{{0, 0}, {2, 2}});
  expect_box_equal(sub2.absolute_area(), default_rect{{2, 2}, {4, 4}});
}

TEST(RecursiveAreaNavigator, Nudger) // NOLINT
{
  auto nav = recursive_area_navigator({{0, 0}, {5, 5}});
  auto nudger = nav.relative_to_absolute_nudger();
  auto xy = nudger(default_pixel_coord{0, 0});
  auto &[x, y] = xy;
  EXPECT_THAT(x, Eq(0));
  EXPECT_THAT(y, Eq(0));
  auto sub = nav.sub({{
                          1,
                          1,
                      },
                      {5, 5}});
  nudger = sub.relative_to_absolute_nudger();
  xy = nudger(default_pixel_coord{0, 0});
  EXPECT_THAT(x, Eq(1));
  EXPECT_THAT(y, Eq(1));
}

TEST(SubRenderer, DrawPixels) // NOLINT
{
  auto r = test_renderer({{0, 0}, {6, 7}});
  auto sr1 = sub_renderer(r, r.area());
  sr1.draw_pixels(default_rect{{0, 0}, {6, 7}},
                  [&](bounding_box auto &&b, auto &&drawer) {
                    EXPECT_THAT(call::l_x(b), Eq(0));
                    EXPECT_THAT(call::t_y(b), Eq(0));
                    EXPECT_THAT(call::width(b), Eq(call::width(r.area())));
                    EXPECT_THAT(call::height(b), Eq(call::height(r.area())));
                    drawer(default_pixel_coord{}, default_colour_t{1, 1, 1, 1});
                  });
  EXPECT_THAT(r.failed_calls, ElementsAre());
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  ASSERT_THAT(ssize(r.drawn_pixels), Eq(6 * 7));
  EXPECT_THAT(r.at(0, 0).red, Eq(1));
  EXPECT_THAT(r.at(0, 0).green, Eq(1));
  EXPECT_THAT(r.at(0, 0).blue, Eq(1));
  EXPECT_THAT(r.at(0, 0).alpha, Eq(1));
  EXPECT_THAT(r.at(1, 0).red, Eq(0));
  EXPECT_THAT(r.at(1, 0).green, Eq(0));
  EXPECT_THAT(r.at(1, 0).blue, Eq(0));
  EXPECT_THAT(r.at(1, 0).alpha, Eq(0));
  EXPECT_THAT(r.at(0, 1).red, Eq(0));
  EXPECT_THAT(r.at(0, 1).green, Eq(0));
  EXPECT_THAT(r.at(0, 1).blue, Eq(0));
  EXPECT_THAT(r.at(0, 1).alpha, Eq(0));
  auto s2_x = 1;
  auto s2_y = 1;
  auto s2 = sr1.sub(default_rect{{s2_x, s2_y}, {4, 4}});
  s2.draw_pixels(default_rect{{0, 0}, {4, 4}},
                 [&](bounding_box auto &&b, auto &&drawer) {
                   EXPECT_THAT(call::l_x(b), Eq(0));
                   EXPECT_THAT(call::t_y(b), Eq(0));
                   EXPECT_THAT(call::width(b), Eq(3));
                   EXPECT_THAT(call::height(b), Eq(3));
                   drawer(default_pixel_coord{}, default_colour_t{2, 1, 1, 1});
                 });
  EXPECT_THAT(r.at(0, 0).red, Eq(1));
  EXPECT_THAT(r.at(0, 0).green, Eq(1));
  EXPECT_THAT(r.at(0, 0).blue, Eq(1));
  EXPECT_THAT(r.at(0, 0).alpha, Eq(1));
  EXPECT_THAT(r.at(1, 0).red, Eq(0));
  EXPECT_THAT(r.at(1, 0).green, Eq(0));
  EXPECT_THAT(r.at(1, 0).blue, Eq(0));
  EXPECT_THAT(r.at(1, 0).alpha, Eq(0));
  EXPECT_THAT(r.at(0, 1).red, Eq(0));
  EXPECT_THAT(r.at(0, 1).green, Eq(0));
  EXPECT_THAT(r.at(0, 1).blue, Eq(0));
  EXPECT_THAT(r.at(0, 1).alpha, Eq(0));
  EXPECT_THAT(r.at(1, 1).red, Eq(2));
  EXPECT_THAT(r.at(1, 1).green, Eq(1));
  EXPECT_THAT(r.at(1, 1).blue, Eq(1));
  EXPECT_THAT(r.at(1, 1).alpha, Eq(1));
}

TEST(SubRenderer, PartialDrawPixels) // NOLINT
{
  auto r = test_renderer({{0, 0}, {4, 4}});
  auto s1 = sub_renderer(r, default_rect{{1, 2}, {3, 3}});
  s1.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(1));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(3));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{1, 1, 1, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  using namespace std::views;
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 1, 0, 0, //
                          0, 0, 0, 0  //
                          ));
  EXPECT_THAT(ic.green, ElementsAre(    //
                            0, 0, 0, 0, //
                            0, 0, 0, 0, //
                            0, 1, 0, 0, //
                            0, 0, 0, 0  //
                            ));
  EXPECT_THAT(ic.blue, ElementsAre(    //
                           0, 0, 0, 0, //
                           0, 0, 0, 0, //
                           0, 1, 0, 0, //
                           0, 0, 0, 0  //
                           ));
  EXPECT_THAT(ic.alpha, ElementsAre(      //
                            0, 0, 0, 0,   //
                            0, 0, 0, 0,   //
                            0, 255, 0, 0, //
                            0, 0, 0, 0    //
                            ));

  auto s2full = s1.sub(r.area());
  s2full.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(1));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(3));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{2, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 2, 0, 0, //
                          0, 0, 0, 0  //
                          ));

  auto s2xid = s2full.sub(default_rect{{1, 0}, {3, 4}});
  s2xid.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(0));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(2));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{3, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 3, 0, 0, //
                          0, 0, 0, 0  //
                          ));

  auto s3x = s2xid.sub(default_rect{{1, 0}, {3, 4}});
  s3x.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(0));
    EXPECT_THAT(call::t_y(b), Eq(2));
    EXPECT_THAT(call::r_x(b), Eq(1));
    EXPECT_THAT(call::b_y(b), Eq(3));
    drawer(call::top_left(b), default_colour_t{4, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 3, 4, 0, //
                          0, 0, 0, 0  //
                          ));

  auto s4 = s2full.sub(default_rect{{2, 2}, {4, 4}});
  s4.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::l_x(b), Eq(0));
    EXPECT_THAT(call::t_y(b), Eq(0));
    EXPECT_THAT(call::r_x(b), Eq(1));
    EXPECT_THAT(call::b_y(b), Eq(1));
    drawer(call::top_left(b), default_colour_t{5, 0, 0, 255});
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
  ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(    //
                          0, 0, 0, 0, //
                          0, 0, 0, 0, //
                          0, 3, 5, 0, //
                          0, 0, 0, 0  //
                          ));
}

TEST(SubRender, DrawPixelOutsideCanvas) // NOLINT
{
  auto r = test_renderer({{0, 0}, {4, 4}});
  auto s_main = sub_renderer(r, r.area());
  auto s1 = s_main.sub(default_rect{{4, 5}, {5, 6}});
  s1.draw_pixels(r.area(), [&r](bounding_box auto &&b, auto &&drawer) {
    EXPECT_THAT(call::width(b), Eq(0));
    EXPECT_THAT(call::height(b), Eq(0));
  });
  EXPECT_THAT(r.failed_pixel_draws, ElementsAre());
  EXPECT_THAT(r.failed_calls, ElementsAre());
}

struct dummy_glyph {
  int length;
  int ascend;
  int descend;
  std::uint_least8_t alpha;

  dummy_glyph(int l, auto a, int asc, int des)
      : length(l), alpha(a), ascend(asc), descend(des) {}

  constexpr int height() const { return ascend + descend + 1; }
  void render(auto &&renderer) const {
    call::draw_alpha(
        renderer, default_rect{0, 0, length, height()},
        [alpha = alpha](bounding_box auto &&bbox, auto &&drawer) {
          for (auto i = call::l_x(bbox); i < call::r_x(bbox); ++i) {
            for (auto j = call::t_y(bbox); j < call::b_y(bbox); ++j) {
              drawer(default_pixel_coord{i, j}, alpha);
            }
          }
        });
  }

  constexpr std::uint_least8_t advance_x() const { return length; }
  static constexpr std::uint_least8_t advance_y() { return {}; }
  constexpr default_rect pixel_area() const {
    return {{0, 0}, {length, height()}};
  }
  constexpr auto base_to_top() const { return ascend + 1; }
};
struct dummy_font_face {
  int faulty_glyphs{};
  bool use_height_{};

  constexpr dummy_font_face() = default;
  constexpr explicit dummy_font_face(bool use_height)
      : use_height_(use_height) {}

  constexpr int _gheight(int h) const {
    if (use_height_) {
      return h;
    }
    return 0;
  }

  constexpr int ascender() const { return _gheight(1) + 1; }
  constexpr int descender() const { return _gheight(1); }

  expected<dummy_glyph, bool> glyph(char c) {
    switch (c) {
    case '0':
      return dummy_glyph{1, 255, _gheight(0), _gheight(1)};
    case '1':
      return dummy_glyph{2, 127, _gheight(1), _gheight(0)};
    case '2':
      return dummy_glyph{3, 3, _gheight(1), _gheight(1)};
    case '-':
      return dummy_glyph{2, 63, _gheight(1), _gheight(0)};
    case ' ':
      return dummy_glyph{1, 0, _gheight(0), _gheight(0)};
    default:
      ++faulty_glyphs;
      return unexpected(false);
    }
  }
  constexpr int full_height() const { return ascender() + descender(); }
};

constexpr int bitmap_top(dummy_font_face const &font, dummy_glyph const &g) {
  return font.ascender() - g.ascend - 1;
}

TEST(TextRender, PerfectWidthString) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});
  call::set_displayed(t2r, 4, 1, "1 0");
  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr, widget_render_args(4, 1));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(255, 255, 255, 255, //
                                  0, 0, 0, 0));
  EXPECT_THAT(ic.alpha, ElementsAre(127, 127, 0, 255, //
                                    0, 0, 0, 0));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}

TEST(TextRender, CenterAligned) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 6, 3});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "1 0");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(0, 0, 0, 0, 0, 0,         //
                                  0, 255, 255, 255, 255, 0, //
                                  0, 0, 0, 0, 0, 0          //

                                  ));
  EXPECT_THAT(ic.alpha, ElementsAre(0, 0, 0, 0, 0, 0,       //
                                    0, 127, 127, 0, 255, 0, //
                                    0, 0, 0, 0, 0, 0        //
                                    ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}

TEST(TextRender, TwoLinesSpace) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "1 1");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(                            //
                          0, 255, 255, AnyOf(Eq(0), Eq(255)), //
                          0, 255, 255, 0                      //
                          ));
  EXPECT_THAT(ic.alpha, ElementsAre(        //
                            0, 127, 127, 0, //
                            0, 127, 127, 0  //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, TwoLinesDashDirect) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "120");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, Each(Eq(255)));
  EXPECT_THAT(ic.alpha, ElementsAre(          //
                            127, 127, 63, 63, //
                            3, 3, 3, 255      //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, TwoLinesDashIndirect) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "1001");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, Each(Eq(255)));
  EXPECT_THAT(ic.alpha, ElementsAre(           //
                            127, 127, 63, 63,  //
                            255, 255, 127, 127 //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, ManualNewLine) // NOLINT
{

  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 2});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "1\n1");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.alpha, ElementsAre(        //
                            0, 127, 127, 0, //
                            0, 127, 127, 0  //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}
TEST(TextRender, ThreeLines) // NOLINT
{

  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face{});

  auto r = test_renderer({0, 0, 4, 3});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "10011");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.alpha, ElementsAre(           //
                            127, 127, 63, 63,  //
                            255, 255, 63, 63,  //
                            127, 127, 127, 127 //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
}
TEST(TextRender,
     CenterWithAscendAndDescend) // NOLINT
{
  using t2r_t = text_renderer<dummy_font_face>;
  auto t2r = t2r_t(dummy_font_face(true));

  auto r = test_renderer({0, 0, 6, 5});
  auto sr = sub_renderer(r);
  call::set_displayed(t2r, call::width(r.area()), call::height(r.area()),
                      "012");
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(
      t2r, sr,
      widget_render_args(call::width(r.area()), call::height(r.area())));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(t2r.font().faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.alpha, ElementsAre(                //
                            0, 0, 0, 0, 0, 0,       //
                            0, 127, 127, 3, 3, 3,   //
                            255, 127, 127, 3, 3, 3, //
                            255, 0, 0, 3, 3, 3,     //
                            0, 0, 0, 0, 0, 0        //
                            ));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
}
TEST(TextRender, RefFace) // NOLINT
{
  auto face = dummy_font_face();
  auto t2r = text_renderer(std::ref(face));
  t2r.set_displayed(1, 1, "?");
  dummy_renderer r;
  t2r.render(r, widget_render_args(1, 1));
  EXPECT_THAT(face.faulty_glyphs, Eq(1));
}
struct mock_face {
  MOCK_METHOD((expected<dummy_glyph, bool>), glyph, (char), (const));
  inline int full_height() const { return 1; }
  inline int ascender() const { return 1; }
  inline int descender() const { return 1; }
};
TEST(TextRender, CachedGlyphs) // NOLINT
{
  auto mface = mock_face();
  EXPECT_CALL(mface, glyph(Eq('0'))).WillOnce([](auto &&...) {
    return dummy_font_face().glyph('0');
  });
  EXPECT_CALL(mface, glyph(Not(Eq('0'))))
      .Times(AnyNumber())
      .WillRepeatedly([](auto &&...) { return dummy_font_face().glyph('0'); });
  auto face = cached_font(std::ref(mface));
  auto t2r = text_renderer(std::ref(face));
  t2r.set_displayed(5, 5, "00");
  dummy_renderer r;
  t2r.render(r, widget_render_args(1, 1));
}
TEST(TextRender, CachedGlyphs4) // NOLINT
{
  using font_t = cached_font<dummy_font_face &>;
  auto dummy_face = dummy_font_face();
  auto t2r = text_renderer(font_t(dummy_face));
  call::set_displayed(t2r, 7, 1, "1 02");
  auto r = test_renderer({0, 0, 7, 1});
  auto sr = sub_renderer(r);
  call::text_colour(t2r, default_colour_t{255, 0, 0, 255});
  call::render(t2r, sr, widget_render_args(7, 1));
  EXPECT_THAT(r.failed_calls, IsEmpty());
  EXPECT_THAT(r.failed_pixel_draws, IsEmpty());
  EXPECT_THAT(dummy_face.faulty_glyphs, Eq(0));
  auto ic = r.individual_colours();
  EXPECT_THAT(ic.red, ElementsAre(255, 255, 255, 255, 255, 255, 255));
  EXPECT_THAT(ic.alpha, ElementsAre(127, 127, 0, 255, 3, 3, 3));
  EXPECT_THAT(ic.blue, Each(Eq(0)));
  EXPECT_THAT(ic.green, Each(Eq(0)));
}

struct mock_button_callback {
  MOCK_METHOD(void, do_on_button_hover, ());
  MOCK_METHOD(void, do_on_button_hold, ());
  MOCK_METHOD(void, do_on_button_click, (mouse_buttons b));
  MOCK_METHOD(void, do_on_button_exit, ());

  void handle(button_state_events::hover) { do_on_button_hover(); }
  void handle(button_state_events::hold) { do_on_button_hold(); }
  void handle(button_state_events::click const &e) {
    do_on_button_click(e.button);
  }
  void handle(button_state_events::exit) { do_on_button_exit(); }
  void operator()(auto const &e) { handle(e); }
  no_state_t state() const { return {}; }
};
static_assert(button_state<mock_button_callback>);

struct dummy_widget {
  default_rect a;

  constexpr default_rect const &area() const { return a; }
};

TEST(ButtonlikeEventTrigger, MouseHoverAndClick) // NOLINT
{
  auto button_state = mock_button_callback();
  auto trig = buttonlike_trigger(std::ref(button_state));
  auto checkpoint = MockFunction<void()>();
  InSequence s;
  EXPECT_CALL(button_state, do_on_button_hover());
  EXPECT_CALL(button_state, do_on_button_hold());
  EXPECT_CALL(checkpoint, Call());
  EXPECT_CALL(button_state, do_on_button_click(Eq(mouse_buttons::primary)));
  EXPECT_CALL(button_state, do_on_button_exit());
  constexpr auto dummy_w = dummy_widget{default_rect{{0, 0}, {4, 4}}};
  // auto dummy_wref = widget_ref_no_set_area(dummy_w);
  // auto callback = [&button_state](auto &&evt) { button_state.handle(evt); };
  trig.handle(dummy_w.area(), dummy_mouse_move_event{{1, 1}});
  trig.handle(dummy_w.area(),
              dummy_mouse_down_event{{1, 1}, mouse_buttons::primary});
  checkpoint.Call();
  trig.handle(dummy_w.area(),
              dummy_mouse_up_event{{1, 1}, mouse_buttons::primary});
  trig.handle(dummy_w.area(), dummy_mouse_move_event{{-1, 1}});
}

struct mock_renderable {
  MOCK_METHOD(void, do_render, (), (const));

  void render(auto &&...) const { do_render(); }
};

TEST(WidgetBuilder, BuildRender) // NOLINT
{
  auto renderable = mock_renderable();
  EXPECT_CALL(renderable, do_render());
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               .display(std::ref(renderable))
               .build();
  call::render(w, dummy_renderer{});
}

struct mock_colourable {
  MOCK_METHOD(void, do_render, (), (const));
  MOCK_METHOD(void, colour, (default_colour_t const &), ());

  void render(auto &&...) const { do_render(); }
};

inline auto
expect_colour_eq(cgui::colour auto const &val,
                 cgui::colour auto const &expected,
                 std::source_location s = std::source_location::current()) {
  auto [vr, vg, vb, va] = val;
  auto [er, eg, eb, ea] = expected;
  EXPECT_THAT((std::array{vr, vg, vb, va}),
              ElementsAre(Eq(er), Eq(eg), Eq(eb), Eq(ea)))
      << "Called at line " << s.line();
}

TEST(WidgetBuilder, SetColour) // NOLINT
{
  auto m1 = mock_colourable{};
  auto m2 = mock_colourable{};
  InSequence s;
  default_colour_t m1c{}, m2c{};
  EXPECT_CALL(m1, colour(_)).WillOnce([&](default_colour_t const &c) {
    m1c = c;
  });
  EXPECT_CALL(m2, colour(_)).WillOnce([&](default_colour_t const &c) {
    m2c = c;
  });
  EXPECT_CALL(m1, do_render()).Times(1);
  EXPECT_CALL(m2, do_render()).Times(1);
  using namespace dooc::tuple_literals;

  auto w = widget_builder()
               .area(default_rect{})
               .display("text"_na = std::ref(m1), "fill"_na = std::ref(m2))
               .build();
  auto constexpr exp_m1c = default_colour_t{255, 0, 0, 255};
  auto constexpr exp_m2c = default_colour_t{0, 255, 0, 255};
  "text"_from(w.displays()).colour(exp_m1c);
  "fill"_from(w.displays()).colour(exp_m2c);
  expect_colour_eq(m1c, exp_m1c);
  expect_colour_eq(m2c, exp_m2c);
  w.render(dummy_renderer{});
}

struct mock_state_aware_renderer {
  MOCK_METHOD(void, do_render, (int), (const));
  MOCK_METHOD(void, do_set_state, (int), (const));
  std::string_view mutable render_failed_type{};

  void render(renderer auto &&, auto &&arg) const {
    using state_t =
        std::remove_cvref_t<decltype(arg.widget_state().current_state())>;
    if constexpr (std::is_integral_v<state_t>) {
      do_render(arg.widget_state().current_state());
    } else {
      render_failed_type = std::string_view(typeid(state_t).name());
    }
  }
  void set_state(state_marker auto const &i, widget_back_propagater auto &&cb) {
    do_set_state(i.current_state());
    cb.rerender();
  }
};

struct int_states {
  int state_ = 0;
  void handle(int i) {
    CGUI_ASSERT(i == 0 || i == 1);
    state_ = i;
  }
  widget_state_marker<int, 0, 1> state() const { return state_; }
};

static_assert(has_handle<int_states, int>);

struct int_as_event_handler {
  int state_ = 0;
  void handle(auto const &, int i) { state_ = std::clamp(i, 0, 1); }
  widget_state_marker<int, 0, 1> state() const { return state_; }
};

TEST(WidgetBuilder, BuildWithState) // NOLINT
{
  auto state_aware_rend = mock_state_aware_renderer{};
  MockFunction<void()> checkpoint{};
  InSequence s;
  EXPECT_CALL(state_aware_rend, do_render(Eq(0)));
  EXPECT_CALL(state_aware_rend, do_set_state(Eq(1)));
  EXPECT_CALL(checkpoint, Call());
  EXPECT_CALL(state_aware_rend, do_render(Eq(1)));
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               //.state(int_states{})
               .event(int_as_event_handler{})
               .display(std::ref(state_aware_rend))
               .build();
  w.render(dummy_renderer{});
  w.handle(1);
  checkpoint.Call();
  w.render(dummy_renderer{});
  EXPECT_THAT(state_aware_rend.render_failed_type, IsEmpty());
}

TEST(WidgetBuilder, DisplayForEachState) // NOLINT
{
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               .event(int_as_event_handler{})
               //.state(int_states{})
               .display(display_per_state(fill_rect{}))
               .build();
  auto &[per_state] = w.displays();
  get<0>(per_state).colour() = default_colour_t{255, 0, 0, 255};
  get<1>(per_state).colour() = default_colour_t{0, 255, 0, 255};

  auto r = test_renderer({0, 0, 1, 1});
  auto sr = sub_renderer(r);
  w.render(sr);
  ASSERT_THAT(r.drawn_pixels, SizeIs(Eq(1)));
  auto &[red, green, blue, alpha] = r.drawn_pixels[0];
  EXPECT_THAT(red, Eq(255));
  EXPECT_THAT(green, Eq(0));
  EXPECT_THAT(blue, Eq(0));
  EXPECT_THAT(alpha, Eq(255));
  r.drawn_pixels[0] = {};
  bounding_box auto new_area = w.handle(1);
  expect_box_equal(new_area, r.area());
  w.render(sr);
  ASSERT_THAT(r.drawn_pixels, SizeIs(Eq(1)));
  EXPECT_THAT(red, Eq(0));
  EXPECT_THAT(green, Eq(255));
  EXPECT_THAT(blue, Eq(0));
  EXPECT_THAT(alpha, Eq(255));
}

struct mock_widget {
  default_rect a_{};
  MOCK_METHOD(void, do_area, (default_rect const &));
  MOCK_METHOD(void, do_render, (), (const));

  default_rect const &area() const { return a_; }
  void area(bounding_box auto const &a) {
    a_ = copy_box<default_rect>(a);
    do_area(a_);
  }
  void render(auto &&...) const { do_render(); }
};

TEST(WidgetBuilder, SubcomponentsResize) // NOLINT
{
  mock_widget subw;
  int mock_calls{};
  auto sc_area = default_rect{};
  EXPECT_CALL(subw, do_area(_))
      .WillRepeatedly([&mock_calls, &sc_area](auto const &a) {
        ++mock_calls;
        sc_area = a;
      });
  auto w = widget_builder()
               .area(default_rect{{0, 1}, {3, 4}})
               .subcomponents(std::ref(subw))
               .on_resize([](auto &&self, bounding_box auto const &new_area) {
                 self.subcomponent().area(new_area);
               })
               .build();
  EXPECT_THAT(mock_calls, Eq(1)) << "Should be called once on creation";
  expect_box_equal(sc_area, w.area());
  w.area({{0, 2}, {4, 5}});
  EXPECT_THAT(mock_calls, Eq(2));
  expect_box_equal(sc_area, w.area());
}

TEST(WidgetBuilder, SubcomponentsRender) // NOLINT
{
  auto s1 = NiceMock<mock_widget>{};
  auto s2 = NiceMock<mock_widget>{};
  EXPECT_CALL(s1, do_area(_)).Times(1);
  EXPECT_CALL(s2, do_area(_)).Times(1);
  {
    InSequence s;
    EXPECT_CALL(s1, do_render()).Times(1);
    EXPECT_CALL(s2, do_render()).Times(1);
  }
  auto w = widget_builder()
               .area(default_rect{{0, 0}, {3, 3}})
               .subcomponents(std::ref(s1), std::ref(s2))
               .on_resize([](auto &&self, bounding_box auto b) {
                 auto &[s1, s2] = self.subcomponents();
                 s1.area(trim_from_left(&b, 1));
                 s2.area(b);
               })
               .build();
  w.render(dummy_renderer{});
}

TEST(Widget, BasicButton) // NOLINT
{
  bool clicked{};
  using enum momentary_button_states;
  auto last_state = off;
  int calls{};
  auto w = widget_builder()
               .area(default_rect{0, 0, 1, 1})
               .event(buttonlike_trigger(
                   momentary_button()
                       .click([&clicked, &calls](auto &&...) {
                         clicked = true;
                         ++calls;
                       })
                       .hover([&last_state, &calls](auto &&...) {
                         last_state = hover;
                         ++calls;
                       })
                       .hold([&last_state, &calls](auto &&...) {
                         last_state = hold;
                         ++calls;
                       })
                       .exit([&last_state, &calls](auto &&...) {
                         last_state = off;
                         ++calls;
                       })
                       .build()))
               .display(display_per_state(fill_rect{}))
               .build();
  auto &[filler] = w.displays();
  get<off>(filler).colour() = {0, 0, 0, 255};
  get<hover>(filler).colour() = {1, 0, 0, 255};
  get<hold>(filler).colour() = {2, 0, 0, 255};
  test_renderer r{{0, 0, 1, 1}};
  ASSERT_THAT(r.drawn_pixels, SizeIs(1));
  auto &[red, green, blue, alpha] = r.drawn_pixels.front();
  auto sr = sub_renderer(r);
  auto reset = [&clicked, &calls] {
    clicked = false;
    calls = 0;
  };

  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(0, 0, 0, 255));

  w.handle(dummy_mouse_move_event{{-1, 0}});
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(0));
  EXPECT_THAT(last_state, Eq(off));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(0, 0, 0, 255));
  reset();

  w.handle(dummy_mouse_move_event{});
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_state, Eq(hover));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(1, 0, 0, 255));
  reset();

  w.handle(dummy_mouse_down_event{});
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_state, Eq(hold));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(2, 0, 0, 255));
  reset();

  w.handle(dummy_mouse_up_event{});
  EXPECT_THAT(clicked, IsTrue());
  EXPECT_THAT(calls, Eq(2));
  EXPECT_THAT(last_state, Eq(hover));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(1, 0, 0, 255));
  reset();

  w.handle(dummy_mouse_move_event{{0, 2}});
  EXPECT_THAT(clicked, IsFalse());
  EXPECT_THAT(calls, Eq(1));
  EXPECT_THAT(last_state, Eq(off));
  w.render(sr);
  EXPECT_THAT((std::array{red, green, blue, alpha}), ElementsAre(0, 0, 0, 255));
  reset();
}

constexpr void click_widget(auto &w, default_pixel_coord const &pos = {},
                            auto &&...args) {
  w.handle(dummy_mouse_down_event{.pos = pos, .button_id = {}}, args...);
  w.handle(dummy_mouse_up_event{.pos = pos, .button_id = {}}, args...);
}

TEST(Widget, ButtonSharedStateCallback) // NOLINT
{
  int i{};
  auto w = widget_builder()
               .area(default_rect{{0, 0}, {2, 2}})
               .event(buttonlike_trigger(momentary_button()
                                             .callback_state(std::ref(i))
                                             .click([](int &i_in) { ++i_in; })
                                             .build()))
               .build();
  EXPECT_THAT(i, Eq(0));
  click_widget(w);
  EXPECT_THAT(i, Eq(1));
}

struct test_button_list {
  struct element_t {
    test_button_list &parent;
    int index;

    constexpr void handle(radio_button::trigger_on,
                          widget_back_propagater auto &&bp) {
      parent.on_activate(index);
      bp.rerender(default_rect{{index, 0}, {index + 1, 1}});
    }
    constexpr void handle(radio_button::trigger_off,
                          widget_back_propagater auto &&bp) {
      parent.on_deactivate(index);
      bp.rerender(default_rect{{index, 0}, {index + 1, 1}});
    }
    template <state_marker S, widget_back_propagater T>
    constexpr void set_state(S const& s, T&& t) const {
      t.rerender(default_rect{{index, 0}, {index + 1, 1}});
      parent.state_change(s.current_state(), index);
    }
  };
  std::function<void(int)> on_activate = bp::no_op;
  std::function<void(int)> on_deactivate = bp::no_op;
  int sz{};
  std::function<void(radio_button::element_state, int)> state_change = bp::no_op;
  using element_id = std::size_t;
  constexpr bool find_sub_at_location(
      pixel_coord auto const &pos,
      std::invocable<element_t &, std::size_t> auto &&on_find) {
    if (call::x_of(pos) >= 0 && call::x_of(pos) < sz) {
      auto i = call::x_of(pos);
      auto e = element_t(*this, i);
      on_find(e, i);
      return true;
    }
    return false;
  }
  constexpr void for_each(auto &&f) {
    for (int i = 0; i < sz; ++i) {
      f(element_t{*this, i}, static_cast<std::size_t>(i));
    }
  }

  constexpr void render(renderer auto &&r, button_list_args auto &&args) const {
    for (int i = 0; i < sz; ++i) {
      auto bright =
          static_cast<std::uint_least8_t>(args.button_state(i).current_state());
      fill(r, default_rect{{i, 0}, {i + 1, 1}},
           default_colour_t{bright, bright, bright, 255u});
    }
  }
};

TEST(Widget, RadioButtonDecorator) // NOLINT
{
  int current_element = lowest_possible;
  int activations{};
  int deactivations{};
  auto constexpr full_area = default_rect{0, 0, 16, 10};
  auto list =
      widget_builder()
          .area(full_area)
          .event(radio_button_trigger()
                     .elements(test_button_list{
                         [&activations, &current_element](int element) {
                           ++activations;
                           current_element = element;
                         },
                         [&deactivations, &current_element](int element) {
                           ++deactivations;
                           current_element = -1;
                         },
                         3})
                     .build())
          .build();
  // activate button 0
  auto backprop = basic_widget_back_propagater(full_area);
  click_widget(list, {}, backprop);
  EXPECT_THAT(activations, Eq(1));
  EXPECT_THAT(deactivations, Eq(0));
  EXPECT_THAT(current_element, Eq(0));
  // deactivate button 0
  click_widget(list, {}, backprop);
  EXPECT_THAT(activations, Eq(1));
  EXPECT_THAT(deactivations, Eq(1));
  EXPECT_THAT(current_element, Eq(-1));
  // click outside any subcomponents
  click_widget(list, {12, 0}, backprop);
  EXPECT_THAT(activations, Eq(1));
  EXPECT_THAT(deactivations, Eq(1));
  EXPECT_THAT(current_element, Eq(-1));

  // activate button 1
  click_widget(list, {1, 7}, backprop);
  EXPECT_THAT(activations, Eq(2));
  EXPECT_THAT(deactivations, Eq(1));
  EXPECT_THAT(current_element, Eq(1));
}

TEST(Widget, RadioButtonListRender) // NOLINT
{
  constexpr auto full_area = default_rect{{0, 0}, {3, 1}};
  constexpr auto state2bright = [](radio_button::element_state s) {
    return static_cast<std::uint_least8_t>(s);
  };
  constexpr auto state2colour = [=](radio_button::element_state s) {
    auto const val = state2bright(s);
    return default_colour_t{val, val, val, 255};
  };
  using enum radio_button::element_state;
  std::vector<radio_button::element_state> states {relaxed_off, relaxed_off, relaxed_off};
  auto exp_states = states;
  std::vector<std::pair<radio_button::element_state, int>> state_changes;
  auto list =
      widget_builder()
          .area(full_area)
          .event(radio_button_trigger()
                     .elements(test_button_list{bp::no_op, bp::no_op, 3, [&] (auto s, auto i) {
                       state_changes.emplace_back(s, i);
                       ASSERT_THAT(i, Lt(ssize(states)));
                       states.at(i) = s;
                     }})
                     .build())
          .build();
  auto rend = test_renderer{full_area};
  auto sr = sub_renderer(rend);
  auto rgba_sep = rend.individual_colours();
  auto &[r, g, b, a] = rgba_sep;
  auto do_render = [&] {
    list.render(sr);
    rgba_sep = rend.individual_colours();
  };

  do_render();
  EXPECT_THAT(r, AllOf(SizeIs(3), Each(red(state2colour(relaxed_off)))));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));

  auto re_area = call::handle(list, dummy_mouse_move_event{{0, 0}});
  EXPECT_THAT(state_changes, ElementsAre(Pair(hover_off, 0)));
  exp_states[0] = hover_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(hover_off), state2bright(relaxed_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
  expect_box_equal(re_area, default_rect{{0, 0}, {1, 1}});

  state_changes.clear();
  re_area = call::handle(list, dummy_mouse_move_event{{1, 0}});
  EXPECT_THAT(state_changes, UnorderedElementsAre(Pair(relaxed_off, 0),Pair(hover_off, 1)));
  exp_states[0] = relaxed_off;
  exp_states[1] = hover_off;
  EXPECT_THAT(states, ElementsAreArray(exp_states));
  do_render();
  EXPECT_THAT(r, ElementsAre(state2bright(relaxed_off), state2bright(hover_off),
                             state2bright(relaxed_off)));
  EXPECT_THAT(a, AllOf(SizeIs(3), Each(255u)));
  expect_box_equal(re_area, default_rect{{0, 0}, {2, 1}});

  FAIL() << "Not yet implemented. Need to interact with mouse events.";
}

struct mock_widget_resize {

  MOCK_METHOD(void, do_resize, (int w, int h));
  void area(bounding_box auto const &b) {
    do_resize(call::width(b), call::height(b));
  }
  void render(auto &&) const {}
};

template <typename T> struct ref_builder {
  T *to_return_;

  constexpr T &build(auto &&...) const { return *to_return_; }
};

TEST(GuiContext, BuildResize) // NOLINT
{
  auto w = mock_widget_resize();
  InSequence s;
  EXPECT_CALL(w, do_resize(2, 2));
  EXPECT_CALL(w, do_resize(3, 3));
  auto gui =
      gui_context_builder()
          .widgets(std::ref(w))
          .on_resize([](size_wh auto const &wh, auto &&widgets) {
            auto &[w] = widgets;
            w.area(default_rect{0, 0, call::width(wh), call::height(wh)});
          })
          .build({{0, 0}, {2, 2}});
  auto area = gui.handle(dummy_window_resized_event{{3, 3}});
  expect_box_equal(area, default_rect{{0, 0}, {3, 3}});
}

struct rerender_if_state {
  int rerender_state;

  constexpr void render(auto &&...) const noexcept {}
  constexpr void set_state(state_marker auto const &i,
                           widget_back_propagater auto &&cb) {
    if (i.current_state() == rerender_state) {
      cb.rerender();
    }
  }
};

TEST(GuiContext, RerenderOutput) // NOLINT
{
  auto w1b = widget_builder()
                 .area(default_rect{{0, 0}, {1, 1}})
                 .event(int_as_event_handler{})
                 .display(rerender_if_state{0});
  auto w2b = widget_builder()
                 .area(default_rect{{1, 0}, {2, 1}})
                 .event(int_as_event_handler{})
                 .display(rerender_if_state{1});
  auto w3b = widget_builder()
                 .area(default_rect{{2, 0}, {3, 1}})
                 .event(int_as_event_handler{})
                 .display(rerender_if_state{0});
  auto r = test_renderer({{0, 0}, {3, 1}});
  auto guic = gui_context_builder()
                  .widgets(std::move(w1b), std::move(w2b), std::move(w3b))
                  .build({{0, 0}, {1, 1}});
  guic.render(r);
  auto rarea = guic.handle(1);
  expect_box_equal(rarea, default_rect{{1, 0}, {2, 1}});
  rarea = guic.handle(0);
  EXPECT_TRUE(box_includes_box(rarea, default_rect{{0, 0}, {1, 1}}));
  EXPECT_TRUE(box_includes_box(rarea, default_rect{{2, 0}, {3, 1}}));
}
} // namespace cgui::tests

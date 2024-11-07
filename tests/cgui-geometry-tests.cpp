
#include <cgui/geometry.hpp>

#include <gmock/gmock.h>

namespace cgui::tests {
static_assert(point_scalar<point_unit_t<int>>);
static_assert(pixel_scalar<pixel_unit_t<int>>);
static_assert(pixel_or_point_rect<autoconverting_pixelpoint_unit<pixel_size_tag, default_rect>>);
static_assert(bounding_box<default_rect>);
static_assert(impl::has_bbox_init<default_rect, int>);
static_assert(std::is_same_v<
              pixelpoint_unit<pixel_size_tag, default_rect>,
              decltype(extend_api_t<pixelpoint_unit<pixel_size_tag, default_rect>>::from_xyxy(
                  0, 0, 0, 0))>);
static_assert(mutable_bounding_box<pixelpoint_unit<pixel_size_tag, default_rect>, pixelpoint_unit<pixel_size_tag, int>>);


using namespace ::testing;

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


}

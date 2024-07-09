#include "fun_rt.hpp"
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>

TEST(FunRuntimeTest, BasicDataCreation) {
    auto x = ff_make_int(1);
    EXPECT_EQ(x.tag, FF_INT_TAG);
    EXPECT_EQ(ff_get_int(x), 1);

    auto y = ff_make_int(0);
    EXPECT_EQ(y.tag, FF_INT_TAG);
    EXPECT_EQ(ff_get_int(y), 0);

    auto z = ff_make_str("Hello");
    EXPECT_EQ(z.tag, FF_STR_TAG);
    EXPECT_STREQ(ff_get_str(z), "Hello");

    z = ff_make_str("");
    EXPECT_EQ(z.tag, FF_STR_TAG);
    EXPECT_STREQ(ff_get_str(z), "");
}

TEST(FunRuntimeTest, BasicDataOperation) {
    // todo: property test is good at this job
    auto x = ff_make_int(1);
    auto y = ff_make_int(3);

    EXPECT_EQ(1 + 3, ff_get_int(ff_add_int(x, y)));
}

// fun arg_z -> x + y + z
ff_obj_t add_closure_cfn(ff_fvs_t fvs, ff_obj_t arg_z) {
    auto fv_x = fvs[0];
    auto fv_y = fvs[1];
    return ff_add_int(ff_add_int(fv_x, fv_y), arg_z);
}

TEST(FunRuntimeTest, ClosureOperation) {
    ff_obj_t fvs[] = {ff_make_int(1), ff_make_int(2)};

    auto clos = ff_make_closure(fvs, 2, (ff_erased_fptr)add_closure_cfn);

    ff_obj_t x = ff_make_int(3);
    auto result = ff_apply_generic(clos, x);
    EXPECT_EQ(ff_get_int(result), 6);
}

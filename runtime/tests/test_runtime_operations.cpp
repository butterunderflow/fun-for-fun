#include "fun_rt.hpp"
#include "fun_rt_core.hpp"
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
    auto z_1 = ff_make_str("");
    EXPECT_EQ(z_1.tag, FF_STR_TAG);
    EXPECT_STREQ(ff_get_str(z_1), "");
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

TEST(FunRuntimeTest, SimpleClosureCreation) {
    ff_obj_t fvs[] = {ff_make_int(1), ff_make_int(2)};

    auto clos = ff_make_closure(fvs, 2, (ff_erased_fptr)add_closure_cfn);

    ff_obj_t x = ff_make_int(3);
    auto result = ff_apply_generic(clos, x);
    EXPECT_EQ(ff_get_int(result), 6);
}

// fun y ->  x + y + z
ff_obj_t adder_closure_cfn1(ff_fvs_t fvs, ff_obj_t arg_y) {
    auto fv_z = fvs[0];
    auto fv_x = fvs[1];
    return ff_add_int(ff_add_int(fv_x, arg_y), fv_z);
}

// fun arg_x -> fun y ->  x + y + z
ff_obj_t adder_closure_cfn2(ff_fvs_t fvs, ff_obj_t arg_x) {
    auto fv_z = fvs[0];
    return ff_make_closure({fv_z, arg_x}, 2,
                           (ff_erased_fptr)adder_closure_cfn1);
}

TEST(FunRuntimeTest, AdderClosureCreation) {
    auto fv1 = ff_make_int(1);

    auto clos1 = ff_make_closure({fv1}, 1, (ff_erased_fptr)adder_closure_cfn2);

    auto clos2 = ff_apply_generic(clos1, ff_make_int(2));

    auto result = ff_apply_generic(clos2, ff_make_int(3));
    EXPECT_EQ(ff_get_int(result), 6);
}

TEST(FunRuntimeTest, BuiltinAdder) {
    auto adder = ff_apply_generic(ff_builtin_add, ff_make_int(5));
    auto result = ff_apply_generic(adder, ff_make_int(10));

    EXPECT_EQ(ff_get_tag(result), FF_INT_TAG);
    EXPECT_EQ(ff_get_int(result), 15);
}

TEST(FunRuntimeTest, TupleCreation) {
    auto x = ff_make_int(1);
    auto y = ff_make_str("Hello");
    auto z = ff_make_int(3);
    auto tu = ff_make_tuple({x, y, z}, 3);

    EXPECT_EQ(tu.tag, FF_TUPLE_TAG);
}

TEST(FunRuntimeTest, TupleMatch) {
    auto x = ff_make_int(1);
    auto y = ff_make_str("Hello");
    auto z = ff_make_int(3);
    auto tu = ff_make_tuple({x, y, z}, 3);

    ff_obj_t x_1;
    ff_obj_t y_1;
    ff_obj_t z_1;
    ff_match_tuple(tu, {&x_1, &y_1, &z_1});
    EXPECT_EQ(x_1.tag, FF_INT_TAG);
    EXPECT_TRUE(ff_is_equal_aux(x, x_1));

    EXPECT_EQ(y_1.tag, FF_STR_TAG);
    EXPECT_TRUE(ff_is_equal_aux(y, y_1));
    EXPECT_EQ(z_1.tag, FF_INT_TAG);
    EXPECT_TRUE(ff_is_equal_aux(z, z_1));
}

TEST(FunRuntimeTest, ModObjectCreation) {
    {
        auto x = ff_make_int(1);
        auto y = ff_make_str("Hello");
        auto z = ff_make_int(3);

        auto mod = ff_make_mod_obj(3, {"x", "y", "z"}, {x, y, z});
        EXPECT_EQ(mod.tag, FF_MODOBJ_TAG);

        auto x_1 = ff_get_member(mod, "x");
        EXPECT_TRUE(ff_is_equal_aux(x, x_1));

        auto y_1 = ff_get_member(mod, "y");
        EXPECT_TRUE(ff_is_equal_aux(y, y_1));
    }

    {
        auto mod = ff_make_mod_obj(0, {}, {});
        EXPECT_EQ(mod.tag, FF_MODOBJ_TAG);
    }
}

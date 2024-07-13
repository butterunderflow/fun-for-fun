#include "fun_rt_core.hpp"
#include "fun_rt_lib.hpp"
#include <cstdio>

void test_rt() {
    printf("Hello Runtime\n");
}

ff_obj_t ff_add_int(ff_obj_t x, ff_obj_t y) {
    assert(x.tag == FF_INT_TAG);
    assert(y.tag == FF_INT_TAG);
    return ff_make_int(ff_get_int(x) + ff_get_int(y));
}

ff_obj_t ff_adder_cfn(ff_fvs_t fvs, ff_obj_t y) {
    auto x = fvs[0];
    auto result = ff_add_int(x, y);
    return result;
}

ff_obj_t ff_make_adder_cfn(ff_fvs_t fvs, ff_obj_t x) {
    auto adder = ff_make_closure({x}, 1, (ff_erased_fptr)ff_adder_cfn);
    return adder;
}

const ff_obj_t ff_builtin_add =
    ff_make_closure({}, 0, (ff_erased_fptr)ff_make_adder_cfn);

ff_obj_t ff_print_int_cfn(ff_fvs_t fvs, ff_obj_t x) {
    assert(x.tag == FF_INT_TAG);
    auto value = ff_get_int(x);
    std::printf("%ld", value);
    return ff_make_int(0);
}

ff_obj_t ff_print_bool_cfn(ff_fvs_t fvs, ff_obj_t x) {
    assert(x.tag == FF_INT_TAG);
    auto value = ff_get_int(x);
    if (value == 0) {
        std::printf("false");
    } else {
        std::printf("true");
    }
    return ff_make_int(0);
}

const ff_obj_t ff_builtin_print_int =
    ff_make_closure({}, 0, (ff_erased_fptr)ff_print_int_cfn);

const ff_obj_t ff_builtin_print_bool =
    ff_make_closure({}, 0, (ff_erased_fptr)ff_print_bool_cfn);

ff_obj_t ff_print_str_cfn(ff_fvs_t fvs, ff_obj_t x) {
    auto value = ff_get_str(x);
    std::printf("%s", value);
    return ff_make_int(0);
}

const ff_obj_t ff_builtin_print_str =
    ff_make_closure({}, 0, (ff_erased_fptr)ff_print_str_cfn);

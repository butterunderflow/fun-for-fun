#ifndef FUN4FUN_RUNTIME_FUN_RT_LIB_HPP
#define FUN4FUN_RUNTIME_FUN_RT_LIB_HPP
#include "fun_rt_core.hpp"

void test_rt();

ff_obj_t ff_add_int(ff_obj_t x, ff_obj_t y);

ff_obj_t ff_is_equal(ff_obj_t x, ff_obj_t y);

ff_obj_t ff_is_not_equal(ff_obj_t x, ff_obj_t y);

extern const ff_obj_t ff_builtin_add;

extern const ff_obj_t ff_builtin_print_int;

extern const ff_obj_t ff_builtin_print_str;

extern const ff_obj_t ff_builtin_print_bool;

#endif /* FUN4FUN_RUNTIME_FUN_RT_LIB_HPP */

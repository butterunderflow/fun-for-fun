  $ $FF test_equality.fun

  $ cat test_equality.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_35__l;
  	ff_obj_t z_34__l;
  	ff_obj_t is_eq_33__l;
  	ff_obj_t app_res_32__l;
  	ff_obj_t tu2_31__l;
  	ff_obj_t tu_30__l;
  	ff_obj_t temp_29__l;
  	ff_obj_t temp_28__l;
  	ff_obj_t tu1_27__l;
  	ff_obj_t tu_26__l;
  	ff_obj_t temp_25__l;
  	ff_obj_t temp_24__l;
  	ff_obj_t z_23__l;
  	ff_obj_t is_eq_22__l;
  	ff_obj_t app_res_21__l;
  	ff_obj_t tu2_20__l;
  	ff_obj_t tu_19__l;
  	ff_obj_t temp_18__l;
  	ff_obj_t temp_17__l;
  	ff_obj_t tu1_16__l;
  	ff_obj_t tu_15__l;
  	ff_obj_t temp_14__l;
  	ff_obj_t temp_13__l;
  	ff_obj_t x_12__l;
  	ff_obj_t temp_11__l;
  	ff_obj_t temp_10__l;
  	ff_obj_t is_eq_9__l;
  	ff_obj_t app_res_8__l;
  	ff_obj_t x_7__l;
  	ff_obj_t temp_6__l;
  	ff_obj_t temp_5__l;
  	ff_obj_t is_eq_4__l;
  	ff_obj_t app_res_3__l;
  	ff_obj_t print_bool_2__l;
  	print_bool_2__l = ff_builtin_print_bool;
  	temp_5__l = ff_make_int(1);
  	temp_6__l = ff_make_int(1);
  	is_eq_4__l = ff_is_equal(temp_5__l, temp_6__l);
  	app_res_3__l = ff_apply_generic(print_bool_2__l, is_eq_4__l);
  	x_7__l = app_res_3__l;
  	temp_10__l = ff_make_int(1);
  	temp_11__l = ff_make_int(1);
  	is_eq_9__l = ff_is_not_equal(temp_10__l, temp_11__l);
  	app_res_8__l = ff_apply_generic(print_bool_2__l, is_eq_9__l);
  	x_12__l = app_res_8__l;
  	temp_13__l = ff_make_int(1);
  	temp_14__l = ff_make_int(2);
  	tu_15__l = ff_make_tuple({temp_13__l, temp_14__l}, 2);
  	tu1_16__l = tu_15__l;
  	temp_17__l = ff_make_int(1);
  	temp_18__l = ff_make_int(2);
  	tu_19__l = ff_make_tuple({temp_17__l, temp_18__l}, 2);
  	tu2_20__l = tu_19__l;
  	is_eq_22__l = ff_is_equal(tu1_16__l, tu2_20__l);
  	app_res_21__l = ff_apply_generic(print_bool_2__l, is_eq_22__l);
  	z_23__l = app_res_21__l;
  	temp_24__l = ff_make_int(1);
  	temp_25__l = ff_make_int(2);
  	tu_26__l = ff_make_tuple({temp_24__l, temp_25__l}, 2);
  	tu1_27__l = tu_26__l;
  	temp_28__l = ff_make_int(1);
  	temp_29__l = ff_make_int(3);
  	tu_30__l = ff_make_tuple({temp_28__l, temp_29__l}, 2);
  	tu2_31__l = tu_30__l;
  	is_eq_33__l = ff_is_equal(tu1_27__l, tu2_31__l);
  	app_res_32__l = ff_apply_generic(print_bool_2__l, is_eq_33__l);
  	z_34__l = app_res_32__l;
  	mod_35__l = ff_make_mod_obj(5, {"print_bool", "x", "x", "z", "z"},
  		{print_bool_2__l, x_7__l, x_12__l, z_23__l, z_34__l});
  	return mod_35__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }

  $ ./test_equality.fun.out
  Hello Runtime
  truefalsetruefalse


  $ $FF test_equality.fun

  $ cat test_equality.fun.cpp
  
  #include "fun_rt.hpp"
  #include <stdio.h>
  #include <stdexcept>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_35;
  	ff_obj_t z_34;
  	ff_obj_t is_eq_33;
  	ff_obj_t app_res_32;
  	ff_obj_t tu2_31;
  	ff_obj_t tu_30;
  	ff_obj_t temp_29;
  	ff_obj_t temp_28;
  	ff_obj_t tu1_27;
  	ff_obj_t tu_26;
  	ff_obj_t temp_25;
  	ff_obj_t temp_24;
  	ff_obj_t z_23;
  	ff_obj_t is_eq_22;
  	ff_obj_t app_res_21;
  	ff_obj_t tu2_20;
  	ff_obj_t tu_19;
  	ff_obj_t temp_18;
  	ff_obj_t temp_17;
  	ff_obj_t tu1_16;
  	ff_obj_t tu_15;
  	ff_obj_t temp_14;
  	ff_obj_t temp_13;
  	ff_obj_t x_12;
  	ff_obj_t temp_11;
  	ff_obj_t temp_10;
  	ff_obj_t is_eq_9;
  	ff_obj_t app_res_8;
  	ff_obj_t x_7;
  	ff_obj_t temp_6;
  	ff_obj_t temp_5;
  	ff_obj_t is_eq_4;
  	ff_obj_t app_res_3;
  	ff_obj_t print_bool_2;
  	print_bool_2 = ff_builtin_print_bool;
  	temp_5 = ff_make_int(1);
  	temp_6 = ff_make_int(1);
  	is_eq_4 = ff_is_equal(temp_5, temp_6);
  	app_res_3 = ff_apply_generic(print_bool_2, is_eq_4);
  	x_7 = app_res_3;
  	temp_10 = ff_make_int(1);
  	temp_11 = ff_make_int(1);
  	is_eq_9 = ff_is_not_equal(temp_10, temp_11);
  	app_res_8 = ff_apply_generic(print_bool_2, is_eq_9);
  	x_12 = app_res_8;
  	temp_13 = ff_make_int(1);
  	temp_14 = ff_make_int(2);
  	tu_15 = ff_make_tuple({temp_13, temp_14}, 2);
  	tu1_16 = tu_15;
  	temp_17 = ff_make_int(1);
  	temp_18 = ff_make_int(2);
  	tu_19 = ff_make_tuple({temp_17, temp_18}, 2);
  	tu2_20 = tu_19;
  	is_eq_22 = ff_is_equal(tu1_16, tu2_20);
  	app_res_21 = ff_apply_generic(print_bool_2, is_eq_22);
  	z_23 = app_res_21;
  	temp_24 = ff_make_int(1);
  	temp_25 = ff_make_int(2);
  	tu_26 = ff_make_tuple({temp_24, temp_25}, 2);
  	tu1_27 = tu_26;
  	temp_28 = ff_make_int(1);
  	temp_29 = ff_make_int(3);
  	tu_30 = ff_make_tuple({temp_28, temp_29}, 2);
  	tu2_31 = tu_30;
  	is_eq_33 = ff_is_equal(tu1_27, tu2_31);
  	app_res_32 = ff_apply_generic(print_bool_2, is_eq_33);
  	z_34 = app_res_32;
  	mod_35 = ff_make_mod_obj(5, {"print_bool", "x", "x", "z", "z"},
  		{print_bool_2, x_7, x_12, z_23, z_34});
  	return mod_35;
  }
  
  
  int main()
  {
    try
    {
      main_1__fn(nullptr);
    }
    catch (const std::runtime_error& error)
    {
      printf("Runtime error: %s", error.what());
    }
  }

  $ ./test_equality.fun.out
  truefalsetruefalse


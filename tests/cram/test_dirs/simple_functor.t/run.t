

  $ $FF test_increment_functor.fun

  $ ./test_increment_functor.fun.out
  Hello Runtime
  1
  2
  3
  4
  4

  $ cat test_increment_functor.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  ff_obj_t Increment_2__fn(ff_fvs_t fvs_3, ff_obj_t M_2);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_39;
  	ff_obj_t __38;
  	ff_obj_t field_37;
  	ff_obj_t app_res_36;
  	ff_obj_t Four__35;
  	ff_obj_t app_res_34;
  	ff_obj_t Three_and_more_33;
  	ff_obj_t mod_32;
  	ff_obj_t y_31;
  	ff_obj_t temp_30;
  	ff_obj_t x_29;
  	ff_obj_t temp_28;
  	ff_obj_t __27;
  	ff_obj_t field_26;
  	ff_obj_t app_res_25;
  	ff_obj_t Four_24;
  	ff_obj_t app_res_23;
  	ff_obj_t __22;
  	ff_obj_t field_21;
  	ff_obj_t app_res_20;
  	ff_obj_t Three_19;
  	ff_obj_t app_res_18;
  	ff_obj_t __17;
  	ff_obj_t field_16;
  	ff_obj_t app_res_15;
  	ff_obj_t Two_14;
  	ff_obj_t app_res_13;
  	ff_obj_t __12;
  	ff_obj_t field_11;
  	ff_obj_t app_res_10;
  	ff_obj_t One_9;
  	ff_obj_t mod_8;
  	ff_obj_t x_7;
  	ff_obj_t temp_6;
  	ff_obj_t Increment_5;
  	ff_obj_t clos_4;
  	ff_obj_t println_int_3;
  	ff_obj_t add_2;
  	add_2 = ff_builtin_add;
  	println_int_3 = ff_builtin_println_int;
  	clos_4 = ff_make_closure({add_2}, 1, (ff_erased_fptr)Increment_2__fn);
  	Increment_5 = clos_4;
  	temp_6 = ff_make_int(1);
  	x_7 = temp_6;
  	mod_8 = ff_make_mod_obj(1, {"x"}, {x_7});
  	One_9 = mod_8;
  	field_11 = ff_get_member(One_9, "x");
  	app_res_10 = ff_apply_generic(println_int_3, field_11);
  	__12 = app_res_10;
  	app_res_13 = ff_apply_generic(Increment_5, One_9);
  	Two_14 = app_res_13;
  	field_16 = ff_get_member(Two_14, "x");
  	app_res_15 = ff_apply_generic(println_int_3, field_16);
  	__17 = app_res_15;
  	app_res_18 = ff_apply_generic(Increment_5, Two_14);
  	Three_19 = app_res_18;
  	field_21 = ff_get_member(Three_19, "x");
  	app_res_20 = ff_apply_generic(println_int_3, field_21);
  	__22 = app_res_20;
  	app_res_23 = ff_apply_generic(Increment_5, Three_19);
  	Four_24 = app_res_23;
  	field_26 = ff_get_member(Four_24, "x");
  	app_res_25 = ff_apply_generic(println_int_3, field_26);
  	__27 = app_res_25;
  	temp_28 = ff_make_int(3);
  	x_29 = temp_28;
  	temp_30 = ff_make_str("three");
  	y_31 = temp_30;
  	mod_32 = ff_make_mod_obj(2, {"x", "y"}, {x_29, y_31});
  	Three_and_more_33 = mod_32;
  	app_res_34 = ff_apply_generic(Increment_5, Three_and_more_33);
  	Four__35 = app_res_34;
  	field_37 = ff_get_member(Four__35, "x");
  	app_res_36 = ff_apply_generic(println_int_3, field_37);
  	__38 = app_res_36;
  	mod_39 = ff_make_mod_obj(14, {"add", "println_int", "Increment", "One",
  		"_", "Two", "_", "Three", "_", "Four", "_", "Three_and_more",
  		"Four_", "_"}, {add_2, println_int_3, Increment_5, One_9, __12,
  		Two_14, __17, Three_19, __22, Four_24, __27, Three_and_more_33,
  		Four__35, __38});
  	return mod_39;
  }
  
  ff_obj_t Increment_2__fn(ff_fvs_t fvs_3, ff_obj_t M_2)
  {
  	ff_obj_t mod_9;
  	ff_obj_t x_8;
  	ff_obj_t temp_7;
  	ff_obj_t field_6;
  	ff_obj_t app_res_5;
  	ff_obj_t app_res_4;
  	ff_obj_t add_1;
  	add_1 = fvs_3[0];
  	field_6 = ff_get_member(M_2, "x");
  	app_res_5 = ff_apply_generic(add_1, field_6);
  	temp_7 = ff_make_int(1);
  	app_res_4 = ff_apply_generic(app_res_5, temp_7);
  	x_8 = app_res_4;
  	mod_9 = ff_make_mod_obj(1, {"x"}, {x_8});
  	return mod_9;
  }
  
  
  int main()
  {
    test_rt();
    main_1__fn(nullptr);
  }

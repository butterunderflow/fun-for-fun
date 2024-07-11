

  $ $FF test_match.fun

  $ cat test_match.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1();
  ff_obj_t f_1(ff_fvs_t fvs_3, ff_obj_t x_2);
  
  ff_obj_t _ff_main_1()
  {
  	ff_obj_t mod_15;
  	ff_obj_t n_14;
  	ff_obj_t temp_13;
  	ff_obj_t constr0_12;
  	ff_obj_t app_res_11;
  	ff_obj_t app_res_10;
  	ff_obj_t n_9;
  	ff_obj_t constr1_8;
  	ff_obj_t app_res_7;
  	ff_obj_t f_6;
  	ff_obj_t clos_5;
  	ff_obj_t x_4;
  	ff_obj_t constr1_3;
  	ff_obj_t print_int_2;
  	print_int_2 = ff_builtin_print_int;
  	constr1_3 = ff_make_constr_no_payload(1);
  	x_4 = constr1_3;
  	clos_5 = ff_make_closure({print_int_2}, 1, (ff_erased_fptr)f_1);
  	f_6 = clos_5;
  	constr1_8 = ff_make_constr_no_payload(1);
  	app_res_7 = ff_apply_generic(f_6, constr1_8);
  	n_9 = app_res_7;
  	constr0_12 = ff_make_constr_payload(0);
  	temp_13 = ff_make_int(998);
  	app_res_11 = ff_apply_generic(constr0_12, temp_13);
  	app_res_10 = ff_apply_generic(f_6, app_res_11);
  	n_14 = app_res_10;
  	mod_15 = ff_make_mod_obj(5, {"print_int", "x", "f", "n", "n"},
  		{print_int_2, x_4, f_6, n_9, n_14});
  	return mod_15;
  }
  
  ff_obj_t f_1(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t temp_9;
  	ff_obj_t app_res_8;
  	ff_obj_t app_res_7;
  	ff_obj_t x_6;
  	ff_obj_t pat_var_5;
  	ff_obj_t match_res_4;
  	ff_obj_t print_int_1;
  	print_int_1 = fvs_3[0];
  	do
  	{
  		if(ff_match_constr(0, x_2, &pat_var_5))
  		{
  			x_6 = pat_var_5;
  			app_res_7 = ff_apply_generic(print_int_1, x_6);
  			match_res_4 = app_res_7;
  			break;
  		}
  		if(ff_match_constr(1, x_2))
  		{
  			temp_9 = ff_make_int(1);
  			app_res_8 = ff_apply_generic(print_int_1, temp_9);
  			match_res_4 = app_res_8;
  			break;
  		}
  	}
  	while(0);
  	return match_res_4;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1();
  }

  $ ./test_match.fun.out
  Hello Runtime
  1998

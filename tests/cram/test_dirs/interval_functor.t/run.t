  $ ff test_interval.fun -o test_interval.cpp

  $ cat test_interval.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  ff_obj_t print_int_interval_14__fn(ff_fvs_t fvs_4, ff_obj_t interval_3);
  ff_obj_t Make_interval_2__fn(ff_fvs_t fvs_3, ff_obj_t Endpoint_2);
  ff_obj_t intersect_8__fn(ff_fvs_t fvs_5, ff_obj_t t1_4);
  ff_obj_t intersect_9__fn(ff_fvs_t fvs_6, ff_obj_t t2_5);
  ff_obj_t min_10__fn(ff_fvs_t fvs_4, ff_obj_t x_3);
  ff_obj_t min_11__fn(ff_fvs_t fvs_5, ff_obj_t y_4);
  ff_obj_t max_12__fn(ff_fvs_t fvs_4, ff_obj_t x_3);
  ff_obj_t max_13__fn(ff_fvs_t fvs_5, ff_obj_t y_4);
  ff_obj_t contains_6__fn(ff_fvs_t fvs_2, ff_obj_t t_1);
  ff_obj_t contains_7__fn(ff_fvs_t fvs_3, ff_obj_t x_2);
  ff_obj_t is_empty_5__fn(ff_fvs_t fvs_2, ff_obj_t interval_1);
  ff_obj_t create_3__fn(ff_fvs_t fvs_4, ff_obj_t low_3);
  ff_obj_t create_4__fn(ff_fvs_t fvs_5, ff_obj_t high_4);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_31;
  	ff_obj_t __30;
  	ff_obj_t app_res_29;
  	ff_obj_t __28;
  	ff_obj_t app_res_27;
  	ff_obj_t print_int_interval_26;
  	ff_obj_t clos_25;
  	ff_obj_t ii_0_10_24;
  	ff_obj_t tu_23;
  	ff_obj_t temp_22;
  	ff_obj_t temp_21;
  	ff_obj_t constr0_20;
  	ff_obj_t app_res_19;
  	ff_obj_t i_0_10_18;
  	ff_obj_t temp_17;
  	ff_obj_t temp_16;
  	ff_obj_t field_15;
  	ff_obj_t app_res_14;
  	ff_obj_t app_res_13;
  	ff_obj_t Int_interval_12;
  	ff_obj_t app_res_11;
  	ff_obj_t Int_10;
  	ff_obj_t mod_9;
  	ff_obj_t compare_8;
  	ff_obj_t Make_interval_7;
  	ff_obj_t clos_6;
  	ff_obj_t greater_5;
  	ff_obj_t compare_4;
  	ff_obj_t println_string_3;
  	ff_obj_t println_int_2;
  	println_int_2 = ff_builtin_println_int;
  	println_string_3 = ff_builtin_println_str;
  	compare_4 = ff_builtin_minus;
  	greater_5 = ff_builtin_greater;
  	clos_6 = ff_make_closure({greater_5}, 1,
  		(ff_erased_fptr)Make_interval_2__fn);
  	Make_interval_7 = clos_6;
  	compare_8 = compare_4;
  	mod_9 = ff_make_mod_obj(1, {"compare"}, {compare_8});
  	Int_10 = mod_9;
  	app_res_11 = ff_apply_generic(Make_interval_7, Int_10);
  	Int_interval_12 = app_res_11;
  	field_15 = ff_get_member(Int_interval_12, "create");
  	temp_16 = ff_make_int(0);
  	app_res_14 = ff_apply_generic(field_15, temp_16);
  	temp_17 = ff_make_int(10);
  	app_res_13 = ff_apply_generic(app_res_14, temp_17);
  	i_0_10_18 = app_res_13;
  	constr0_20 = ff_make_constr_payload(0);
  	temp_21 = ff_make_int(0);
  	temp_22 = ff_make_int(10);
  	tu_23 = ff_make_tuple({temp_21, temp_22}, 2);
  	app_res_19 = ff_apply_generic(constr0_20, tu_23);
  	ii_0_10_24 = app_res_19;
  	clos_25 = ff_make_closure({println_string_3, println_int_2}, 2,
  		(ff_erased_fptr)print_int_interval_14__fn);
  	print_int_interval_26 = clos_25;
  	app_res_27 = ff_apply_generic(print_int_interval_26, ii_0_10_24);
  	__28 = app_res_27;
  	app_res_29 = ff_apply_generic(print_int_interval_26, i_0_10_18);
  	__30 = app_res_29;
  	mod_31 = ff_make_mod_obj(12, {"println_int", "println_string",
  		"compare", "greater", "Make_interval", "Int", "Int_interval",
  		"i_0_10", "ii_0_10", "print_int_interval", "_", "_"},
  		{println_int_2, println_string_3, compare_4, greater_5,
  		Make_interval_7, Int_10, Int_interval_12, i_0_10_18, ii_0_10_24,
  		print_int_interval_26, __28, __30});
  	return mod_31;
  }
  
  ff_obj_t print_int_interval_14__fn(ff_fvs_t fvs_4, ff_obj_t interval_3)
  {
  	ff_obj_t temp_18;
  	ff_obj_t app_res_17;
  	ff_obj_t app_res_16;
  	ff_obj_t temp_15;
  	ff_obj_t app_res_14;
  	ff_obj_t app_res_13;
  	ff_obj_t temp_12;
  	ff_obj_t app_res_11;
  	ff_obj_t h_10;
  	ff_obj_t l_9;
  	ff_obj_t tu_1th_8;
  	ff_obj_t tu_0th_7;
  	ff_obj_t pat_var_6;
  	ff_obj_t match_res_5;
  	ff_obj_t println_string_1;
  	ff_obj_t println_int_2;
  	println_string_1 = fvs_4[0];
  	println_int_2 = fvs_4[1];
  	do
  	{
  		if(ff_match_constr(0, interval_3, &pat_var_6))
  		{
  			if(ff_match_tuple(pat_var_6, {&tu_0th_7, &tu_1th_8}))
  			{
  				l_9 = tu_0th_7;
  				h_10 = tu_1th_8;
  				temp_12 = ff_make_str("low");
  				app_res_11 = ff_apply_generic(println_string_1,
  					temp_12);
  				app_res_13 = ff_apply_generic(println_int_2,
  					l_9);
  				temp_15 = ff_make_str("high");
  				app_res_14 = ff_apply_generic(println_string_1,
  					temp_15);
  				app_res_16 = ff_apply_generic(println_int_2,
  					h_10);
  				match_res_5 = app_res_16;
  				break;
  			}
  		}
  		if(ff_match_constr(1, interval_3))
  		{
  			temp_18 = ff_make_str("Empty");
  			app_res_17 = ff_apply_generic(println_string_1,
  				temp_18);
  			match_res_5 = app_res_17;
  			break;
  		}
  	}
  	while(0);
  	return match_res_5;
  }
  
  ff_obj_t Make_interval_2__fn(ff_fvs_t fvs_3, ff_obj_t Endpoint_2)
  {
  	ff_obj_t mod_12;
  	ff_obj_t intersect_11;
  	ff_obj_t clos_10;
  	ff_obj_t contains_9;
  	ff_obj_t clos_8;
  	ff_obj_t is_empty_7;
  	ff_obj_t clos_6;
  	ff_obj_t create_5;
  	ff_obj_t clos_4;
  	ff_obj_t greater_1;
  	greater_1 = fvs_3[0];
  	clos_4 = ff_make_closure({greater_1, Endpoint_2}, 2,
  		(ff_erased_fptr)create_3__fn);
  	create_5 = clos_4;
  	clos_6 = ff_make_closure({}, 0, (ff_erased_fptr)is_empty_5__fn);
  	is_empty_7 = clos_6;
  	clos_8 = ff_make_closure({}, 0, (ff_erased_fptr)contains_6__fn);
  	contains_9 = clos_8;
  	clos_10 = ff_make_closure({greater_1, Endpoint_2, create_5}, 3,
  		(ff_erased_fptr)intersect_8__fn);
  	intersect_11 = clos_10;
  	mod_12 = ff_make_mod_obj(4, {"create", "is_empty", "contains",
  		"intersect"}, {create_5, is_empty_7, contains_9, intersect_11});
  	return mod_12;
  }
  
  ff_obj_t intersect_8__fn(ff_fvs_t fvs_5, ff_obj_t t1_4)
  {
  	ff_obj_t clos_6;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	ff_obj_t create_3;
  	greater_1 = fvs_5[0];
  	Endpoint_2 = fvs_5[1];
  	create_3 = fvs_5[2];
  	clos_6 = ff_make_closure({greater_1, Endpoint_2, t1_4, create_3}, 4,
  		(ff_erased_fptr)intersect_9__fn);
  	return clos_6;
  }
  
  ff_obj_t intersect_9__fn(ff_fvs_t fvs_6, ff_obj_t t2_5)
  {
  	ff_obj_t app_res_38;
  	ff_obj_t app_res_37;
  	ff_obj_t app_res_36;
  	ff_obj_t app_res_35;
  	ff_obj_t app_res_34;
  	ff_obj_t app_res_33;
  	ff_obj_t h2_32;
  	ff_obj_t l2_31;
  	ff_obj_t tu_1th_30;
  	ff_obj_t tu_0th_29;
  	ff_obj_t pat_var_28;
  	ff_obj_t h1_27;
  	ff_obj_t l1_26;
  	ff_obj_t tu_1th_25;
  	ff_obj_t tu_0th_24;
  	ff_obj_t pat_var_23;
  	ff_obj_t tu_1th_22;
  	ff_obj_t tu_0th_21;
  	ff_obj_t constr1_20;
  	ff_obj_t __19;
  	ff_obj_t tu_1th_18;
  	ff_obj_t tu_0th_17;
  	ff_obj_t constr1_16;
  	ff_obj_t __15;
  	ff_obj_t tu_1th_14;
  	ff_obj_t tu_0th_13;
  	ff_obj_t tu_12;
  	ff_obj_t match_res_11;
  	ff_obj_t max_10;
  	ff_obj_t clos_9;
  	ff_obj_t min_8;
  	ff_obj_t clos_7;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	ff_obj_t t1_3;
  	ff_obj_t create_4;
  	greater_1 = fvs_6[0];
  	Endpoint_2 = fvs_6[1];
  	t1_3 = fvs_6[2];
  	create_4 = fvs_6[3];
  	clos_7 = ff_make_closure({greater_1, Endpoint_2}, 2,
  		(ff_erased_fptr)min_10__fn);
  	min_8 = clos_7;
  	clos_9 = ff_make_closure({greater_1, Endpoint_2}, 2,
  		(ff_erased_fptr)max_12__fn);
  	max_10 = clos_9;
  	do
  	{
  		tu_12 = ff_make_tuple({t1_3, t2_5}, 2);
  		if(ff_match_tuple(tu_12, {&tu_0th_13, &tu_1th_14}))
  		{
  			if(ff_match_constr(1, tu_0th_13))
  			{
  				__15 = tu_1th_14;
  				constr1_16 = ff_make_constr_no_payload(1);
  				match_res_11 = constr1_16;
  				break;
  			}
  		}
  		if(ff_match_tuple(tu_12, {&tu_0th_17, &tu_1th_18}))
  		{
  			__19 = tu_0th_17;
  			if(ff_match_constr(1, tu_1th_18))
  			{
  				constr1_20 = ff_make_constr_no_payload(1);
  				match_res_11 = constr1_20;
  				break;
  			}
  		}
  		if(ff_match_tuple(tu_12, {&tu_0th_21, &tu_1th_22}))
  		{
  			if(ff_match_constr(0, tu_0th_21, &pat_var_23))
  			{
  				if(ff_match_tuple(pat_var_23, {&tu_0th_24,
  					&tu_1th_25}))
  				{
  					l1_26 = tu_0th_24;
  					h1_27 = tu_1th_25;
  					if(ff_match_constr(0, tu_1th_22,
  						&pat_var_28))
  					{
  						if(ff_match_tuple(pat_var_28,
  							{&tu_0th_29,
  							&tu_1th_30}))
  						{
  							l2_31 = tu_0th_29;
  							h2_32 = tu_1th_30;
  							app_res_36 =
  								ff_apply_generic(
  								max_10, l1_26);
  							app_res_35 =
  								ff_apply_generic(
  								app_res_36,
  								l2_31);
  							app_res_34 =
  								ff_apply_generic(
  								create_4,
  								app_res_35);
  							app_res_38 =
  								ff_apply_generic(
  								min_8, h1_27);
  							app_res_37 =
  								ff_apply_generic(
  								app_res_38,
  								h2_32);
  							app_res_33 =
  								ff_apply_generic(
  								app_res_34,
  								app_res_37);
  							match_res_11 =
  								app_res_33;
  							break;
  						}
  					}
  				}
  			}
  		}
  	}
  	while(0);
  	return match_res_11;
  }
  
  ff_obj_t min_10__fn(ff_fvs_t fvs_4, ff_obj_t x_3)
  {
  	ff_obj_t clos_5;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	greater_1 = fvs_4[0];
  	Endpoint_2 = fvs_4[1];
  	clos_5 = ff_make_closure({greater_1, Endpoint_2, x_3}, 3,
  		(ff_erased_fptr)min_11__fn);
  	return clos_5;
  }
  
  ff_obj_t min_11__fn(ff_fvs_t fvs_5, ff_obj_t y_4)
  {
  	ff_obj_t field_12;
  	ff_obj_t app_res_11;
  	ff_obj_t app_res_10;
  	ff_obj_t temp_9;
  	ff_obj_t app_res_8;
  	ff_obj_t app_res_7;
  	ff_obj_t ifel_res_6;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	ff_obj_t x_3;
  	greater_1 = fvs_5[0];
  	Endpoint_2 = fvs_5[1];
  	x_3 = fvs_5[2];
  	temp_9 = ff_make_int(0);
  	app_res_8 = ff_apply_generic(greater_1, temp_9);
  	field_12 = ff_get_member(Endpoint_2, "compare");
  	app_res_11 = ff_apply_generic(field_12, x_3);
  	app_res_10 = ff_apply_generic(app_res_11, y_4);
  	app_res_7 = ff_apply_generic(app_res_8, app_res_10);
  	if(!ff_is_zero(app_res_7))
  		ifel_res_6 = x_3;
  	else
  		ifel_res_6 = y_4;
  	return ifel_res_6;
  }
  
  ff_obj_t max_12__fn(ff_fvs_t fvs_4, ff_obj_t x_3)
  {
  	ff_obj_t clos_5;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	greater_1 = fvs_4[0];
  	Endpoint_2 = fvs_4[1];
  	clos_5 = ff_make_closure({greater_1, Endpoint_2, x_3}, 3,
  		(ff_erased_fptr)max_13__fn);
  	return clos_5;
  }
  
  ff_obj_t max_13__fn(ff_fvs_t fvs_5, ff_obj_t y_4)
  {
  	ff_obj_t temp_12;
  	ff_obj_t field_11;
  	ff_obj_t app_res_10;
  	ff_obj_t app_res_9;
  	ff_obj_t app_res_8;
  	ff_obj_t app_res_7;
  	ff_obj_t ifel_res_6;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	ff_obj_t x_3;
  	greater_1 = fvs_5[0];
  	Endpoint_2 = fvs_5[1];
  	x_3 = fvs_5[2];
  	field_11 = ff_get_member(Endpoint_2, "compare");
  	app_res_10 = ff_apply_generic(field_11, x_3);
  	app_res_9 = ff_apply_generic(app_res_10, y_4);
  	app_res_8 = ff_apply_generic(greater_1, app_res_9);
  	temp_12 = ff_make_int(0);
  	app_res_7 = ff_apply_generic(app_res_8, temp_12);
  	if(!ff_is_zero(app_res_7))
  		ifel_res_6 = x_3;
  	else
  		ifel_res_6 = y_4;
  	return ifel_res_6;
  }
  
  ff_obj_t contains_6__fn(ff_fvs_t fvs_2, ff_obj_t t_1)
  {
  	ff_obj_t clos_3;
  	clos_3 = ff_make_closure({t_1}, 1, (ff_erased_fptr)contains_7__fn);
  	return clos_3;
  }
  
  ff_obj_t contains_7__fn(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t temp_11;
  	ff_obj_t h_10;
  	ff_obj_t l_9;
  	ff_obj_t tu_1th_8;
  	ff_obj_t tu_0th_7;
  	ff_obj_t pat_var_6;
  	ff_obj_t temp_5;
  	ff_obj_t match_res_4;
  	ff_obj_t t_1;
  	t_1 = fvs_3[0];
  	do
  	{
  		if(ff_match_constr(1, t_1))
  		{
  			temp_5 = ff_make_int(0);
  			match_res_4 = temp_5;
  			break;
  		}
  		if(ff_match_constr(0, t_1, &pat_var_6))
  		{
  			if(ff_match_tuple(pat_var_6, {&tu_0th_7, &tu_1th_8}))
  			{
  				l_9 = tu_0th_7;
  				h_10 = tu_1th_8;
  				temp_11 = ff_make_int(1);
  				match_res_4 = temp_11;
  				break;
  			}
  		}
  	}
  	while(0);
  	return match_res_4;
  }
  
  ff_obj_t is_empty_5__fn(ff_fvs_t fvs_2, ff_obj_t interval_1)
  {
  	ff_obj_t temp_7;
  	ff_obj_t __6;
  	ff_obj_t pat_var_5;
  	ff_obj_t temp_4;
  	ff_obj_t match_res_3;
  	do
  	{
  		if(ff_match_constr(1, interval_1))
  		{
  			temp_4 = ff_make_int(1);
  			match_res_3 = temp_4;
  			break;
  		}
  		if(ff_match_constr(0, interval_1, &pat_var_5))
  		{
  			__6 = pat_var_5;
  			temp_7 = ff_make_int(0);
  			match_res_3 = temp_7;
  			break;
  		}
  	}
  	while(0);
  	return match_res_3;
  }
  
  ff_obj_t create_3__fn(ff_fvs_t fvs_4, ff_obj_t low_3)
  {
  	ff_obj_t clos_5;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	greater_1 = fvs_4[0];
  	Endpoint_2 = fvs_4[1];
  	clos_5 = ff_make_closure({greater_1, Endpoint_2, low_3}, 3,
  		(ff_erased_fptr)create_4__fn);
  	return clos_5;
  }
  
  ff_obj_t create_4__fn(ff_fvs_t fvs_5, ff_obj_t high_4)
  {
  	ff_obj_t tu_16;
  	ff_obj_t constr0_15;
  	ff_obj_t app_res_14;
  	ff_obj_t constr1_13;
  	ff_obj_t temp_12;
  	ff_obj_t field_11;
  	ff_obj_t app_res_10;
  	ff_obj_t app_res_9;
  	ff_obj_t app_res_8;
  	ff_obj_t app_res_7;
  	ff_obj_t ifel_res_6;
  	ff_obj_t greater_1;
  	ff_obj_t Endpoint_2;
  	ff_obj_t low_3;
  	greater_1 = fvs_5[0];
  	Endpoint_2 = fvs_5[1];
  	low_3 = fvs_5[2];
  	field_11 = ff_get_member(Endpoint_2, "compare");
  	app_res_10 = ff_apply_generic(field_11, low_3);
  	app_res_9 = ff_apply_generic(app_res_10, high_4);
  	app_res_8 = ff_apply_generic(greater_1, app_res_9);
  	temp_12 = ff_make_int(0);
  	app_res_7 = ff_apply_generic(app_res_8, temp_12);
  	if(!ff_is_zero(app_res_7))
  	{
  		constr1_13 = ff_make_constr_no_payload(1);
  		ifel_res_6 = constr1_13;
  	}
  	else
  	{
  		constr0_15 = ff_make_constr_payload(0);
  		tu_16 = ff_make_tuple({low_3, high_4}, 2);
  		app_res_14 = ff_apply_generic(constr0_15, tu_16);
  		ifel_res_6 = app_res_14;
  	}
  	return ifel_res_6;
  }
  
  
  int main()
  {
    test_rt();
    main_1__fn(nullptr);
  }

  $ $FF test_interval.fun
  $ ./test_interval.fun.out
  Hello Runtime
  low
  0
  high
  10
  low
  0
  high
  10


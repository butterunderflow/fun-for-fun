

  $ $FF test_match.fun

  $ cat test_match.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  ff_obj_t f_1__fn(ff_fvs_t fvs_3__l, ff_obj_t x_2__l);
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_40__l;
  	ff_obj_t m_39__l;
  	ff_obj_t app_res_38__l;
  	ff_obj_t b_37__l;
  	ff_obj_t a_36__l;
  	ff_obj_t tu_1th_35__l;
  	ff_obj_t tu_0th_34__l;
  	ff_obj_t match_res_33__l;
  	ff_obj_t tu_32__l;
  	ff_obj_t tu_31__l;
  	ff_obj_t pat_var_30__l;
  	ff_obj_t match_res_29__l;
  	ff_obj_t m_28__l;
  	ff_obj_t app_res_27__l;
  	ff_obj_t b_26__l;
  	ff_obj_t a_25__l;
  	ff_obj_t tu_1th_24__l;
  	ff_obj_t tu_0th_23__l;
  	ff_obj_t pat_var_22__l;
  	ff_obj_t match_res_21__l;
  	ff_obj_t n_20__l;
  	ff_obj_t tu_19__l;
  	ff_obj_t temp_18__l;
  	ff_obj_t temp_17__l;
  	ff_obj_t constr0_16__l;
  	ff_obj_t app_res_15__l;
  	ff_obj_t n_14__l;
  	ff_obj_t temp_13__l;
  	ff_obj_t constr0_12__l;
  	ff_obj_t app_res_11__l;
  	ff_obj_t app_res_10__l;
  	ff_obj_t n_9__l;
  	ff_obj_t constr1_8__l;
  	ff_obj_t app_res_7__l;
  	ff_obj_t f_6__l;
  	ff_obj_t clos_5__l;
  	ff_obj_t x_4__l;
  	ff_obj_t constr1_3__l;
  	ff_obj_t print_int_2__l;
  	print_int_2__l = ff_builtin_print_int;
  	constr1_3__l = ff_make_constr_no_payload(1);
  	x_4__l = constr1_3__l;
  	clos_5__l = ff_make_closure({print_int_2__l}, 1,
  		(ff_erased_fptr)f_1__fn);
  	f_6__l = clos_5__l;
  	constr1_8__l = ff_make_constr_no_payload(1);
  	app_res_7__l = ff_apply_generic(f_6__l, constr1_8__l);
  	n_9__l = app_res_7__l;
  	constr0_12__l = ff_make_constr_payload(0);
  	temp_13__l = ff_make_int(998);
  	app_res_11__l = ff_apply_generic(constr0_12__l, temp_13__l);
  	app_res_10__l = ff_apply_generic(f_6__l, app_res_11__l);
  	n_14__l = app_res_10__l;
  	constr0_16__l = ff_make_constr_payload(0);
  	temp_17__l = ff_make_int(10);
  	temp_18__l = ff_make_int(22);
  	tu_19__l = ff_make_tuple({temp_17__l, temp_18__l}, 2);
  	app_res_15__l = ff_apply_generic(constr0_16__l, tu_19__l);
  	n_20__l = app_res_15__l;
  	do
  	{
  		if(ff_match_constr(0, n_20__l, &pat_var_22__l))
  		{
  			if(ff_match_tuple(pat_var_22__l, {&tu_0th_23__l,
  				&tu_1th_24__l}))
  			{
  				a_25__l = tu_0th_23__l;
  				b_26__l = tu_1th_24__l;
  				app_res_27__l = ff_apply_generic(print_int_2__l,
  					a_25__l);
  				match_res_21__l = app_res_27__l;
  				break;
  			}
  		}
  	}
  	while(0);
  	m_28__l = match_res_21__l;
  	do
  	{
  		if(ff_match_constr(0, n_20__l, &pat_var_30__l))
  		{
  			tu_31__l = pat_var_30__l;
  			match_res_29__l = tu_31__l;
  			break;
  		}
  	}
  	while(0);
  	tu_32__l = match_res_29__l;
  	do
  	{
  		if(ff_match_tuple(tu_32__l, {&tu_0th_34__l, &tu_1th_35__l}))
  		{
  			a_36__l = tu_0th_34__l;
  			b_37__l = tu_1th_35__l;
  			app_res_38__l = ff_apply_generic(print_int_2__l,
  				b_37__l);
  			match_res_33__l = app_res_38__l;
  			break;
  		}
  	}
  	while(0);
  	m_39__l = match_res_33__l;
  	mod_40__l = ff_make_mod_obj(9, {"print_int", "x", "f", "n", "n", "n",
  		"m", "tu", "m"}, {print_int_2__l, x_4__l, f_6__l, n_9__l,
  		n_14__l, n_20__l, m_28__l, tu_32__l, m_39__l});
  	return mod_40__l;
  }
  
  ff_obj_t f_1__fn(ff_fvs_t fvs_3__l, ff_obj_t x_2__l)
  {
  	ff_obj_t temp_9__l;
  	ff_obj_t app_res_8__l;
  	ff_obj_t app_res_7__l;
  	ff_obj_t x_6__l;
  	ff_obj_t pat_var_5__l;
  	ff_obj_t match_res_4__l;
  	ff_obj_t print_int_1__l;
  	print_int_1__l = fvs_3__l[0];
  	do
  	{
  		if(ff_match_constr(0, x_2__l, &pat_var_5__l))
  		{
  			x_6__l = pat_var_5__l;
  			app_res_7__l = ff_apply_generic(print_int_1__l, x_6__l);
  			match_res_4__l = app_res_7__l;
  			break;
  		}
  		if(ff_match_constr(1, x_2__l))
  		{
  			temp_9__l = ff_make_int(1);
  			app_res_8__l = ff_apply_generic(print_int_1__l,
  				temp_9__l);
  			match_res_4__l = app_res_8__l;
  			break;
  		}
  	}
  	while(0);
  	return match_res_4__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }

  $ ./test_match.fun.out
  Hello Runtime
  19981022

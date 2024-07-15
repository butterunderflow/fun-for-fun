
  $ ff test_external.fun --stdout
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_3__l;
  	ff_obj_t add_2__l;
  	add_2__l = ff_add;
  	mod_3__l = ff_make_mod_obj(1, {"add"}, {add_2__l});
  	return mod_3__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }


  $ $FF test_add_external.fun --stdout

  $ cat test_add_external.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_11__l;
  	ff_obj_t z_10__l;
  	ff_obj_t app_res_9__l;
  	ff_obj_t y_8__l;
  	ff_obj_t temp_7__l;
  	ff_obj_t temp_6__l;
  	ff_obj_t app_res_5__l;
  	ff_obj_t app_res_4__l;
  	ff_obj_t print_int_3__l;
  	ff_obj_t add_2__l;
  	add_2__l = ff_builtin_add;
  	print_int_3__l = ff_builtin_print_int;
  	temp_6__l = ff_make_int(1);
  	app_res_5__l = ff_apply_generic(add_2__l, temp_6__l);
  	temp_7__l = ff_make_int(2);
  	app_res_4__l = ff_apply_generic(app_res_5__l, temp_7__l);
  	y_8__l = app_res_4__l;
  	app_res_9__l = ff_apply_generic(print_int_3__l, y_8__l);
  	z_10__l = app_res_9__l;
  	mod_11__l = ff_make_mod_obj(4, {"add", "print_int", "y", "z"},
  		{add_2__l, print_int_3__l, y_8__l, z_10__l});
  	return mod_11__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }

  $ ./test_add_external.fun.out
  Hello Runtime
  3


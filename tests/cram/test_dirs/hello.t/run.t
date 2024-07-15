
  $ $FF hello.fun

  $ ./hello.fun.out
  Hello Runtime
  Hello World
  I'm FF

  $ cat hello.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_7__l;
  	ff_obj_t y_6__l;
  	ff_obj_t app_res_5__l;
  	ff_obj_t x_4__l;
  	ff_obj_t temp_3__l;
  	ff_obj_t print_string_2__l;
  	print_string_2__l = ff_builtin_print_str;
  	temp_3__l = ff_make_str("Hello World\nI\'m FF");
  	x_4__l = temp_3__l;
  	app_res_5__l = ff_apply_generic(print_string_2__l, x_4__l);
  	y_6__l = app_res_5__l;
  	mod_7__l = ff_make_mod_obj(3, {"print_string", "x", "y"},
  		{print_string_2__l, x_4__l, y_6__l});
  	return mod_7__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }

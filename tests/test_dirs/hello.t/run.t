
  $ $FF hello.fun

  $ ./hello.fun.out
  Hello Runtime
  Hello World
  I'm FF

  $ cat hello.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1();
  
  ff_obj_t _ff_main_1()
  {
  	ff_obj_t mod_7;
  	ff_obj_t y_6;
  	ff_obj_t app_res_5;
  	ff_obj_t x_4;
  	ff_obj_t temp_3;
  	ff_obj_t print_string_2;
  	print_string_2 = ff_builtin_print_str;
  	temp_3 = ff_make_str("Hello World\nI\'m FF");
  	x_4 = temp_3;
  	app_res_5 = ff_apply_generic(print_string_2, x_4);
  	y_6 = app_res_5;
  	mod_7 = ff_make_mod_obj(3, {"print_string", "x", "y"}, {print_string_2,
  		x_4, y_6});
  	return mod_7;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1();
  }

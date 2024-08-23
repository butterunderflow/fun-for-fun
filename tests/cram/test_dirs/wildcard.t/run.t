
  $ $FF test_wildcard.fun

  $ cat test_wildcard.fun.cpp
  
  #include "fun_rt.hpp"
  #include <stdio.h>
  #include <stdexcept>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_9;
  	ff_obj_t __8;
  	ff_obj_t temp_7;
  	ff_obj_t app_res_6;
  	ff_obj_t __5;
  	ff_obj_t temp_4;
  	ff_obj_t app_res_3;
  	ff_obj_t print_string_2;
  	print_string_2 = ff_builtin_print_str;
  	temp_4 = ff_make_str("Hello I\'m here\n");
  	app_res_3 = ff_apply_generic(print_string_2, temp_4);
  	__5 = app_res_3;
  	temp_7 = ff_make_str("Good bye\n");
  	app_res_6 = ff_apply_generic(print_string_2, temp_7);
  	__8 = app_res_6;
  	mod_9 = ff_make_mod_obj(3, {"print_string", "_", "_"}, {print_string_2,
  		__5, __8});
  	return mod_9;
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

  $ ./test_wildcard.fun.out
  Hello I'm here
  Good bye



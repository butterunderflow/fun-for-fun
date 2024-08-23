
  $ ff test_external.fun --stdout
  
  #include "fun_rt.hpp"
  #include <stdio.h>
  #include <stdexcept>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_3;
  	ff_obj_t add_2;
  	add_2 = ff_add;
  	mod_3 = ff_make_mod_obj(1, {"add"}, {add_2});
  	return mod_3;
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


  $ $FF test_add_external.fun --stdout

  $ cat test_add_external.fun.cpp
  
  #include "fun_rt.hpp"
  #include <stdio.h>
  #include <stdexcept>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_11;
  	ff_obj_t z_10;
  	ff_obj_t app_res_9;
  	ff_obj_t y_8;
  	ff_obj_t temp_7;
  	ff_obj_t temp_6;
  	ff_obj_t app_res_5;
  	ff_obj_t app_res_4;
  	ff_obj_t print_int_3;
  	ff_obj_t add_2;
  	add_2 = ff_builtin_add;
  	print_int_3 = ff_builtin_print_int;
  	temp_6 = ff_make_int(1);
  	app_res_5 = ff_apply_generic(add_2, temp_6);
  	temp_7 = ff_make_int(2);
  	app_res_4 = ff_apply_generic(app_res_5, temp_7);
  	y_8 = app_res_4;
  	app_res_9 = ff_apply_generic(print_int_3, y_8);
  	z_10 = app_res_9;
  	mod_11 = ff_make_mod_obj(4, {"add", "print_int", "y", "z"}, {add_2,
  		print_int_3, y_8, z_10});
  	return mod_11;
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

  $ ./test_add_external.fun.out
  3


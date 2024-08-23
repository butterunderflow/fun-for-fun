

  $ ff test_assert.fun -o test_assert.cpp

  $ cat test_assert.cpp
  
  #include "fun_rt.hpp"
  #include <stdio.h>
  #include <stdexcept>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_12;
  	ff_obj_t x_11;
  	ff_obj_t is_true_10;
  	ff_obj_t temp_9;
  	ff_obj_t x_8;
  	ff_obj_t temp_7;
  	ff_obj_t app_res_6;
  	ff_obj_t x_5;
  	ff_obj_t is_true_4;
  	ff_obj_t temp_3;
  	ff_obj_t println_str_2;
  	println_str_2 = ff_builtin_println_str;
  	temp_3 = ff_make_int(1);
  	is_true_4 = ff_assert(temp_3);
  	x_5 = is_true_4;
  	temp_7 = ff_make_str("A true asserted!");
  	app_res_6 = ff_apply_generic(println_str_2, temp_7);
  	x_8 = app_res_6;
  	temp_9 = ff_make_int(0);
  	is_true_10 = ff_assert(temp_9);
  	x_11 = is_true_10;
  	mod_12 = ff_make_mod_obj(4, {"println_str", "x", "x", "x"},
  		{println_str_2, x_5, x_8, x_11});
  	return mod_12;
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

  $ $FF test_assert.fun

  $ ./test_assert.fun.out
  A true asserted!
  Runtime error: Assertion failed!


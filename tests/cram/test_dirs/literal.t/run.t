
  $ $FF test_literal.fun

  $ cat test_literal.fun.cpp
  
  #include "fun_rt.hpp"
  #include <stdio.h>
  #include <stdexcept>
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
  {
  	ff_obj_t mod_10;
  	ff_obj_t u_9;
  	ff_obj_t temp_8;
  	ff_obj_t m_7;
  	ff_obj_t temp_6;
  	ff_obj_t s_5;
  	ff_obj_t temp_4;
  	ff_obj_t x_3;
  	ff_obj_t temp_2;
  	temp_2 = ff_make_int(1);
  	x_3 = temp_2;
  	temp_4 = ff_make_str("hello \134n I\'m FF");
  	s_5 = temp_4;
  	temp_6 = ff_make_int(1);
  	m_7 = temp_6;
  	temp_8 = ff_make_int(0);
  	u_9 = temp_8;
  	mod_10 = ff_make_mod_obj(4, {"x", "s", "m", "u"}, {x_3, s_5, m_7, u_9});
  	return mod_10;
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



  $ $FF test_literal.fun

  $ cat test_literal.fun.cpp
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_10__l;
  	ff_obj_t u_9__l;
  	ff_obj_t temp_8__l;
  	ff_obj_t m_7__l;
  	ff_obj_t temp_6__l;
  	ff_obj_t s_5__l;
  	ff_obj_t temp_4__l;
  	ff_obj_t x_3__l;
  	ff_obj_t temp_2__l;
  	temp_2__l = ff_make_int(1);
  	x_3__l = temp_2__l;
  	temp_4__l = ff_make_str("hello \134n I\'m FF");
  	s_5__l = temp_4__l;
  	temp_6__l = ff_make_int(1);
  	m_7__l = temp_6__l;
  	temp_8__l = ff_make_int(0);
  	u_9__l = temp_8__l;
  	mod_10__l = ff_make_mod_obj(4, {"x", "s", "m", "u"}, {x_3__l, s_5__l,
  		m_7__l, u_9__l});
  	return mod_10__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }


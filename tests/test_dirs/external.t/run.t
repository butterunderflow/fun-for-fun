
  $ ff test_external.fun --stdout
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1();
  
  ff_obj_t _ff_main_1()
  {
  	ff_obj_t mod_3;
  	ff_obj_t add_2;
  	add_2 = ff_add;
  	mod_3 = ff_make_mod_obj(1, {"add"}, {add_2});
  	return mod_3;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1();
  }



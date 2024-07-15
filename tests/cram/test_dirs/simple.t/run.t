

  $ ff --help
  ff: a functional programming language build for fun
    -o Set output file name
    --version Show version number
    --debug Enable debug
    --stdout Output to stdout
    -help  Display this list of options
    --help  Display this list of options

  $ ff simple.fun
  $ ff simple.fun --stdout
  
  #include"fun_rt.hpp"
  #include<stdio.h>
  
  ff_obj_t _ff_main_1__fn();
  ff_obj_t m_3__fn(ff_fvs_t fvs_3__l, ff_obj_t x_2__l);
  ff_obj_t w_2__fn(ff_fvs_t fvs_2__l, ff_obj_t x_1__l);
  ff_obj_t z_1__fn(ff_fvs_t fvs_3__l, ff_obj_t x_2__l);
  
  ff_obj_t _ff_main_1__fn()
  {
  	ff_obj_t mod_12__l;
  	ff_obj_t m_11__l;
  	ff_obj_t clos_10__l;
  	ff_obj_t w_9__l;
  	ff_obj_t clos_8__l;
  	ff_obj_t z_7__l;
  	ff_obj_t clos_6__l;
  	ff_obj_t y_5__l;
  	ff_obj_t temp_4__l;
  	ff_obj_t x_3__l;
  	ff_obj_t temp_2__l;
  	temp_2__l = ff_make_int(1);
  	x_3__l = temp_2__l;
  	temp_4__l = ff_make_int(1);
  	y_5__l = temp_4__l;
  	clos_6__l = ff_make_closure({y_5__l}, 1, (ff_erased_fptr)z_1__fn);
  	z_7__l = clos_6__l;
  	clos_8__l = ff_make_closure({}, 0, (ff_erased_fptr)w_2__fn);
  	w_9__l = clos_8__l;
  	clos_10__l = ff_make_closure({w_9__l}, 1, (ff_erased_fptr)m_3__fn);
  	m_11__l = clos_10__l;
  	mod_12__l = ff_make_mod_obj(5, {"x", "y", "z", "w", "m"}, {x_3__l,
  		y_5__l, z_7__l, w_9__l, m_11__l});
  	return mod_12__l;
  }
  
  ff_obj_t m_3__fn(ff_fvs_t fvs_3__l, ff_obj_t x_2__l)
  {
  	ff_obj_t temp_5__l;
  	ff_obj_t app_res_4__l;
  	ff_obj_t w_1__l;
  	w_1__l = fvs_3__l[0];
  	temp_5__l = ff_make_int(1);
  	app_res_4__l = ff_apply_generic(w_1__l, temp_5__l);
  	return app_res_4__l;
  }
  
  ff_obj_t w_2__fn(ff_fvs_t fvs_2__l, ff_obj_t x_1__l)
  {
  	ff_obj_t temp_3__l;
  	temp_3__l = ff_make_int(0);
  	return temp_3__l;
  }
  
  ff_obj_t z_1__fn(ff_fvs_t fvs_3__l, ff_obj_t x_2__l)
  {
  	ff_obj_t y_1__l;
  	y_1__l = fvs_3__l[0];
  	return y_1__l;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1__fn();
  }

  $ ff simple.fun -o simple1.out --debug

  $ ls
  a.out
  fib.fun
  simple.fun
  simple1.out
  simple1.out.c_dbg
  simple1.out.closure
  simple1.out.lambda
  simple1.out.parsing
  simple1.out.typed


  $ cat simple1.out.closure
  Lifted main expression: 
  (EModObject
    ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
      (FSimple z (EClosure ((y) z/1))) (FSimple w (EClosure (() w/2)))
      (FSimple m (EClosure ((w) m/3)))))
  
  Global C functions: 
  (m/3 (w) (x) (EApp (EVar w) ((EConst (CInt 1)))))(w/2 () (x) (EConst (CInt 0)))(z/1 (y) (x) (EVar y))

  $ cat simple1.out.lambda
  (EModObject
    ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
      (FSimple z (ELam ((x) (EVar y) (y))))
      (FSimple w (ELam ((x) (EConst (CInt 0)) ())))
      (FSimple m (ELam ((x) (EApp (EVar w) ((EConst (CInt 1)))) (w))))))

  $ cat simple1.out.parsing
  ((TopLet x (EConst (CInt 1))) (TopLet y (EConst (CInt 1)))
    (TopLet z (ELam ((PBare x) (EVar y))))
    (TopLet w (ELam ((PBare x) (EConst (CInt 0)))))
    (TopLet m (ELam ((PBare x) (EApp (EVar w) (EConst (CInt 1)))))))

  $ cat simple1.out.typed
  
  
  let x = (1 is () 0.int)
  
  let y = (1 is () 0.int)
  
  let z = fun x ->
    (y is () 0.int)
  
  let w = fun x ->
    (0 is () 0.int)
  
  let m = fun x ->
    (w is ({() 0.int}
            ->() 0.int)) (1 is () 0.int)

  $ $FF simple.fun

  $ $FF fib.fun

  $ ./fib.fun.out
  Hello Runtime
  89


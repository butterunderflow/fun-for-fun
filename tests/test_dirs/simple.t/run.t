

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
  
  ff_obj_t _ff_main_1();
  ff_obj_t m_3(ff_fvs_t fvs_3, ff_obj_t x_2);
  ff_obj_t w_2(ff_fvs_t fvs_2, ff_obj_t x_1);
  ff_obj_t z_1(ff_fvs_t fvs_3, ff_obj_t x_2);
  
  ff_obj_t _ff_main_1()
  {
  	ff_obj_t mod_12;
  	ff_obj_t m_11;
  	ff_obj_t clos_10;
  	ff_obj_t w_9;
  	ff_obj_t clos_8;
  	ff_obj_t z_7;
  	ff_obj_t clos_6;
  	ff_obj_t y_5;
  	ff_obj_t temp_4;
  	ff_obj_t x_3;
  	ff_obj_t temp_2;
  	temp_2 = ff_make_int(1);
  	x_3 = temp_2;
  	temp_4 = ff_make_int(1);
  	y_5 = temp_4;
  	clos_6 = ff_make_closure((ff_obj_t[]){y_5}, 1, (ff_erased_fptr)z_1);
  	z_7 = clos_6;
  	clos_8 = ff_make_closure((ff_obj_t[]){}, 0, (ff_erased_fptr)w_2);
  	w_9 = clos_8;
  	clos_10 = ff_make_closure((ff_obj_t[]){w_9}, 1, (ff_erased_fptr)m_3);
  	m_11 = clos_10;
  	mod_12 = ff_make_mod_obj(5, (char const *[]){"x", "y", "z", "w", "m"},
  		(ff_obj_t[]){x_3, y_5, z_7, w_9, m_11});
  	return mod_12;
  }
  
  ff_obj_t m_3(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t temp_5;
  	ff_obj_t app_res_4;
  	ff_obj_t w_1;
  	w_1 = fvs_3[0];
  	temp_5 = ff_make_int(1);
  	app_res_4 = ff_apply_generic(w_1, temp_5);
  	return app_res_4;
  }
  
  ff_obj_t w_2(ff_fvs_t fvs_2, ff_obj_t x_1)
  {
  	ff_obj_t temp_3;
  	temp_3 = ff_make_int(0);
  	return temp_3;
  }
  
  ff_obj_t z_1(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t y_1;
  	y_1 = fvs_3[0];
  	return y_1;
  }
  
  
  int main()
  {
    test_rt();
    _ff_main_1();
  }

  $ ff simple.fun -o simple1.out --debug

  $ ls
  a.out
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


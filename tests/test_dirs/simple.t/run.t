

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
  
  #include<stdio.h>
  #include"fun_rt.h"
       
  ff_obj_t main_1()
  {
  	ff_obj_t mod_15;
  	ff_obj_t m_14;
  	ff_obj_t clos_12;
  	ff_obj_t w_11;
  	ff_obj_t clos_9;
  	ff_obj_t z_8;
  	ff_obj_t clos_6;
  	ff_obj_t y_5;
  	ff_obj_t temp_4;
  	ff_obj_t x_3;
  	ff_obj_t temp_2;
  	temp_2 = ff_make_int(1);
  	x_3 = temp_2;
  	temp_4 = ff_make_int(1);
  	y_5 = temp_4;
  	clos_6 = ff_make_closure((ff_obj_t[]){y_5}, z_1_7);
  	z_8 = clos_6;
  	clos_9 = ff_make_closure((ff_obj_t[]){}, w_2_10);
  	w_11 = clos_9;
  	clos_12 = ff_make_closure((ff_obj_t[]){w_11}, m_3_13);
  	m_14 = clos_12;
  	mod_15 = ff_make_mod_obj(5, (char const *[]){"x", "y", "z", "w", "m"},
  		(ff_obj_t[]){x_3, y_5, z_8, w_11, m_14});
  	return mod_15;
  }
  
  ff_obj_t m_3_2(ff_fvs_t fvs_4, ff_obj_t x_3)
  {
  	ff_obj_t temp_6;
  	ff_obj_t app_res_5;
  	ff_obj_t w_1;
  	w_1 = fvs_4[0];
  	temp_6 = ff_make_int(1);
  	app_res_5 = ff_apply(w_1, temp_6);
  	return app_res_5;
  }
  
  ff_obj_t w_2_1(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t temp_4;
  	temp_4 = ff_make_int(0);
  	return temp_4;
  }
  
  ff_obj_t z_1_2(ff_fvs_t fvs_4, ff_obj_t x_3)
  {
  	ff_obj_t y_1;
  	y_1 = fvs_4[0];
  	return y_1;
  }
  
  
  int main()
  {
    test_rt();
    main_1();
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
  (m/3 (w) x (EApp (EVar w) (EConst (CInt 1))))(w/2 () x (EConst (CInt 0)))(z/1 (y) x (EVar y))

  $ cat simple1.out.lambda
  (EModObject
    ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
      (FSimple z (ELam (x (EVar y) (y))))
      (FSimple w (ELam (x (EConst (CInt 0)) ())))
      (FSimple m (ELam (x (EApp (EVar w) (EConst (CInt 1))) (w))))))

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




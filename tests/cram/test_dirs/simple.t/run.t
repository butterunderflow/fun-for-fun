

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
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1);
  ff_obj_t m_4__fn(ff_fvs_t fvs_3, ff_obj_t x_2);
  ff_obj_t w_3__fn(ff_fvs_t fvs_2, ff_obj_t x_1);
  ff_obj_t z_2__fn(ff_fvs_t fvs_3, ff_obj_t x_2);
  
  ff_obj_t main_1__fn(ff_fvs_t fvs_1)
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
  	clos_6 = ff_make_closure({y_5}, 1, (ff_erased_fptr)z_2__fn);
  	z_7 = clos_6;
  	clos_8 = ff_make_closure({}, 0, (ff_erased_fptr)w_3__fn);
  	w_9 = clos_8;
  	clos_10 = ff_make_closure({w_9}, 1, (ff_erased_fptr)m_4__fn);
  	m_11 = clos_10;
  	mod_12 = ff_make_mod_obj(5, {"x", "y", "z", "w", "m"}, {x_3, y_5, z_7,
  		w_9, m_11});
  	return mod_12;
  }
  
  ff_obj_t m_4__fn(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t temp_5;
  	ff_obj_t app_res_4;
  	ff_obj_t w_1;
  	w_1 = fvs_3[0];
  	temp_5 = ff_make_int(1);
  	app_res_4 = ff_apply_generic(w_1, temp_5);
  	return app_res_4;
  }
  
  ff_obj_t w_3__fn(ff_fvs_t fvs_2, ff_obj_t x_1)
  {
  	ff_obj_t temp_3;
  	temp_3 = ff_make_int(0);
  	return temp_3;
  }
  
  ff_obj_t z_2__fn(ff_fvs_t fvs_3, ff_obj_t x_2)
  {
  	ff_obj_t y_1;
  	y_1 = fvs_3[0];
  	return y_1;
  }
  
  
  int main()
  {
    test_rt();
    main_1__fn(nullptr);
  }

  $ ff simple.fun -o simple1.out --debug

  $ ls
  a.out
  fib.fun
  simple.fun
  simple1.c_dbg
  simple1.closure
  simple1.lambda
  simple1.out
  simple1.parsing
  simple1.typed


  $ cat simple1.closure
  Main function: 
  main/1
  Global C functions: 
  (main/1 () ()
    (EModObject
      ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
        (FSimple z (EClosure ((y) z/2))) (FSimple w (EClosure (() w/3)))
        (FSimple m (EClosure ((w) m/4))))))(m/4 (w) (x) (EApp (EVar w) ((EConst (CInt 1)))))(w/3 () (x) (EConst (CInt 0)))(z/2 (y) (x) (EVar y))

  $ cat simple1.lambda
  (EModObject
    ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
      (FSimple z (ELam ((x) (EVar y) (y))))
      (FSimple w (ELam ((x) (EConst (CInt 0)) ())))
      (FSimple m (ELam ((x) (EApp (EVar w) ((EConst (CInt 1)))) (w))))))

  $ cat simple1.parsing
  ((TopLet x
     ((node (EConst (CInt 1)))
       (start_loc
         ((pos_fname simple.fun) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
       (end_loc ((pos_fname simple.fun) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
       (attrs ())))
    (TopLet y
      ((node (EConst (CInt 1)))
        (start_loc
          ((pos_fname simple.fun) (pos_lnum 3) (pos_bol 11) (pos_cnum 19)))
        (end_loc
          ((pos_fname simple.fun) (pos_lnum 3) (pos_bol 11) (pos_cnum 20)))
        (attrs ())))
    (TopLet z
      ((node
         (ELam
           ((PBare x)
             ((node (EVar y))
               (start_loc
                 ((pos_fname simple.fun) (pos_lnum 5) (pos_bol 22)
                   (pos_cnum 40)))
               (end_loc
                 ((pos_fname simple.fun) (pos_lnum 5) (pos_bol 22)
                   (pos_cnum 41)))
               (attrs ())))))
        (start_loc
          ((pos_fname simple.fun) (pos_lnum 5) (pos_bol 22) (pos_cnum 31)))
        (end_loc
          ((pos_fname simple.fun) (pos_lnum 5) (pos_bol 22) (pos_cnum 41)))
        (attrs ())))
    (TopLet w
      ((node
         (ELam
           ((PBare x)
             ((node (EConst (CInt 0)))
               (start_loc
                 ((pos_fname simple.fun) (pos_lnum 7) (pos_bol 44)
                   (pos_cnum 62)))
               (end_loc
                 ((pos_fname simple.fun) (pos_lnum 7) (pos_bol 44)
                   (pos_cnum 63)))
               (attrs ())))))
        (start_loc
          ((pos_fname simple.fun) (pos_lnum 7) (pos_bol 44) (pos_cnum 53)))
        (end_loc
          ((pos_fname simple.fun) (pos_lnum 7) (pos_bol 44) (pos_cnum 63)))
        (attrs ())))
    (TopLet m
      ((node
         (ELam
           ((PBare x)
             ((node
                (EApp
                  ((node (EVar w))
                    (start_loc
                      ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66)
                        (pos_cnum 84)))
                    (end_loc
                      ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66)
                        (pos_cnum 85)))
                    (attrs ()))
                  ((node (EConst (CInt 1)))
                    (start_loc
                      ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66)
                        (pos_cnum 86)))
                    (end_loc
                      ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66)
                        (pos_cnum 87)))
                    (attrs ()))))
               (start_loc
                 ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66)
                   (pos_cnum 84)))
               (end_loc
                 ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66)
                   (pos_cnum 87)))
               (attrs ())))))
        (start_loc
          ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66) (pos_cnum 75)))
        (end_loc
          ((pos_fname simple.fun) (pos_lnum 9) (pos_bol 66) (pos_cnum 87)))
        (attrs ()))))

  $ cat simple1.typed
  
  
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


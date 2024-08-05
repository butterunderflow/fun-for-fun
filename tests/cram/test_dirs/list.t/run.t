
  $ ff test_list.fun -o test_list --debug

  $ cat test_list.typed 
  
  
  external println_int : (() int
                           ->() unit)= "ff_builtin_println_int"
  
  external println_string : (() string
                              ->() unit)= "ff_builtin_println_str"
  
  external add : (() int
                   ->(() int
                       ->() int))= "ff_builtin_add"
  
  type ('a/0) list = 
  | Cons of (['a/0]
              * (['a/0]) list)
  | Nil
  
  let cons = fun x ->
    fun y ->
      (Cons[0] is (({{'_t/1}}
                     * ({{'_t/1}}) list)
                    ->({{'_t/1}}) list)) ((x is {'_t/1}), (y is {({{'_t/1}})
                                                                 list}))
  
  let lst = (cons is ({() int}
                       ->(({() int}) list
                           ->({() int}) list))) (3 is () int) (cons is (
                                                               {() int}
                                                                ->(({() int})
                                                                   list
                                                                    ->({
                                                                      () int})
                                                                      list))) (
  2 is () int) (cons is ({() int}
                          ->(({() int}) list
                              ->({() int}) list))) (1 is () int) (Nil[1] is 
                                                                  ({() int})
                                                                  list)
  
  let rec iter = fun lst -> 
                   fun f ->
                     match (lst is {({{'_t/20}}) list}) with
                     | (Cons[0]
                     ((x is {'_t/20})
                       * (lst is {({{'_t/20}}) list}))) ->
                     (f is {({'_t/20}
                              ->{() unit})}) (x is {'_t/20}) ;
                     (iter is {(({'_t/20}) list
                                 ->{(({'_t/20}
                                       ->() unit)
                                      ->{{() unit}})})}) (lst is ({'_t/20})
                                                                 list) (
                     f is ({'_t/20}
                            ->() unit))
                     | Nil -> (() is () unit)
  
  let print_int_lst = fun lst ->
    (iter is (({() int}) list
               ->(({() int}
                    ->() unit)
                   ->() unit))) (lst is {({() int}) list}) (println_int is (
                                                            () int
                                                             ->() unit))
  
  let result = (print_int_lst is ((() int) list
                                   ->() unit)) (lst is (() int) list)
  
  let rec map = fun lst -> 
                  fun f ->
                    match (lst is {({{'_t/36}}) list}) with
                    | (Cons[0] ((x is {'_t/36})
                                 * (lst is {({{'_t/36}}) list}))) ->
                    (cons is ({{'ret/39}}
                               ->(({{'ret/39}}) list
                                   ->({{'ret/39}}) list))) (f is {({'_t/36}
                                                                    ->{'ret/39})}) (
                    x is {'_t/36}) (map is {(({'_t/36}) list
                                              ->{(({'_t/36}
                                                    ->{'ret/39})
                                                   ->{({{'ret/39}}) list})})}) (
                    lst is ({'_t/36}) list) (f is ({'_t/36}
                                                    ->{'ret/39}))
                    | Nil -> (Nil[1] is ({{'ret/39}}) list)
  
  let result = (print_int_lst is ((() int) list
                                   ->() unit)) (map is (({() int}) list
                                                         ->(({() int}
                                                              ->{() int})
                                                             ->({() int}) list))) (
  lst is (() int) list) (add is (() int
                                  ->(() int
                                      ->() int))) (7 is () int)
  
  let lst = (cons is ({(() int
                         * () int)}
                       ->(({(() int
                              * () int)}) list
                           ->({(() int
                                 * () int)}) list))) ((2 is () int), (4 is 
                                                                      () int)) (
  cons is ({(() int
              * () int)}
            ->(({(() int
                   * () int)}) list
                ->({(() int
                      * () int)}) list))) ((1 is () int), (2 is () int)) (
  Nil[1] is ({(() int
                * () int)}) list)
  
  let print_int_tu = fun tu ->
    match (tu is {({() int}
                    * {() int})}) with
    | ((x is {() int})
        * (y is {() int})) ->
    (println_int is (() int
                      ->() unit)) (x is {() int}) ;
    (println_int is (() int
                      ->() unit)) (y is {() int})
  
  let print_int_tu_lst = fun lst ->
    (iter is (({(() int
                  * () int)}) list
               ->(({(() int
                      * () int)}
                    ->() unit)
                   ->() unit))) (lst is {({(() int
                                             * () int)}) list}) (print_int_tu is (
                                                                 (() int
                                                                   * () int)
                                                                  ->() unit))
  
  let result = (print_int_tu_lst is (((() int
                                        * () int)) list
                                      ->() unit)) (lst is ((() int
                                                             * () int))
                                                          list)
  
  let tu_adder = fun tu ->
    match (tu is {({() int}
                    * {() int})}) with
    | ((x is {() int})
        * (y is {() int})) ->
    ((add is (() int
               ->(() int
                   ->() int))) (x is {() int}) (1 is () int), (add is (
                                                               () int
                                                                ->(() int
                                                                    ->() int))) (
    y is {() int}) (1 is () int))
  
  let result = (print_int_tu_lst is (((() int
                                        * () int)) list
                                      ->() unit)) (map is (({(() int
                                                               * () int)})
                                                           list
                                                            ->(({(() int
                                                                   * () int)}
                                                                 ->{(() int
                                                                      * 
                                                                      () int)})
                                                                ->({(() int
                                                                      * 
                                                                      () int)})
                                                                  list))) (
  lst is ((() int
            * () int)) list) (tu_adder is ((() int
                                             * () int)
                                            ->(() int
                                                * () int)))

  $ $FF test_list.fun

  $ ./test_list.fun.out
  3
  2
  1
  10
  9
  8
  2
  4
  1
  2
  3
  5
  2
  3


  $ ff test_list.fun -o test_list --debug

  $ cat test_list.typed 
  
  
  external println_int : (() 0.int
                           ->() 0.unit)= "ff_builtin_println_int"
  
  external println_string : (() 0.string
                              ->() 0.unit)= "ff_builtin_println_str"
  
  external add : (() 0.int
                   ->(() 0.int
                       ->() 0.int))= "ff_builtin_add"
  
  type ('a/0) list = 
  | Cons of (['a/0]
              * (['a/0]) 0.list)
  | Nil
  
  let cons = fun x ->
    fun y ->
      (Cons[0] is (({{'_t/1}}
                     * ({{'_t/1}}) 0.list)
                    ->({{'_t/1}}) 0.list)) ((x is {'_t/1}), (y is {({{'_t/1}})
                                                                   0.list}))
  
  let lst = (cons is ({() 0.int}
                       ->(({() 0.int}) 0.list
                           ->({() 0.int}) 0.list))) (3 is () 0.int) (cons is (
                                                                     {() 0.int}
                                                                      ->(
                                                                      ({
                                                                      () 0.int})
                                                                      0.list
                                                                      ->
                                                                      ({
                                                                      () 0.int})
                                                                      0.list))) (
  2 is () 0.int) (cons is ({() 0.int}
                            ->(({() 0.int}) 0.list
                                ->({() 0.int}) 0.list))) (1 is () 0.int) (
  Nil[1] is ({() 0.int}) 0.list)
  
  let rec iter = fun lst -> 
                   fun f ->
                     match (lst is {({{'_t/20}}) 0.list}) with
                     | (Cons[0]
                     ((x is {'_t/20})
                       * (lst is {({{'_t/20}}) 0.list}))) ->
                     (f is {({'_t/20}
                              ->{() 0.unit})}) (x is {'_t/20}) ;
                     (iter is {(({'_t/20}) 0.list
                                 ->{(({'_t/20}
                                       ->() 0.unit)
                                      ->{{() 0.unit}})})}) (lst is ({'_t/20})
                                                                   0.list) (
                     f is ({'_t/20}
                            ->() 0.unit))
                     | Nil -> (() is () 0.unit)
  
  let print_int_lst = fun lst ->
    (iter is (({() 0.int}) 0.list
               ->(({() 0.int}
                    ->() 0.unit)
                   ->() 0.unit))) (lst is {({() 0.int}) 0.list}) (println_int is (
                                                                  () 0.int
                                                                   ->() 0.unit))
  
  let result = (print_int_lst is ((() 0.int) 0.list
                                   ->() 0.unit)) (lst is (() 0.int) 0.list)
  
  let rec map = fun lst -> 
                  fun f ->
                    match (lst is {({{'_t/36}}) 0.list}) with
                    | (Cons[0]
                    ((x is {'_t/36})
                      * (lst is {({{'_t/36}}) 0.list}))) ->
                    (cons is ({{'ret/39}}
                               ->(({{'ret/39}}) 0.list
                                   ->({{'ret/39}}) 0.list))) (f is {({'_t/36}
                                                                      ->{'ret/39})}) (
                    x is {'_t/36}) (map is {(({'_t/36}) 0.list
                                              ->{(({'_t/36}
                                                    ->{'ret/39})
                                                   ->{({{'ret/39}}) 0.list})})}) (
                    lst is ({'_t/36}) 0.list) (f is ({'_t/36}
                                                      ->{'ret/39}))
                    | Nil -> (Nil[1] is ({{'ret/39}}) 0.list)
  
  let result = (print_int_lst is ((() 0.int) 0.list
                                   ->() 0.unit)) (map is (({() 0.int}) 0.list
                                                           ->(({() 0.int}
                                                                ->{() 0.int})
                                                               ->({() 0.int})
                                                                 0.list))) (
  lst is (() 0.int) 0.list) (add is (() 0.int
                                      ->(() 0.int
                                          ->() 0.int))) (7 is () 0.int)
  
  let lst = (cons is ({(() 0.int
                         * () 0.int)}
                       ->(({(() 0.int
                              * () 0.int)}) 0.list
                           ->({(() 0.int
                                 * () 0.int)}) 0.list))) ((2 is () 0.int), (
  4 is () 0.int)) (cons is ({(() 0.int
                               * () 0.int)}
                             ->(({(() 0.int
                                    * () 0.int)}) 0.list
                                 ->({(() 0.int
                                       * () 0.int)}) 0.list))) ((1 is () 0.int), (
  2 is () 0.int)) (Nil[1] is ({(() 0.int
                                 * () 0.int)}) 0.list)
  
  let print_int_tu = fun tu ->
    match (tu is {({() 0.int}
                    * {() 0.int})}) with
    | ((x is {() 0.int})
        * (y is {() 0.int})) ->
    (println_int is (() 0.int
                      ->() 0.unit)) (x is {() 0.int}) ;
    (println_int is (() 0.int
                      ->() 0.unit)) (y is {() 0.int})
  
  let print_int_tu_lst = fun lst ->
    (iter is (({(() 0.int
                  * () 0.int)}) 0.list
               ->(({(() 0.int
                      * () 0.int)}
                    ->() 0.unit)
                   ->() 0.unit))) (lst is {({(() 0.int
                                               * () 0.int)}) 0.list}) (
    print_int_tu is ((() 0.int
                       * () 0.int)
                      ->() 0.unit))
  
  let result = (print_int_tu_lst is (((() 0.int
                                        * () 0.int)) 0.list
                                      ->() 0.unit)) (lst is ((() 0.int
                                                               * () 0.int))
                                                            0.list)
  
  let tu_adder = fun tu ->
    match (tu is {({() 0.int}
                    * {() 0.int})}) with
    | ((x is {() 0.int})
        * (y is {() 0.int})) ->
    ((add is (() 0.int
               ->(() 0.int
                   ->() 0.int))) (x is {() 0.int}) (1 is () 0.int), (add is (
                                                                     () 0.int
                                                                      ->(
                                                                      () 0.int
                                                                      ->
                                                                      () 0.int))) (
    y is {() 0.int}) (1 is () 0.int))
  
  let result = (print_int_tu_lst is (((() 0.int
                                        * () 0.int)) 0.list
                                      ->() 0.unit)) (map is (({(() 0.int
                                                                 * () 0.int)})
                                                             0.list
                                                              ->(({(() 0.int
                                                                     * 
                                                                     () 0.int)}
                                                                   ->{(
                                                                   () 0.int
                                                                    * () 0.int)})
                                                                  ->({(
                                                                    () 0.int
                                                                     * 
                                                                     () 0.int)})
                                                                    0.list))) (
  lst is ((() 0.int
            * () 0.int)) 0.list) (tu_adder is ((() 0.int
                                                 * () 0.int)
                                                ->(() 0.int
                                                    * () 0.int)))

  $ $FF test_list.fun

  $ ./test_list.fun.out
  Hello Runtime
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

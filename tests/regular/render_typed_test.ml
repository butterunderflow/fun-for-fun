open Syntax.Parsing
open Typing
module DefaultPP = Render.MakePP (Render.ShowAllConfig)

let%expect_test "Test: pretty print typed expression" =
  let print_typed str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed = Check.check_expr e (Env.init ()) in
    let fmt = Format.std_formatter in
    DefaultPP.pp_expr fmt typed;
    Format.pp_print_flush fmt ()
  in
  print_typed "1";
  [%expect {| (1 is () int) |}];
  print_typed {|let x = 1 in x|};
  [%expect {|
    let x = (1 is () int)
    in (x is () int)
    |}];

  print_typed {|fun x -> let y = x in y |};
  [%expect
    {|
    fun x ->
      let y = (x is {'_t/1})
      in (y is {'_t/1})
    |}];

  print_typed {| if true then 1 else 2 |};
  [%expect
    {|
    if
      (true is () bool)
    then
      (1 is () int)
    else
      (2 is () int)
    |}];

  print_typed {| (1, 2, 3,4) |};
  [%expect
    {| ((1 is () int), (2 is () int), (3 is () int), (4 is () int)) |}];

  print_typed
    {|
               let rec  f = fun x -> x
               and y = fun x -> f 1
               in
               f 1|};
  [%expect
    {|
    let rec f = fun x ->
                  (x is {() int})
    and y = fun x ->
              (f is ({() int}
                      ->{() int})) (1 is () int)
    in (f is (() int
               ->() int)) (1 is () int)
    |}]

let%expect_test "Test: pretty print typed program" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Typing.Tools.type_check_program prog in
    let fmt = Format.std_formatter in
    DefaultPP.pp_prog fmt typed;
    Format.pp_print_flush fmt ()
  in

  print_typed
    {|
     type () int_l 
     = Cons of int
     | Nil 
           let co = (Cons 1)

     let c = Nil

     let f =
         match c with
         | Cons x -> x
         | Nil    -> 0
|};
  [%expect
    {|
    type () int_l =
    | Cons of () int
    | Nil

    let co = (Cons[0] is (() int
                           ->() int_l)) (1 is () int)

    let c = (Nil[1] is () int_l)

    let f = match (c is () int_l) with
            | (Cons[0] (x is () int)) -> (x is () int)
            | Nil -> (0 is () int)
    |}];

  print_typed
    {|
     module M =
       struct
         type () t = Nil


         let x = Nil
       end
     let c = M.x
     |};
  [%expect
    {|
    module M =
      (struct

         type () t =
         | Nil

         let x = (Nil[0] is () 1.t)

       end is sig

                id = 1

                val x : () 1.t

                constr Nil[0] : () 1.t

                type () t =
                | Nil

                Owned Modules = {
                }

              end)

    let c = (M.x is () 1.t)
    |}];
  print_typed
    {|
     module M =
       struct
         type () t = Nil

         let x = Nil
         module N =
           struct
             type () t = Nil

           end

         let z = N.Nil
       end


     let c = M.x
     let x = M.N.Nil
     let y = M.z
     |};
  [%expect
    {|
    module M =
      (struct

         type () t =
         | Nil

         let x = (Nil[0] is () 1.t)

         module N =
           (struct

              type () t =
              | Nil

            end is sig

                     id = 2

                     constr Nil[0] : () 2.t

                     type () t =
                     | Nil

                     Owned Modules = {
                     }

                   end)

         let z = (N.Nil[0] is () 2.t)

       end is sig

                id = 1

                val z : () 2.t

                val x : () 1.t

                constr Nil[0] : () 1.t

                type () t =
                | Nil

                module N : sig

                             id = 2

                             constr Nil[0] : () 2.t

                             type () t =
                             | Nil

                             Owned Modules = {
                             }

                           end

                Owned Modules = {
                  2 ;
                }

              end)

    let c = (M.x is () 1.t)

    let x = (M.N.Nil[0] is () 2.t)

    let y = (M.z is () 2.t)
    |}];

  print_typed
    {|
     module M = 
     struct 
       type () t = Nil

       let x = Nil
     end :
     sig
       type () t
       val x : t
     end 
     |};
  [%expect
    {|
    module M =
      (((struct

           type () t =
           | Nil

           let x = (Nil[0] is () 1.t)

         end is sig

                  id = 1

                  val x : () 1.t

                  constr Nil[0] : () 1.t

                  type () t =
                  | Nil

                  Owned Modules = {
                  }

                end) : sig

                         id = 2

                         val x : () 2.t

                         type () t

                         Owned Modules = {
                         }

                       end) is sig

                                 id = 3

                                 val x : () 3.t

                                 type () t

                                 Owned Modules = {
                                 }

                               end)
    |}];

  print_typed
    {|
     module type MIntf = 
     sig 
       type () t

       val x : t
     end 

     module MImpl = 
     struct 
       type () t = Nil

       let z = 1       

       let x = Nil
     end : MIntf

     |};
  [%expect
    {|
    module type MIntf =
      sig

        id = 1

        val x : () 1.t

        type () t

        Owned Modules = {
        }

      end

    module MImpl =
      (((struct

           type () t =
           | Nil

           let z = (1 is () int)

           let x = (Nil[0] is () 2.t)

         end is sig

                  id = 2

                  val x : () 2.t

                  val z : () int

                  constr Nil[0] : () 2.t

                  type () t =
                  | Nil

                  Owned Modules = {
                  }

                end) : sig

                         id = 1

                         val x : () 1.t

                         type () t

                         Owned Modules = {
                         }

                       end) is sig

                                 id = 3

                                 val x : () 3.t

                                 type () t

                                 Owned Modules = {
                                 }

                               end)
    |}];

  print_typed
    {|
     module type I = sig
        val x : int

        val y : int
     end

     module type J = sig
        val x : int

        val y : int

        val z : int
     end

module MJ = struct
  let x = 1

  let y = 1

  let z = 1
end

module Simple = struct
  let x = 1

  let y = 2

end

module M =
functor
  (MI : functor (MI : I) -> I)
  ->
  struct
    module K = MI (Simple)
    module K2 = MI (MJ)
  end

module F =
functor
  (MI : I)
  ->
  (
    struct
      let x = 1

      let y = 1

      let z = 1

      let w = 2
    end :
      J)


module MMM = (M(F).K : I)
     |};
  [%expect
    {|
    module type I =
      sig

        id = 1

        val y : () int

        val x : () int

        Owned Modules = {
        }

      end

    module type J =
      sig

        id = 2

        val z : () int

        val y : () int

        val x : () int

        Owned Modules = {
        }

      end

    module MJ =
      (struct

         let x = (1 is () int)

         let y = (1 is () int)

         let z = (1 is () int)

       end is sig

                id = 3

                val z : () int

                val y : () int

                val x : () int

                Owned Modules = {
                }

              end)

    module Simple =
      (struct

         let x = (1 is () int)

         let y = (2 is () int)

       end is sig

                id = 4

                val y : () int

                val x : () int

                Owned Modules = {
                }

              end)

    module M =
      functor (MI : functor (_ : sig

                                   id = 1

                                   val y : () int

                                   val x : () int

                                   Owned Modules = {
                                   }

                                 end)
                      ->
                      sig

                        id = 1

                        val y : () int

                        val x : () int

                        Owned Modules = {
                        }

                      end)
                      ->
                      (struct

                         module K =
                           (MI(Simple) is sig

                                            id = 6

                                            val y : () int

                                            val x : () int

                                            Owned Modules = {
                                            }

                                          end)

                         module K2 =
                           (MI(MJ) is sig

                                        id = 7

                                        val y : () int

                                        val x : () int

                                        Owned Modules = {
                                        }

                                      end)

                       end is sig

                                id = 5

                                module K2 : sig

                                              id = 7

                                              val y : () int

                                              val x : () int

                                              Owned Modules = {
                                              }

                                            end

                                module K : sig

                                             id = 6

                                             val y : () int

                                             val x : () int

                                             Owned Modules = {
                                             }

                                           end

                                Owned Modules = {
                                  7 ;
                                  6 ;
                                }

                              end)

      module F =
        functor (MI : sig

                        id = 1

                        val y : () int

                        val x : () int

                        Owned Modules = {
                        }

          end)
          ->
          (((struct

               let x = (1 is () int)

               let y = (1 is () int)

               let z = (1 is () int)

               let w = (2 is () int)

             end is sig

                      id = 8

                      val w : () int

                      val z : () int

                      val y : () int

                      val x : () int

                      Owned Modules = {
                      }

                    end) : sig

                             id = 2

                             val z : () int

                             val y : () int

                             val x : () int

                             Owned Modules = {
                             }

                           end) is sig

                                     id = 9

                                     val z : () int

                                     val y : () int

                                     val x : () int

                                     Owned Modules = {
                                     }

                                   end)

      module MMM =
        (((M(F) is sig

                     id = 10

                     module K2 : sig

                                   id = 11

                                   val y : () int

                                   val x : () int

                                   Owned Modules = {
                                   }

                                 end

                     module K : sig

                                  id = 12

                                  val y : () int

                                  val x : () int

                                  Owned Modules = {
                                  }

                                end

                     Owned Modules = {
                       11 ;
                       12 ;
                     }

                   end).K : sig

                              id = 1

                              val y : () int

                              val x : () int

                              Owned Modules = {
                              }

                            end) is sig

                                      id = 13

                                      val y : () int

                                      val x : () int

                                      Owned Modules = {
                                      }

                                    end)
    |}];

  print_typed
    {|
     module M = struct
       type () x = | Nil
     end

     module N = struct
       type t = M.x

       type n = t
     end

     type y = N.n
|};
  [%expect
    {|
    module M =
      (struct

         type () x =
         | Nil

       end is sig

                id = 1

                constr Nil[0] : () 1.x

                type () x =
                | Nil

                Owned Modules = {
                }

              end)

    module N =
      (struct

         type t = () 1.x

         type n = () 1.x

       end is sig

                id = 2

                type n = () 1.x

                type t = () 1.x

                Owned Modules = {
                }

              end)

    type y = () 1.x
    |}];

  print_typed {| let x = ();();();1 |};
  [%expect
    {|
    let x = (() is () unit) ;
            (() is () unit) ;
            (() is () unit) ;
            (1 is () int)
    |}];

  (* constructor accessed by path *)
  print_typed
    {|
               module C = struct
                 type () t =
                 | Nil
                 | Integer of int

               end

               let c = C.Nil
               
               let result = match c with
                            | C.Nil -> ()
                            | C.Integer i -> ()
               |};
  [%expect
    {|
    module C =
      (struct

         type () t =
         | Nil
         | Integer of () int

       end is sig

                id = 1

                constr Nil[0] : () 1.t

                constr Integer[1] : (() int
                                      ->() 1.t)

                type () t =
                | Nil
                | Integer of () int

                Owned Modules = {
                }

              end)

    let c = (C.Nil[0] is () 1.t)

    let result = match (c is () 1.t) with
                 | Nil -> (() is () unit)
                 | (Integer[1] (i is () int)) -> (() is () unit)
    |}];

  print_typed
    {|
               type () list =
               | Nil
               | Cons of int * int list

               type ('a, 'b) tu =
               | Tu of ('b * 'a)

               let x = Tu (1, "c")

               let result1 = match x with
                            | Tu (a, b) -> a

               let result2 = match x with
                            | Tu (a, b) -> b
               |};
  [%expect
    {|
    type () list =
    | Nil
    | Cons of (() int
                * (() int) list)

    type ('a/0, 'b/0) tu =
    | Tu of (['b/0]
              * ['a/0])

    let x = (Tu[0] is (({() int}
                         * {() string})
                        ->({() string}, {() int}) tu)) ((1 is () int), (""c"" is
                                                                        () string))

    let result1 = match (x is (() string, () int) tu) with
                  | (Tu[0] ((a is {() int})
                             * (b is {() string}))) -> (a is () int)

    let result2 = match (x is (() string, () int) tu) with
                  | (Tu[0] ((a is {() int})
                             * (b is {() string}))) -> (b is () string)
    |}];

  print_typed
    {|
               module type I0 =
               sig
                 type () t
                 
                 val x : t
               end

               module type I1 = sig
                 type () t
                 
                 type () s

                 val x : t
               end

               module F = functor(F1: functor (X:I1) -> I0) -> struct
               end

               module N = functor(MI0: I0) -> struct
                 type t = int
                 type s = t
                 let x = 1
               end

               module M = F(N)
               |};
  [%expect
    {|
    module type I0 =
      sig

        id = 1

        val x : () 1.t

        type () t

        Owned Modules = {
        }

      end

    module type I1 =
      sig

        id = 2

        val x : () 2.t

        type () s

        type () t

        Owned Modules = {
        }

      end

    module F =
      functor (F1 : functor (_ : sig

                                   id = 2

                                   val x : () 2.t

                                   type () s

                                   type () t

                                   Owned Modules = {
                                   }

                                 end)
                      ->
                      sig

                        id = 1

                        val x : () 1.t

                        type () t

                        Owned Modules = {
                        }

                      end)
                      ->
                      (struct

                       end is sig

                                id = 3

                                Owned Modules = {
                                }

                              end)

      module N =
        functor (MI0 : sig

                         id = 1

                         val x : () 1.t

                         type () t

                         Owned Modules = {
                         }

          end)
          ->
          (struct

             type t = () int

             type s = () int

             let x = (1 is () int)

           end is sig

                    id = 4

                    val x : () int

                    type s = () int

                    type t = () int

                    Owned Modules = {
                    }

                  end)

      module M =
        (F(N) is sig

                   id = 5

                   Owned Modules = {
                   }

                 end)
    |}];
  print_typed
    {|
               module type I0 =
               sig
               end


               module N = functor(MI0: I0) -> struct
                 module K = functor(MI0: I0) -> struct
                 end
               end

               module A = struct
               end

               module L = N(A)

               |};
  [%expect
    {|
    module type I0 =
      sig

        id = 1

        Owned Modules = {
        }

      end

    module N =
      functor (MI0 : sig

                       id = 1

                       Owned Modules = {
                       }

        end)
        ->
        (struct

           module K =
             functor (MI0 : sig

                              id = 1

                              Owned Modules = {
                              }

               end)
               ->
               (struct

                end is sig

                         id = 3

                         Owned Modules = {
                         }

                       end)

         end is sig

                  id = 2

                  module K : functor (_ : sig

                                            id = 1

                                            Owned Modules = {
                                            }

                                          end)
                               ->
                               sig

                                 id = 3

                                 Owned Modules = {
                                 }

                               end

                             Owned Modules = {
                               3 ;
                             }

                  end)

      module A =
        (struct

         end is sig

                  id = 4

                  Owned Modules = {
                  }

                end)

      module L =
        (N(A) is sig

                   id = 5

                   module K : functor (_ : sig

                                             id = 1

                                             Owned Modules = {
                                             }

                                           end)
                                ->
                                sig

                                  id = 6

                                  Owned Modules = {
                                  }

                                end

                              Owned Modules = {
                                6 ;
                              }

                   end)
    |}];
  print_typed
    {|
     module type M = sig
       type () t
     end

     module F = functor (X: M) -> struct
       type k = X.t
       type t = k
     end

     module N = F(struct type t = int end)
     |};
  [%expect
    {|
    module type M =
      sig

        id = 1

        type () t

        Owned Modules = {
        }

      end

    module F =
      functor (X : sig

                     id = 1

                     type () t

                     Owned Modules = {
                     }

        end)
        ->
        (struct

           type k = () 1.t

           type t = () 1.t

         end is sig

                  id = 2

                  type t = () 1.t

                  type k = () 1.t

                  Owned Modules = {
                  }

                end)

    module N =
      (F((struct

            type t = () int

          end is sig

                   id = 3

                   type t = () int

                   Owned Modules = {
                   }

                 end)) is sig

                            id = 4

                            type t = () int

                            type k = () int

                            Owned Modules = {
                            }

                          end)
     |}];
  print_typed
    {|
     module type M = sig
       type () t
     end


     module F = functor (X: M) -> (struct
       type t = X.t

       type m = t

       type n = | Nil of X.t
     end : sig
       type () t

       type m = X.t

       type n = | Nil of X.t
     end)
        
     module N = F(struct type t = int end)
     |};
  [%expect
    {|
    module type M =
      sig

        id = 1

        type () t

        Owned Modules = {
        }

      end

    module F =
      functor (X : sig

                     id = 1

                     type () t

                     Owned Modules = {
                     }

        end)
        ->
        (((struct

             type t = () 1.t

             type m = () 1.t

             type () n =
             | Nil of () 1.t

           end is sig

                    id = 2

                    constr Nil[0] : (() 1.t
                                      ->() 2.n)

                    type () n =
                    | Nil of () 1.t

                    type m = () 1.t

                    type t = () 1.t

                    Owned Modules = {
                    }

                  end) : sig

                           id = 3

                           constr Nil[0] : (() 1.t
                                             ->() 3.n)

                           type () n =
                           | Nil of () 1.t

                           type m = () 1.t

                           type () t

                           Owned Modules = {
                           }

                         end) is sig

                                   id = 4

                                   constr Nil[0] : (() 1.t
                                                     ->() 4.n)

                                   type () n =
                                   | Nil of () 1.t

                                   type m = () 1.t

                                   type () t

                                   Owned Modules = {
                                   }

                                 end)

    module N =
      (F((struct

            type t = () int

          end is sig

                   id = 5

                   type t = () int

                   Owned Modules = {
                   }

                 end)) is sig

                            id = 6

                            constr Nil[0] : (() int
                                              ->() 6.n)

                            type () n =
                            | Nil of () int

                            type m = () int

                            type () t

                            Owned Modules = {
                            }

                          end)
    |}];
  print_typed
    {|
     module type T =  sig
       val id : 'b -> 'b
     end

     module F = (struct
       let id = fun x -> x
     end : T)
     |};
  [%expect
    {|
    module type T =
      sig

        id = 1

        val id : forall '_t/1 . (['_t/1]
                                  ->['_t/1])

        Owned Modules = {
        }

      end

    module F =
      (((struct

           let id = fun x ->
             (x is {'_t/2})

         end is sig

                  id = 2

                  val id : forall '_t/2 . (['_t/2]
                                            ->['_t/2])

                  Owned Modules = {
                  }

                end) : sig

                         id = 1

                         val id : forall '_t/1 . (['_t/1]
                                                   ->['_t/1])

                         Owned Modules = {
                         }

                       end) is sig

                                 id = 3

                                 val id : forall '_t/1 . (['_t/1]
                                                           ->['_t/1])

                                 Owned Modules = {
                                 }

                               end)
    |}];
  print_typed
    {|

external greater : int -> int -> bool = "ff_builtin_greater"

module type Cmp = sig
  type () t

  val compare : t -> t -> bool
end

type 'a lst =
  | Cons of ('a * 'a lst)
  | Nil

module Sorting = functor (C : Cmp) -> struct
   let cmp = fun l -> fun r ->
     match (l, r) with
     | (Cons (h1, _), Cons (h2, _)) -> 
       C.compare h1 h2
end

module Int = struct
  type t = int

  let compare = greater
end

module ListIntSorting = Sorting (Int)
              |};
  [%expect {|
    external greater : (() int
                         ->(() int
                             ->() bool))= "ff_builtin_greater"

    module type Cmp =
      sig

        id = 1

        val compare : (() 1.t
                        ->(() 1.t
                            ->() bool))

        type () t

        Owned Modules = {
        }

      end

    type ('a/0) lst =
    | Cons of (['a/0]
                * (['a/0]) lst)
    | Nil

    module Sorting =
      functor (C : sig

                     id = 1

                     val compare : (() 1.t
                                     ->(() 1.t
                                         ->() bool))

                     type () t

                     Owned Modules = {
                     }

        end)
        ->
        (struct

           let cmp = fun l ->
             fun r ->
               match ((l is {{({{() 1.t}}) lst}}), (r is {{({{() 1.t}}) lst}})) with
               | ((Cons[0]
                   ((h1 is {() 1.t})
                     * (_ is {({{() 1.t}}) lst})))
                   * (Cons[0]
                   ((h2 is {() 1.t})
                     * (_ is {({{() 1.t}}) lst})))) ->
               (C.compare is (() 1.t
                               ->(() 1.t
                                   ->() bool))) (h1 is {() 1.t}) (h2 is {() 1.t})

         end is sig

                  id = 2

                  val cmp : ((() 1.t) lst
                              ->((() 1.t) lst
                                  ->() bool))

                  Owned Modules = {
                  }

                end)

    module Int =
      (struct

         type t = () int

         let compare = (greater is (() int
                                     ->(() int
                                         ->() bool)))

       end is sig

                id = 3

                val compare : (() int
                                ->(() int
                                    ->() bool))

                type t = () int

                Owned Modules = {
                }

              end)

    module ListIntSorting =
      (Sorting(Int) is sig

                         id = 4

                         val cmp : ((() 3.t) lst
                                     ->((() 3.t) lst
                                         ->() bool))

                         Owned Modules = {
                         }

                       end)
    |}]

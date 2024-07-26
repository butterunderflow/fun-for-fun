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
  [%expect {| (1 is () 0.int) |}];
  print_typed {|let x = 1 in x|};
  [%expect {|
    let x = (1 is () 0.int)
    in (x is () 0.int) |}];

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
      (true is () 0.bool)
    then
      (1 is () 0.int)
    else
      (2 is () 0.int) |}];

  print_typed {| (1, 2, 3,4) |};
  [%expect
    {|
    ((1 is () 0.int), (2 is () 0.int), (3 is () 0.int), (4 is () 0.int)) |}];

  print_typed
    {|
               let rec  f = fun x -> x
               and y = fun x -> f 1
               in
               f 1|};
  [%expect
    {|
    let rec f = fun x ->
                  (x is {() 0.int})
    and y = fun x ->
              (f is ({() 0.int}
                      ->{() 0.int})) (1 is () 0.int)
    in (f is (() 0.int
               ->() 0.int)) (1 is () 0.int) |}]

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
    | Cons of () 0.int
    | Nil

    let co = (Cons[0] is (() 0.int
                           ->() 0.int_l)) (1 is () 0.int)

    let c = (Nil[1] is () 0.int_l)

    let f = match (c is () 0.int_l) with
            | (Cons[0] (x is () 0.int)) -> (x is () 0.int)
            | Nil -> (0 is () 0.int) |}];

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

           let z = (1 is () 0.int)

           let x = (Nil[0] is () 2.t)

         end is sig

                  id = 2

                  val x : () 2.t

                  val z : () 0.int

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

        val y : () 0.int

        val x : () 0.int

        Owned Modules = {
        }

      end

    module type J =
      sig

        id = 2

        val z : () 0.int

        val y : () 0.int

        val x : () 0.int

        Owned Modules = {
        }

      end

    module MJ =
      (struct

         let x = (1 is () 0.int)

         let y = (1 is () 0.int)

         let z = (1 is () 0.int)

       end is sig

                id = 3

                val z : () 0.int

                val y : () 0.int

                val x : () 0.int

                Owned Modules = {
                }

              end)

    module Simple =
      (struct

         let x = (1 is () 0.int)

         let y = (2 is () 0.int)

       end is sig

                id = 4

                val y : () 0.int

                val x : () 0.int

                Owned Modules = {
                }

              end)

    module M =
      functor (MI : functor (_ : sig

                                   id = 1

                                   val y : () 0.int

                                   val x : () 0.int

                                   Owned Modules = {
                                   }

                                 end)
                      ->
                      sig

                        id = 1

                        val y : () 0.int

                        val x : () 0.int

                        Owned Modules = {
                        }

                      end)
                      ->
                      (struct

                         module K =
                           (MI(Simple) is sig

                                            id = 6

                                            val y : () 0.int

                                            val x : () 0.int

                                            Owned Modules = {
                                            }

                                          end)

                         module K2 =
                           (MI(MJ) is sig

                                        id = 7

                                        val y : () 0.int

                                        val x : () 0.int

                                        Owned Modules = {
                                        }

                                      end)

                       end is sig

                                id = 5

                                module K2 : sig

                                              id = 7

                                              val y : () 0.int

                                              val x : () 0.int

                                              Owned Modules = {
                                              }

                                            end

                                module K : sig

                                             id = 6

                                             val y : () 0.int

                                             val x : () 0.int

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

                        val y : () 0.int

                        val x : () 0.int

                        Owned Modules = {
                        }

          end)
          ->
          (((struct

               let x = (1 is () 0.int)

               let y = (1 is () 0.int)

               let z = (1 is () 0.int)

               let w = (2 is () 0.int)

             end is sig

                      id = 8

                      val w : () 0.int

                      val z : () 0.int

                      val y : () 0.int

                      val x : () 0.int

                      Owned Modules = {
                      }

                    end) : sig

                             id = 2

                             val z : () 0.int

                             val y : () 0.int

                             val x : () 0.int

                             Owned Modules = {
                             }

                           end) is sig

                                     id = 9

                                     val z : () 0.int

                                     val y : () 0.int

                                     val x : () 0.int

                                     Owned Modules = {
                                     }

                                   end)

      module MMM =
        (((M(F) is sig

                     id = 10

                     module K2 : sig

                                   id = 11

                                   val y : () 0.int

                                   val x : () 0.int

                                   Owned Modules = {
                                   }

                                 end

                     module K : sig

                                  id = 12

                                  val y : () 0.int

                                  val x : () 0.int

                                  Owned Modules = {
                                  }

                                end

                     Owned Modules = {
                       11 ;
                       12 ;
                     }

                   end).K : sig

                              id = 1

                              val y : () 0.int

                              val x : () 0.int

                              Owned Modules = {
                              }

                            end) is sig

                                      id = 13

                                      val y : () 0.int

                                      val x : () 0.int

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
    let x = (() is () 0.unit) ;
            (() is () 0.unit) ;
            (() is () 0.unit) ;
            (1 is () 0.int)
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
         | Integer of () 0.int

       end is sig

                id = 1

                constr Nil[0] : () 1.t

                constr Integer[1] : (() 0.int
                                      ->() 1.t)

                type () t =
                | Nil
                | Integer of () 0.int

                Owned Modules = {
                }

              end)

    let c = (C.Nil[0] is () 1.t)

    let result = match (c is () 1.t) with
                 | Nil -> (() is () 0.unit)
                 | (Integer[1] (i is () 0.int)) -> (() is () 0.unit)
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
    | Cons of (() 0.int
                * (() 0.int) 0.list)

    type ('a/0, 'b/0) tu =
    | Tu of (['b/0]
              * ['a/0])

    let x = (Tu[0] is (({() 0.int}
                         * {() 0.string})
                        ->({() 0.string}, {() 0.int}) 0.tu)) ((1 is () 0.int), (
    ""c"" is () 0.string))

    let result1 = match (x is (() 0.string, () 0.int) 0.tu) with
                  | (Tu[0] ((a is {() 0.int})
                             * (b is {() 0.string}))) -> (a is () 0.int)

    let result2 = match (x is (() 0.string, () 0.int) 0.tu) with
                  | (Tu[0] ((a is {() 0.int})
                             * (b is {() 0.string}))) -> (b is () 0.string)
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

             type t = () 0.int

             type s = () 0.int

             let x = (1 is () 0.int)

           end is sig

                    id = 4

                    val x : () 0.int

                    type s = () 0.int

                    type t = () 0.int

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
    |}]

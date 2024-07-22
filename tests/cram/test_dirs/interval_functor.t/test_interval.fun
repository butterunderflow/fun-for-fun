external greater : int -> int -> bool = "ff_builtin_minus"

external compare : int -> int -> int = "ff_builtin_minus"

module type Comparable = sig
  type () t

  val compare : t -> t -> int
end

module Make_interval = functor(Endpoint : Comparable) -> struct

  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  let create = (fun low -> fun high ->
    if greater (Endpoint.compare low high) 0 then Empty
    else Interval (low,high))

 let is_empty = (fun interval ->
    match interval with
    | Empty -> true
    | Interval _ -> false )

  let contains = (fun t -> fun x ->
    match t with
    | Empty -> false
    | Interval (l,h) -> true)

  let intersect = (fun t1 -> fun t2 ->
    let min = fun x -> fun y -> if greater 0 (Endpoint.compare x y) then x else y in
    let max = fun x -> fun y -> if greater (Endpoint.compare x y) 0 then x else y in
    match (t1,t2) with
    | (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (Interval (l1,h1), Interval (l2,h2)) ->
        create (max l1 l2) (min h1 h2))
end

module Int = struct
  type t = int

  let compare = compare
end


module Int_interval = Make_interval(Int)

let i_0_10 = Int_interval.create 0 10


external add : int -> int -> int = "ff_builtin_add"

external println_int : int ->  int = "ff_builtin_println_int"

module type X_int = sig val x : int end

module Increment = functor (M : X_int) -> (struct
  let x = add M.x 1
end : X_int)

module One = struct let x = 1 end

let _ = println_int One.x

module Two = Increment (One)

let _ = println_int Two.x

module Three = Increment (Two)

let _ = println_int Three.x

module Four = Increment (Three)

let _ = println_int Four.x

module Three_and_more = struct
  let x = 3
  let y = "three"
end

module Four_ = Increment(Three_and_more)

let _ = println_int Four_.x

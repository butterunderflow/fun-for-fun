open Sexplib.Conv
module S = Sexplib.Sexp

let compare_string = String.compare

let compare_int = Int.compare

type t = int * string [@@deriving compare]

let sexp_of_t t = S.Atom (Printf.sprintf "%s/%d" (snd t) (fst t))

let t_of_sexp s =
  match s with
  | S.Atom s ->
      let splited = String.split_on_char '/' s in
      (int_of_string (List.nth splited 1), List.nth splited 0)
  | _ -> failwith "never reach"

let index = ref 0

let same x y = fst x = fst y && snd x = snd y

let from name = (0, name)

let create ~(hint : string) =
  index := !index + 1;
  (!index, hint)

let rename (_, name) = create ~hint:name

let to_string x = Printf.sprintf "%s/%d" (snd x) (fst x)

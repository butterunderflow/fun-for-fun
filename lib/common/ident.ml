open Sexplib.Conv
module S = Sexplib.Sexp

let compare_string = String.compare

let compare_int = Int.compare

type ident = int * string [@@deriving sexp, compare, show]

type t = ident

let compare (index0, name0) (index1, name1) =
  let index_cmp = compare_int index0 index1 in
  if index_cmp <> 0 then index_cmp else compare_string name0 name1

let sexp_of_ident id = S.Atom (Printf.sprintf "%s/%d" (snd id) (fst id))

let name_of_ident (_index, name) = name

let index_of_ident (index, _name) = index

let mk_ident index name = (index, name)

let ident_of_sexp s =
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

let refresh () = index := 0

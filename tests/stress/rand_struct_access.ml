type tree =
  | Leaf of (string * int)
  | Node of (string * tree list)

let leaf name x = Leaf (name, x)

let node name ts = Node (name, ts)

let tree_gen =
  QCheck.Gen.(
    let ident_len = int_range 10 15 in
    let rec sized_tree (n : int) : tree t =
      match n with
      | 0 ->
          map2 leaf (string_size ~gen:(char_range 'a' 'z') ident_len) big_nat
      | n ->
          frequency
            [
              ( 1,
                map2 leaf
                  (string_size ~gen:(char_range 'a' 'z') ident_len)
                  nat );
              (* a small list of tree *)
              ( 2,
                map2 node
                  (string_size ~gen:(char_range 'A' 'Z') ident_len)
                  (list_size (int_bound 10) (sized_tree (n - 1))) );
            ]
    in
    sized_size (int_bound 5) sized_tree)

let tree_to_prog_str tree =
  let buf = Buffer.create 500 in
  let fmt = Format.formatter_of_buffer buf in
  let rec go tree =
    match tree with
    | Leaf (name, value) -> Format.fprintf fmt "let %s = %d" name value
    | Node (name, ts) ->
        Format.fprintf fmt "@[@[<v 2>module %s = struct" name;
        List.iter
          (fun t ->
            Format.fprintf fmt "@\n";
            go t)
          ts;
        Format.fprintf fmt "@]@\nend@]"
  in
  go tree;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let accesses_of_tree tree =
  let accesses = ref [] in
  let rec go path tree =
    match tree with
    | Leaf (name, value) -> accesses := (name :: path, value) :: !accesses
    | Node (name, ts) -> List.iter (go (name :: path)) ts
  in
  go [] tree;
  List.(split (rev !accesses))

let string_of_accesses accesses =
  let buf = Buffer.create 500 in
  List.iter
    (fun path ->
      Printf.bprintf buf "let _ = println_int %s\n"
        (String.concat "." List.(rev path)))
    accesses;
  Buffer.contents buf

let test_case_of_tree tree =
  let accesses, expect = accesses_of_tree tree in
  ( {|external println_int : int -> unit = "ff_builtin_println_int"|}
    ^ "\n"
    ^ tree_to_prog_str tree
    ^ "\n"
    ^ string_of_accesses accesses,
    expect )

let rec print_tree = function
  | Leaf (name, i) -> Printf.sprintf "Leaf(%s, %d) " name i
  | Node (name, ts) ->
      "Node ("
      ^ name
      ^ ", "
      ^ String.concat ", " (List.map print_tree ts)
      ^ ")"

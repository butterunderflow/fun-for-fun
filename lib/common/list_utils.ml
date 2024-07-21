let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let remove_from_left xs = List.rev (List.fold_left cons_uniq [] xs)

let fold_left_first f xs =
  match xs with
  | x :: xs -> List.fold_left f x xs
  | [] -> assert false

let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let remove_from_left xs = List.rev (List.fold_left cons_uniq [] xs)

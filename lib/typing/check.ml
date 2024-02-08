[@@@warning "-27"] 

type ty = Syntax.Parsetree.type_expr

let dealias (t : ty) env : ty = raise Not_found

let normalize t env = dealias t env

let eq (t1 : ty) (t2 : ty) env =
  if t1 == t2 then true else normalize t1 env = normalize t2 env

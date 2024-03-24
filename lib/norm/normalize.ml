module T = Syntax.Parsetree
module Ut = Syntax.Utils

type env_t = {
  mod_aliases : (string * T.path) list; (* aliases of module path *)
  val_aliases : (string * (T.path * string)) list;
      (* aliases of module level value *)
  ty_aliases : (string * (T.path * string)) list;
  (* aliases of module level type *)
  current : T.path;
      (* normalized path to reach module currently transform, it's PRoot when
         in the root module *)
  locals : string list; (* values created by local binding expression *)
}

let get_mod_path name env = List.assoc name env.mod_aliases

let get_val_path name env = List.assoc name env.val_aliases

let get_ty_path name env = List.assoc name env.ty_aliases

let exist_local name env =
  if not (List.mem name env.locals) then
    failwith (Printf.sprintf "name %s not exist" name)

let get_ty_path name env = List.assoc name env.val_aliases

(* make current scope's value binding's normal form *)
let mk_current_vbn (env : env_t) (name : string) =
  T.EField (env.current, name)

let initial_env =
  {
    mod_aliases = [];
    val_aliases = [];
    ty_aliases = [];
    current = T.PRoot;
    locals = [];
  }

let get_mod_path (name : string) (env : env_t) : T.path =
  match env with
  | { mod_aliases; _ } -> List.assoc name mod_aliases

let rec norm_prog (tr : T.program) (env : env_t) =
  match tr with
  | [] -> []
  | top :: others ->
      let top_normed, env = norm_top_level top env in
      top_normed :: norm_prog others env

and norm_top_level top env : T.top_level * env_t =
  match top with
  | T.TopLet (x, e) -> norm_let x e env
  | T.TopLetRec binds -> norm_top_let_rec binds env
  | T.TopTypeDef ty_def -> norm_top_type_def ty_def env
  | T.TopMod (mn, me) -> norm_top_mod mn me env
  | T.TopModRec mbinds -> norm_mod_rec mbinds env

and norm_let p e env : T.top_level * env_t =
  let e_normed = norm_expr e env in
  let bindings = Ut.get_all_pat_vars p in
  let env =
    {
      env with
      val_aliases =
        List.map (fun x -> (x, (env.current, x))) bindings @ env.val_aliases;
    }
  in
  (T.TopLet (p, e_normed), env)

and norm_top_let_rec binds env : T.top_level * env_t =
  let names = List.map fst binds in
  let lambdas = List.map fst binds in
  let env = { env with locals = names @ env.locals } in
  let lambdas_normed = List.map (fun l -> norm_lambda l env) lambdas in
  (T.TopLetRec (List.combine names lambdas_normed), env)

and norm_top_type_def ty_def env : T.top_level * env_t =
  match ty_def with
  | T.TDAdt (name, ty_args, variants) ->
      let env =
        {
          env with
          ty_aliases = (name, (env.current, name)) :: env.ty_aliases;
        }
      in
      let variants = List.map (fun v -> norm_variant v env) variants in
      (T.TopTypeDef (T.TDAdt (name, ty_args, variants)), env)
  | T.TDRecord (_, _, _) -> failwith "todo"

and norm_variant variant env =
  match variant with
  | cons, te -> (cons, Option.map (fun te -> norm_type_expr te env) te)

and norm_top_mod mn me env =
  let me_normed = norm_mod_expr me env in
  let env =
    {
      env with
      mod_aliases = (mn, T.PMem (env.current, mn)) :: env.mod_aliases;
    }
  in
  (T.TopMod (mn, me_normed), env)

and norm_mod_expr me env : T.mod_expr =
  match me with
  | T.MERoot -> me
  | T.MEName mn ->
      let path = get_mod_path mn env in
      let rec mod_of_path path =
        (* path is included in module expression *)
        match path with
        | T.PRoot -> T.MERoot
        | T.PName n -> T.MEName n
        | T.PMem (p0, n) -> T.MEField (p0, n)
        | T.PApply (p0, ms) -> T.MEApply (mod_of_path p0, mod_of_path ms)
      in
      mod_of_path path
  | T.MEStruct tops -> T.MEStruct (norm_prog tops env)
  | T.MEFunctor ((x, mt), me) ->
      let body_env =
        { env with mod_aliases = (x, T.PName x) :: env.mod_aliases }
      in
      T.MEFunctor ((x, norm_mod_ty mt env), norm_mod_expr me body_env)
  | T.MEField (p, x) -> T.MEField (norm_path p env, x)
  | T.MEApply (m0, m1) ->
      T.MEApply (norm_mod_expr m0 env, norm_mod_expr m1 env)
  | T.MERestrict (me, mt) ->
      T.MERestrict (norm_mod_expr me env, norm_mod_ty mt env)

and norm_mod_ty mt env = failwith "todo"

and norm_mod_rec mbinds env = failwith "todo"

and norm_expr e env = failwith "todo"

and norm_lambda e env = failwith "todo"

and norm_path (p : T.path) (env : env_t) =
  match p with
  | T.PRoot -> T.PRoot (* todo: no syntax for root module access *)
  | T.PName name -> get_mod_path name env
  | T.PMem (p0, name) -> T.PMem (norm_path p0 env, name)
  | T.PApply (p0, p1) -> T.PApply (norm_path p0 env, norm_path p1 env)

and norm_type_expr (te : T.type_expr) (env : env_t) : T.type_expr =
  failwith "todo"

let transfrom (prog : T.program) : T.program = norm_prog prog initial_env

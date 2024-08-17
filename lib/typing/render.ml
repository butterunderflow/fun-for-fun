open Typedtree
module I = Types_in
module Fmt = Format

module type PPConfig = sig
  val show_const_ty : bool

  val show_bind_ty : bool

  val show_mod_ty : bool
end

module MakePP (Config : PPConfig) = struct
  let rec pp_expr (fmt : Format.formatter) (e : expr) =
    match e with
    | Exp_const (Const_bool b, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_bool fmt b)
          ty
    | Exp_const (Const_int i, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_int fmt i)
          ty
    | Exp_const (Const_string s, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_string fmt (Printf.sprintf "\"%s\"" s))
          ty
    | Exp_const (Const_unit, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_string fmt "()")
          ty
    | Exp_var (x, ty) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ -> Fmt.pp_print_string fmt x)
          ty
    | Exp_let (x, e0, e1, _) ->
        Fmt.fprintf fmt "@[<hv>let %s = " x;
        pp_expr fmt e0;
        Fmt.fprintf fmt "@\n";
        Fmt.fprintf fmt "in";
        Fmt.fprintf fmt "@ ";
        Fmt.fprintf fmt "@[<hov 2>";
        pp_expr fmt e1;
        Fmt.fprintf fmt "@]";
        Fmt.fprintf fmt "@]"
    | Exp_letrec (lams, body, _) ->
        (match lams with
        | [] -> failwith "neverreach"
        | (x, lam) :: tail ->
            Fmt.fprintf fmt "@[<v>";
            Fmt.fprintf fmt "let rec %s = @[<hov 2>" x;
            pp_lam fmt lam;
            Fmt.fprintf fmt "@]";
            List.iter
              (fun (x, lam) ->
                Fmt.fprintf fmt "@\nand %s = @[" x;
                pp_lam fmt lam;
                Fmt.fprintf fmt "@]")
              tail);
        Fmt.fprintf fmt "@\nin@[<hov>@ ";
        pp_expr fmt body;
        Fmt.fprintf fmt "@]";
        Fmt.fprintf fmt "@]"
    | Exp_lam (x, e, _) ->
        Fmt.fprintf fmt "fun %s ->" x;
        Fmt.fprintf fmt "@\n";
        Fmt.fprintf fmt "  ";
        Fmt.fprintf fmt "@[";
        pp_expr fmt e;
        Fmt.fprintf fmt "@]"
    | Exp_if (e0, e1, e2, _) ->
        Fmt.fprintf fmt "@[<v>";
        Fmt.fprintf fmt "if@\n";
        Fmt.fprintf fmt "  @[";
        pp_expr fmt e0;
        Fmt.fprintf fmt "@]";
        Fmt.fprintf fmt "@\nthen@\n";
        Fmt.fprintf fmt "  @[";
        pp_expr fmt e1;
        Fmt.fprintf fmt "@]";
        Fmt.fprintf fmt "@\nelse@\n";
        Fmt.fprintf fmt "  @[";
        pp_expr fmt e2;
        Fmt.fprintf fmt "@]"
    | Exp_case (cond, branches, _) ->
        Fmt.fprintf fmt "@[<hov>";
        Fmt.fprintf fmt "match ";
        pp_expr fmt cond;
        Fmt.fprintf fmt " with";
        List.iter
          (fun (p, e) ->
            Fmt.fprintf fmt "@\n| ";
            pp_pattern fmt p;
            Fmt.fprintf fmt "@ ->@ ";
            pp_expr fmt e)
          branches;
        Fmt.fprintf fmt "@]"
    | Exp_app (op, arg, _) ->
        pp_expr fmt op;
        Fmt.fprintf fmt " ";
        pp_expr fmt arg
    | Exp_ann (e, te) ->
        Fmt.fprintf fmt "@[<v>";
        pp_expr fmt e;
        Fmt.fprintf fmt ":@[";
        pp_ty fmt te;
        Fmt.fprintf fmt "@]"
    | Exp_tuple (es, _) ->
        let size = List.length es in
        Fmt.fprintf fmt "(";
        List.iteri
          (fun i e ->
            pp_expr fmt e;
            if i = size - 1 then Fmt.fprintf fmt ")"
            else Fmt.fprintf fmt ", ")
          es
    | Exp_field (me, name, te) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ ->
            Fmt.fprintf fmt "@[";
            pp_mod fmt me;
            Fmt.fprintf fmt ".%s" name;
            Fmt.fprintf fmt "@]")
          te
    | Exp_field_constr (me, name, id, te) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ ->
            Fmt.fprintf fmt "@[";
            pp_mod fmt me;
            Fmt.fprintf fmt ".%s[%d]" name id;
            Fmt.fprintf fmt "@]")
          te
    | Exp_constr (c, id, te) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ -> Fmt.fprintf fmt "%s[%d]" c id)
          te
    | Exp_cmp (op, e0, e1, te) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ ->
            pp_expr fmt e0;
            (match op with
            | T.Eq -> Fmt.fprintf fmt " = "
            | T.Neq -> Fmt.fprintf fmt " <> ");
            pp_expr fmt e1)
          te
    | Exp_seq (e0, e1, _te) ->
        Fmt.fprintf fmt "@[<v>";
        pp_expr fmt e0;
        Fmt.fprintf fmt " ;@\n";
        pp_expr fmt e1;
        Fmt.fprintf fmt "@]"

  and pp_lam fmt (x, e, _te) =
    Fmt.fprintf fmt "@[<v 2>fun %s -> @\n" x;
    pp_expr fmt e;
    Fmt.fprintf fmt "@]"

  and pp_is_ty fmt config content_printer ty =
    if config then (
      Fmt.fprintf fmt "(@[";
      content_printer ();
      Fmt.pp_print_string fmt " is ";
      pp_ty fmt ty;
      Fmt.fprintf fmt "@])")
    else content_printer ()

  and pp_is_mod_ty fmt config content_printer ty =
    if config then (
      Fmt.fprintf fmt "(@[";
      content_printer ();
      Fmt.pp_print_string fmt " is ";
      Fmt.fprintf fmt "@[";
      pp_mod_ty fmt ty;
      Fmt.fprintf fmt "@]@])")
    else content_printer ()

  and pp_mod fmt ?(env : Env.t option) me =
    match me with
    | Mod_name (name, _) -> Fmt.pp_print_string fmt name
    | Mod_struct (tops, mt) ->
        pp_is_mod_ty fmt Config.show_mod_ty
          (fun _ ->
            Fmt.fprintf fmt "@[<v 2>struct";
            List.iter
              (fun top ->
                Fmt.fprintf fmt "@\n@\n";
                pp_top fmt top)
              tops;
            Fmt.fprintf fmt "@]@\n@\nend")
          mt
    | Mod_functor ((name, mt), me) ->
        Fmt.fprintf fmt "@[<v 2>functor (%s : " name;
        pp_mod_ty fmt mt;
        Fmt.fprintf fmt ")@\n-> @\n";
        pp_mod fmt ?env me;
        Fmt.fprintf fmt "@]"
    | Mod_field (me, name, _) ->
        Fmt.fprintf fmt "@[";
        pp_mod fmt ?env me;
        Fmt.fprintf fmt ".%s" name;
        Fmt.fprintf fmt "@]"
    | Mod_apply (me0, me1, mt) ->
        pp_is_mod_ty fmt Config.show_mod_ty
          (fun _ ->
            pp_mod fmt ?env me0;
            Fmt.fprintf fmt "(";
            pp_mod fmt ?env me1;
            Fmt.fprintf fmt ")")
          mt
    | Mod_restrict (me, mt, mt') ->
        pp_is_mod_ty fmt Config.show_mod_ty
          (fun _ ->
            Fmt.fprintf fmt "(";
            Fmt.fprintf fmt "@[";
            pp_mod fmt ?env me;
            Fmt.fprintf fmt " : ";
            Fmt.fprintf fmt "@[";
            pp_mod_ty fmt mt;
            Fmt.fprintf fmt "@]";
            Fmt.fprintf fmt "@]";
            Fmt.fprintf fmt ")")
          mt'

  and pp_top fmt top =
    match top with
    | Top_let (x, e) ->
        Fmt.fprintf fmt "@[<hv>let %s = " x;
        pp_expr fmt e;
        Fmt.fprintf fmt "@]"
    | Top_letrec lams -> (
        match lams with
        | [] -> failwith "neverreach"
        | (x, lam) :: tail ->
            Fmt.fprintf fmt "@[<v>";
            Fmt.fprintf fmt "let rec %s = @[<hov 2>" x;
            pp_lam fmt lam;
            Fmt.fprintf fmt "@]";
            List.iter
              (fun (x, lam) ->
                Fmt.fprintf fmt "@\nand %s = @[" x;
                pp_lam fmt lam;
                Fmt.fprintf fmt "@]")
              tail;
            Fmt.fprintf fmt "@]")
    | Top_type_def td -> pp_ty_def fmt td
    | Top_mod (name, me) ->
        Fmt.fprintf fmt "@[<v 2>module %s = @\n" name;
        pp_mod fmt me;
        Fmt.fprintf fmt "@]"
    | Top_mod_sig (name, mt) ->
        Fmt.fprintf fmt "@[<v 2>module type %s = @\n" name;
        pp_mod_ty fmt mt;
        Fmt.fprintf fmt "@]"
    | Top_external (name, te, ext_name) ->
        Fmt.fprintf fmt "@[<v 2>external %s : " name;
        pp_ty fmt te;
        Fmt.fprintf fmt "= \"%s\"@]" ext_name

  and pp_ty_def fmt td =
    match td with
    | I.Ty_def_opaque (name, paras) ->
        Fmt.fprintf fmt "type (";
        (match paras with
        | [] -> ()
        | tv :: rest ->
            Fmt.fprintf fmt "%s" (Ident.show_ident tv);
            List.iter
              (fun x -> Fmt.fprintf fmt ", %s" (Ident.show_ident x))
              rest);
        Fmt.fprintf fmt ") %s" name
    | Ty_def_adt (name, tvs, vs (* variants *)) ->
        Fmt.fprintf fmt "@[<v>type (";
        (match tvs with
        | [] -> ()
        | tv :: rest ->
            Fmt.fprintf fmt "%s" (Ident.show_ident tv);
            List.iter
              (fun x -> Fmt.fprintf fmt ", %s" (Ident.show_ident x))
              rest);
        Fmt.fprintf fmt ") %s = " name;
        List.iter
          (function
            | c_name, None -> Fmt.fprintf fmt "@\n| %s" c_name
            | c_name, Some te ->
                Fmt.fprintf fmt "@\n| %s of " c_name;
                pp_ty fmt te)
          vs;
        Fmt.fprintf fmt "@]"
    | I.Ty_def_record (name, tvs, fields) ->
        Fmt.fprintf fmt "@<v>type (%s) %s = "
          (tvs |> List.map Ident.show_ident |> String.concat ", ")
          name;
        Fmt.fprintf fmt "@\n  @[{@[<v 2>";
        pp_fields fmt fields;
        Fmt.fprintf fmt "@]@\n}@]"
    | I.Ty_def_alias (name, te) ->
        Fmt.fprintf fmt "@[type %s = " name;
        pp_ty fmt te;
        Fmt.fprintf fmt "@]"

  and pp_ty fmt ?env te =
    match te with
    | I.Ty_cons ((id, name), tes) ->
        Fmt.fprintf fmt "@[";
        (match tes with
        | [] -> Fmt.fprintf fmt "()@ "
        | te0 :: tes ->
            Fmt.fprintf fmt "(";
            pp_ty fmt te0;
            List.iter
              (fun te ->
                Fmt.fprintf fmt ",@ ";
                pp_ty fmt te)
              tes;
            Fmt.fprintf fmt ")@ ");
        (match (Option.bind env (Env.lookup_hint id), id) with
        | Some me, _ ->
            pp_mod fmt ?env me;
            Fmt.fprintf fmt "."
        | None, 0 (* 0 is default root module's structure type id *) -> ()
        | None, _ -> Fmt.fprintf fmt "%d." id);
        Fmt.fprintf fmt "%s" name;
        Fmt.fprintf fmt "@]"
    | I.Ty_var { contents = I.Unbound (tv, _level) } ->
        Fmt.fprintf fmt "{%s}" (Ident.show_ident tv)
    | I.Ty_var { contents = I.Link te } ->
        Fmt.fprintf fmt "{";
        pp_ty fmt te;
        Fmt.fprintf fmt "}"
    | I.Ty_qvar tv -> Fmt.fprintf fmt "[%s]" (Ident.show_ident tv)
    | I.Ty_arrow (arg_ty, ret_ty) ->
        Fmt.fprintf fmt "(@[<v 1>";
        pp_ty fmt arg_ty;
        Fmt.fprintf fmt "@\n->";
        pp_ty fmt ret_ty;
        Fmt.fprintf fmt "@])"
    | I.Ty_tuple [] -> failwith "neverreach"
    | I.Ty_tuple (te0 :: tes) ->
        Fmt.fprintf fmt "(@[<v 1>";
        pp_ty fmt te0;
        List.iter
          (fun te ->
            Fmt.fprintf fmt "@\n* ";
            pp_ty fmt te)
          tes;
        Fmt.fprintf fmt "@])"
    | I.Ty_record fields ->
        Fmt.fprintf fmt "{@[<v 2>";
        pp_fields fmt fields;
        Fmt.fprintf fmt "@]}@\n"

  and pp_pattern fmt p =
    match p with
    | Pat_val (Const_bool b) -> Fmt.pp_print_bool fmt b
    | Pat_val (Const_int i) -> Fmt.pp_print_int fmt i
    | Pat_val (Const_string s) -> Fmt.fprintf fmt "\"%s\"" s
    | Pat_val Const_unit -> Fmt.fprintf fmt "()"
    | Pat_constr (cname, id, p) -> (
        match p with
        | None -> Fmt.fprintf fmt "%s" cname
        | Some p ->
            Fmt.fprintf fmt "(%s[%d]" cname id;
            Fmt.fprintf fmt "@ ";
            pp_pattern fmt p;
            Fmt.fprintf fmt ")")
    | Pat_var (x, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_string fmt x)
          ty
    | Pat_tuple [] -> failwith "neverreach"
    | Pat_tuple (p :: ps) ->
        Fmt.fprintf fmt "(@[<v 1>";
        pp_pattern fmt p;
        List.iter
          (fun te ->
            Fmt.fprintf fmt "@\n* ";
            pp_pattern fmt te)
          ps;
        Fmt.fprintf fmt "@])"

  and pp_fields fmt fields =
    List.iter
      (fun (name, te) ->
        Fmt.fprintf fmt "@\n%s :" name;
        pp_ty fmt te;
        Fmt.fprintf fmt ";")
      fields

  and pp_mod_ty fmt mt =
    match mt with
    | I.Mod_ty_struct
        {
          id;
          val_defs;
          constr_defs;
          ty_defs;
          mod_sigs;
          mod_defs;
          owned_mods;
        } ->
        Fmt.fprintf fmt "@[<v 2>sig@\n";
        Fmt.fprintf fmt "@\nid = %d" id;
        List.iter
          (fun (x, ((qvs, te) : I.bind_ty)) ->
            Fmt.fprintf fmt "@\n@\nval %s : @[" x;
            (match qvs with
            | [] -> ()
            | _ ->
                Fmt.fprintf fmt "forall %s . "
                  (qvs |> List.map Ident.show_ident |> String.concat " "));
            Fmt.fprintf fmt "@[";
            pp_ty fmt te;
            Fmt.fprintf fmt "@]";
            Fmt.fprintf fmt "@]")
          val_defs;
        List.iter
          (fun (x, ((qvs, te), id)) ->
            Fmt.fprintf fmt "@\n@\nconstr %s[%d] : @[" x id;
            (match qvs with
            | [] -> ()
            | _ ->
                Fmt.fprintf fmt "forall %s . "
                  (qvs |> List.map Ident.show_ident |> String.concat " "));
            Fmt.fprintf fmt "@[";
            pp_ty fmt te;
            Fmt.fprintf fmt "@]";
            Fmt.fprintf fmt "@]")
          constr_defs;
        List.iter
          (fun td ->
            Fmt.fprintf fmt "@\n@\n";
            pp_ty_def fmt td)
          ty_defs;
        List.iter
          (fun (name, mt) ->
            Fmt.fprintf fmt "@\n@\nmodule type %s = " name;
            pp_mod_ty fmt mt)
          mod_sigs;
        List.iter
          (fun (name, mt) ->
            Fmt.fprintf fmt "@\n@\nmodule %s : @[" name;
            pp_mod_ty fmt mt;
            Fmt.fprintf fmt "@]")
          mod_defs;
        Fmt.fprintf fmt "@\n@\n@[<v 2>Owned Modules = { ";
        List.iter (fun i -> Fmt.fprintf fmt "@\n%d ;" i) owned_mods;
        Fmt.fprintf fmt "@]@\n}";
        Fmt.fprintf fmt "@]@\n@\nend"
    | I.Mod_ty_functor (mt0, mt1) ->
        Fmt.fprintf fmt "@[<v 2>functor (_ : @[";
        pp_mod_ty fmt mt0;
        Fmt.fprintf fmt "@])@\n-> @\n";
        pp_mod_ty fmt mt1

  let pp_prog fmt prog =
    List.iter
      (fun top ->
        Fmt.fprintf fmt "@\n";
        Fmt.fprintf fmt "@\n";
        pp_top fmt top)
      prog;
    Format.pp_print_flush fmt ()

  let pp_str_of_ty ?env ty =
    let buf = Buffer.create 10 in
    let formatter = Fmt.formatter_of_buffer buf in
    pp_ty formatter ?env ty;
    Fmt.pp_print_flush formatter ();
    Buffer.contents buf
end

module ShowAllConfig : PPConfig = struct
  let show_const_ty = true

  let show_bind_ty = true

  let show_mod_ty = true
end

module ShowNothingConfig : PPConfig = struct
  let show_const_ty = false

  let show_bind_ty = false

  let show_mod_ty = false
end

module DefaultPP = MakePP (ShowAllConfig)
module NoTypeHintPP = MakePP (ShowNothingConfig)

let default_dbg prog =
  let buf = Buffer.create 50 in
  let fmt = Format.formatter_of_buffer buf in
  DefaultPP.pp_prog fmt prog;
  Buffer.contents buf

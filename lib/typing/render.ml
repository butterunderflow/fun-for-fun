open Typedtree
module I = Syntax.Types_in
module Fmt = Format

module type PPConfig = sig
  val show_const_ty : bool

  val show_bind_ty : bool
end

module MakePP (Config : PPConfig) = struct
  let rec pp_expr (fmt : Format.formatter) (e : expr) =
    match e with
    | EConst (CBool b, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_bool fmt b)
          ty
    | EConst (CInt i, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_int fmt i)
          ty
    | EConst (CString s, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_string fmt (Printf.sprintf "\"%s\"" s))
          ty
    | EVar (x, ty) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ -> Fmt.pp_print_string fmt x)
          ty
    | ELet (x, e0, e1, _) ->
        Fmt.fprintf fmt "@[<hv>let %s = " x;
        pp_expr fmt e0;
        Fmt.fprintf fmt "@\n";
        Fmt.fprintf fmt "in";
        Fmt.fprintf fmt "@ ";
        Fmt.fprintf fmt "@[<hov 2>";
        pp_expr fmt e1;
        Fmt.fprintf fmt "@]";
        Fmt.fprintf fmt "@]"
    | ELetrec (lams, body, _) ->
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
    | ELam (x, e, _) ->
        Fmt.fprintf fmt "fun %s ->" x;
        Fmt.fprintf fmt "@\n";
        Fmt.fprintf fmt "  ";
        Fmt.fprintf fmt "@[";
        pp_expr fmt e;
        Fmt.fprintf fmt "@]"
    | EIf (e0, e1, e2, _) ->
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
    | ECase (cond, branches, _) ->
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
    | EApp (op, arg, _) ->
        pp_expr fmt op;
        Fmt.fprintf fmt " ";
        pp_expr fmt arg
    | EAnn (e, te) ->
        Fmt.fprintf fmt "@[<v>";
        pp_expr fmt e;
        Fmt.fprintf fmt ":@[";
        pp_ty fmt te;
        Fmt.fprintf fmt "@]"
    | ETuple (es, _) ->
        let size = List.length es in
        Fmt.fprintf fmt "(";
        List.iteri
          (fun i e ->
            pp_expr fmt e;
            if i = size - 1 then Fmt.fprintf fmt ")"
            else Fmt.fprintf fmt ", ")
          es
    | EField (me, name, te)
    | EFieldCons (me, name, te) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ ->
            Fmt.fprintf fmt "@[";
            pp_mod fmt me;
            Fmt.fprintf fmt ".%s" name;
            Fmt.fprintf fmt "@]")
          te
    | ECons (c, te) ->
        pp_is_ty fmt Config.show_bind_ty
          (fun _ -> Fmt.pp_print_string fmt c)
          te

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
      Fmt.fprintf fmt "@])")
    else content_printer ()

  and pp_mod fmt me =
    match me with
    | MEName (name, _) -> Fmt.pp_print_string fmt name
    | MEStruct (tops, mt) ->
        pp_is_mod_ty fmt true
          (fun _ ->
            Fmt.fprintf fmt "@[<v 2>struct";
            List.iter
              (fun top ->
                Fmt.fprintf fmt "@\n@\n";
                pp_top fmt top)
              tops;
            Fmt.fprintf fmt "@]@\n@\nend")
          mt
    | MEFunctor ((name, mt), me) ->
        Fmt.fprintf fmt "@[<v 2>functor (%s : " name;
        pp_mod_ty fmt mt;
        Fmt.fprintf fmt ")@\n-> @\n";
        pp_mod fmt me
    | MEField (me, name, _) ->
        Fmt.fprintf fmt "@[";
        pp_mod fmt me;
        Fmt.fprintf fmt ".%s" name;
        Fmt.fprintf fmt "@]"
    | MEApply (me0, me1, _) ->
        pp_mod fmt me0;
        Fmt.fprintf fmt "(";
        pp_mod fmt me1;
        Fmt.fprintf fmt ")"
    | MERestrict (me, mt) ->
        pp_mod fmt me;
        Fmt.fprintf fmt ":";
        pp_mod_ty fmt mt

  and pp_top fmt top =
    match top with
    | TopLet (x, e) ->
        Fmt.fprintf fmt "@[<hv>let %s = " x;
        pp_expr fmt e;
        Fmt.fprintf fmt "@]"
    | TopLetRec lams -> (
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
              tail)
    | TopTypeDef td -> pp_ty_def fmt td
    | TopMod (name, me) ->
        Fmt.fprintf fmt "@[<v 2>module %s = @\n" name;
        pp_mod fmt me;
        Fmt.fprintf fmt "@]"
    | TopModSig (name, mt) ->
        Fmt.fprintf fmt "module type %s = @[<v 2>@\n" name;
        pp_mod_ty fmt mt;
        Fmt.fprintf fmt "@]"

  and pp_ty_def fmt td =
    match td with
    | I.TDOpaqueI (name, paras) ->
        Fmt.fprintf fmt "type (";
        (match paras with
        | [] -> ()
        | tv :: rest ->
            Fmt.fprintf fmt "%s" (Ident.show_ident tv);
            List.iter
              (fun x -> Fmt.fprintf fmt ", %s" (Ident.show_ident x))
              rest);
        Fmt.fprintf fmt ") %s" name
    | TDAdtI (name, tvs, vs (* variants *)) ->
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
    | I.TDRecordI (name, tvs, fields) ->
        Fmt.fprintf fmt "@<v>type (%s) %s = "
          (tvs |> List.map Ident.show_ident |> String.concat ", ")
          name;
        Fmt.fprintf fmt "@\n  @[{@[<v 2>";
        pp_fields fmt fields;
        Fmt.fprintf fmt "@]@\n}@]"

  and pp_ty fmt te =
    match te with
    | I.TConsI ((id, name), tes) ->
        Fmt.fprintf fmt "@[";
        (match tes with
        | [] -> Fmt.fprintf fmt "()"
        | te0 :: tes ->
            Fmt.fprintf fmt "(";
            pp_ty fmt te0;
            List.iter
              (fun te ->
                Fmt.fprintf fmt ",@ ";
                pp_ty fmt te)
              tes;
            Fmt.fprintf fmt ")");
        Fmt.fprintf fmt "@ %d.%s" id name;
        Fmt.fprintf fmt "@]"
    | I.TVarI { contents = I.Unbound tv } ->
        Fmt.fprintf fmt "{%s}" (Ident.show_ident tv)
    | I.TVarI { contents = I.Link te } ->
        Fmt.fprintf fmt "{";
        pp_ty fmt te;
        Fmt.fprintf fmt "}"
    | I.TQVarI tv -> Fmt.fprintf fmt "[%s]" (Ident.show_ident tv)
    | I.TArrowI (arg_ty, ret_ty) ->
        Fmt.fprintf fmt "(@[<v 1>";
        pp_ty fmt arg_ty;
        Fmt.fprintf fmt "@\n->";
        pp_ty fmt ret_ty;
        Fmt.fprintf fmt "@])"
    | I.TTupleI [] -> failwith "neverreach"
    | I.TTupleI (te0 :: tes) ->
        Fmt.fprintf fmt "(@[<v 1>";
        pp_ty fmt te0;
        List.iter
          (fun te ->
            Fmt.fprintf fmt "@\n* ";
            pp_ty fmt te)
          tes;
        Fmt.fprintf fmt "@])"
    | I.TRecordI fields ->
        Fmt.fprintf fmt "{@[<v 2>";
        pp_fields fmt fields;
        Fmt.fprintf fmt "@]}@\n"

  and pp_pattern fmt p =
    match p with
    | PVal (CBool b) -> Fmt.pp_print_bool fmt b
    | PVal (CInt i) -> Fmt.pp_print_int fmt i
    | PVal (CString s) -> Fmt.fprintf fmt "\"%s\"" s
    | PCons (cname, p) -> (
        match p with
        | None -> Fmt.fprintf fmt "%s" cname
        | Some p ->
            Fmt.fprintf fmt "(%s" cname;
            Fmt.fprintf fmt "@ ";
            pp_pattern fmt p;
            Fmt.fprintf fmt ")")
    | PVar (x, ty) ->
        pp_is_ty fmt Config.show_const_ty
          (fun _ -> Fmt.pp_print_string fmt x)
          ty
    | PTuple [] -> failwith "neverreach"
    | PTuple (p :: ps) ->
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
    | I.MTMod { id; val_defs; ty_defs; mod_sigs; mod_defs; owned_mods } ->
        Fmt.fprintf fmt "@[<v 2>sig@\n";
        Fmt.fprintf fmt "@\nid = %d" id;
        List.iter
          (fun (x, ((qvs, te) : I.bind_ty)) ->
            Fmt.fprintf fmt "@\n@\nval %s = @[" x;
            Fmt.fprintf fmt "forall %s . "
              (qvs |> List.map Ident.show_ident |> String.concat " ");
            Fmt.fprintf fmt "@[";
            pp_ty fmt te;
            Fmt.fprintf fmt "@]";
            Fmt.fprintf fmt "@]")
          val_defs;
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
            Fmt.fprintf fmt "@\n@\nmodule %s : " name;
            pp_mod_ty fmt mt)
          mod_defs;
        Fmt.fprintf fmt "@\n@\n@[<v 2>Owned Modules = { ";
        List.iter (fun i -> Fmt.fprintf fmt "@\n%d ;" i) owned_mods;
        Fmt.fprintf fmt "@]@\n}";
        Fmt.fprintf fmt "@]@\n@\nend"
    | I.MTFun (mt0, mt1) ->
        Fmt.fprintf fmt "@[<v 2>functor (_ : ";
        pp_mod_ty fmt mt0;
        Fmt.fprintf fmt ")@\n-> @\n";
        pp_mod_ty fmt mt1

  let pp_prog fmt prog =
    List.iter
      (fun top ->
        Fmt.fprintf fmt "@\n";
        Fmt.fprintf fmt "@\n";
        pp_top fmt top)
      prog
end

module ShowAllConfig = struct
  let show_const_ty = true

  let show_bind_ty = true
end

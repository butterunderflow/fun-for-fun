open Clos.Closure
module C = Ctree
module S = Syntax.Parsetree
module F = Format

type context = {
  dict : (string * string) list;
  vars : string list ref;
}

let to_c_ident (id : Ident.ident) =
  let str = Ident.to_string id in
  String.map
    (fun c ->
      match c with
      | '\\'
      | '/' ->
          '_'
      | _ -> c)
    str

let ff_obj_typename = C.NAMED_TYPE "ff_obj_t"

let ff_fvs_typename = C.NAMED_TYPE "ff_fvs_t"

let ff_obj_array_type = C.ARRAY (ff_obj_typename, NOTHING)

let ff_str_type = C.PTR (CONST (CHAR NO_SIGN))

let ff_str_array_type = C.ARRAY (PTR (CONST (CHAR NO_SIGN)), NOTHING)

let ff_make_int = C.VARIABLE "ff_make_int"

let ff_make_bool = C.VARIABLE "ff_make_int" (* use int to represent bool *)

let ff_make_str = C.VARIABLE "ff_make_str"

let ff_make_tuple = C.VARIABLE "ff_make_tuple"

let ff_apply = C.VARIABLE "ff_apply"

let ff_get_mem = C.VARIABLE "ff_get_member"

let ff_constr_p = C.VARIABLE "ff_make_constr_payload"

let ff_constr_np = C.VARIABLE "ff_make_constr_no_payload"

let ff_make_closure = C.VARIABLE "ff_make_closure"

let ff_make_placeholder = C.VARIABLE "ff_make_placeholder"

let ff_make_mod_obj = C.VARIABLE "ff_make_mod_obj"

let ff_fill_letrec_closure = C.VARIABLE "ff_fill_letrec_closure"

let header = {|
#include"fun_rt.h"
#include<stdio.h>

|}

let make_c_ident x = to_c_ident (Ident.create ~hint:x)

let make_context fvs =
  let dict = List.map (fun x -> (x, make_c_ident x)) fvs in
  let c_vars = List.map (fun (_, c_ident) -> c_ident) dict in
  ({ dict; vars = ref c_vars }, c_vars)

let create_var ~need_decl x ctx =
  let c_var = make_c_ident x in
  if need_decl then ctx.vars := c_var :: ctx.vars.contents;
  (c_var, { ctx with dict = (x, c_var) :: ctx.dict })

let create_decl x ctx =
  let c_var = make_c_ident x in
  ctx.vars := c_var :: !(ctx.vars);
  c_var

let get_var_decls ctx =
  List.map
    (fun v -> C.DECDEF (C.NO_STORAGE, (v, ff_obj_typename)))
    !(ctx.vars)

let make_stmt_seq stmts =
  List_utils.fold_left_first (fun s0 s1 -> C.SEQUENCE (s0, s1)) stmts

let make_assign lhs rhs = C.(COMPUTATION (BINARY (ASSIGN, lhs, rhs)))

let make_compound element_typ es =
  C.CAST (ARRAY (element_typ, NOTHING), CONSTANT (CONST_COMPOUND es))

let get_all_member_names (mems : object_field list) =
  mems
  |> List.map (fun mem ->
         match mem with
         | FSimple (x, _) -> [ x ]
         | FLetRec (_, binds) -> fst (List.split binds))
  |> List.flatten

let rec trans_main_expr buf (_e : expr) =
  Printf.bprintf buf
    {|
#include<stdio.h>
#include"fun_rt.h"

int main()
{
  test_rt();
  printf("Hello World");
}
|}

and trans_expr ctx e =
  match e with
  | EVar x -> (List.assoc x ctx.dict, [])
  | ELet (x, e0, e1) ->
      let e0_v, e0_stmts = trans_expr ctx e0 in
      let result_stmt = e0_stmts in
      let x, ctx = create_var ~need_decl:true x ctx in
      let result_stmt =
        result_stmt @ [ make_assign (VARIABLE x) (VARIABLE e0_v) ]
      in
      let e1_v, e1_stmts = trans_expr ctx e1 in
      let result_stmt = result_stmt @ e1_stmts in
      (e1_v, result_stmt)
  | EConst (S.CInt i) ->
      let ret_v = create_decl "temp" ctx in
      ( ret_v,
        [
          make_assign (VARIABLE ret_v)
            (CALL (ff_make_int, [ CONSTANT (CONST_INT (string_of_int i)) ]));
        ] )
  | EConst (S.CBool b) ->
      let ret_v = create_decl "temp" ctx in
      ( ret_v,
        [
          make_assign (VARIABLE ret_v)
            (CALL
               ( ff_make_bool,
                 [ CONSTANT (CONST_INT (if b then "1" else "0")) ] ));
        ] )
  | EConst (S.CString s) ->
      let ret_v = create_decl "temp" ctx in
      ( ret_v,
        [
          make_assign (VARIABLE ret_v)
            (CALL (ff_make_str, [ CONSTANT (CONST_STRING s) ]));
        ] )
  | ETuple es ->
      let es_v, stmts_list = List.split (List.map (trans_expr ctx) es) in
      let stmts = List.flatten stmts_list in
      let tu = make_c_ident "tu" in
      let stmts =
        stmts
        @ [
            C.(
              COMPUTATION
                (BINARY
                   ( ASSIGN,
                     VARIABLE tu,
                     CALL (ff_make_tuple, List.map (fun v -> VARIABLE v) es_v)
                   )));
          ]
      in
      (tu, stmts)
  | EIf (e0, e1, e2) ->
      let ifel_v = create_decl "ifel_res" ctx in
      let e0_v, e0_stmts = trans_expr ctx e0 in
      let e1_v, e1_stmts = trans_expr ctx e1 in
      let e2_v, e2_stmts = trans_expr ctx e2 in
      ( ifel_v,
        e0_stmts
        @ [
            C.(
              IF
                ( VARIABLE e0_v,
                  make_stmt_seq
                    (e1_stmts
                    @ [ make_assign (VARIABLE ifel_v) (VARIABLE e1_v) ]),
                  make_stmt_seq
                    (e2_stmts
                    @ [ make_assign (VARIABLE ifel_v) (VARIABLE e2_v) ]) ));
          ] )
  | EApp (e0, e1) ->
      let app_v = create_decl "app_res" ctx in
      let e0_v, e0_stmts = trans_expr ctx e0 in
      let e1_v, e1_stmts = trans_expr ctx e1 in
      ( app_v,
        e0_stmts @ e1_stmts
        @ [
            make_assign (VARIABLE app_v)
              (CALL (ff_apply, [ VARIABLE e0_v; VARIABLE e1_v ]));
          ] )
  | ECons i ->
      let constr_v = create_decl (Printf.sprintf "constr%d" i) ctx in
      ( constr_v,
        [
          make_assign (VARIABLE constr_v)
            (CALL (ff_constr_np, [ CONSTANT (CONST_INT (string_of_int i)) ]));
        ] )
  | EConsWith i ->
      let constr_v = create_decl (Printf.sprintf "constr%d" i) ctx in
      ( constr_v,
        [
          make_assign (VARIABLE constr_v)
            (CALL (ff_constr_p, [ CONSTANT (CONST_INT (string_of_int i)) ]));
        ] )
  | EField (e, name) ->
      let field = create_decl "field" ctx in
      let e_v, e_stmts = trans_expr ctx e in
      ( field,
        e_stmts
        @ [
            make_assign (VARIABLE field)
              (CALL
                 (ff_get_mem, [ VARIABLE e_v; CONSTANT (CONST_STRING name) ]));
          ] )
  | EClosure (fvs, cfunc) ->
      let clos_v = create_decl "clos" ctx in
      let fvs_c = List.map (fun fv -> List.assoc fv ctx.dict) fvs in
      let cfn_name = to_c_ident cfunc in
      ( clos_v,
        [
          make_assign (VARIABLE clos_v)
            (CALL
               ( ff_make_closure,
                 [
                   make_compound ff_obj_typename
                     (List.map (fun v -> C.VARIABLE v) fvs_c);
                   CONSTANT (CONST_INT (string_of_int (List.length fvs)));
                   VARIABLE cfn_name;
                 ] ));
        ] )
  | ELetRec ((fvs, binds), body) ->
      let _binds_c, letrec_init, ctx = trans_letrec fvs binds ctx in
      let body_c, body_stmts = trans_expr ctx body in
      (body_c, letrec_init @ body_stmts)
  | EModObject members ->
      let stmts, mems_c, ctx =
        List.fold_left
          (fun (stmts_acc, mems_c_acc, ctx) mem ->
            match mem with
            | FSimple (x, e) ->
                let e_c, e_stmts = trans_expr ctx e in
                let x_c, ctx = create_var ~need_decl:true x ctx in
                ( stmts_acc @ e_stmts
                  @ [ make_assign (VARIABLE x_c) (VARIABLE e_c) ],
                  mems_c_acc @ [ x_c ],
                  ctx )
            | FLetRec (fvs, binds) ->
                let binds_c, bind_stmts, ctx = trans_letrec fvs binds ctx in
                (stmts_acc @ bind_stmts, mems_c_acc @ binds_c, ctx))
          ([], [], ctx) members
      in
      let mod_v = create_decl "mod" ctx in
      ( mod_v,
        stmts
        @ [
            make_assign (VARIABLE mod_v)
              (CALL
                 ( ff_make_mod_obj,
                   [
                     CONSTANT
                       (CONST_INT (string_of_int (List.length mems_c)));
                     make_compound ff_str_type
                       (get_all_member_names members
                       |> List.map (fun name ->
                              C.CONSTANT (CONST_STRING name)));
                     make_compound ff_obj_typename
                       (List.map (fun x -> C.VARIABLE x) mems_c);
                   ] ));
          ] )
  | ESwitch (_, _)
  | EStruct _ ->
      ("todo", [])

and trans_letrec fvs binds ctx =
  let fvs_c = List.map (fun fv -> List.assoc fv ctx.dict) fvs in
  (* create n binds objects *)
  let binds_c, ctx =
    List.fold_left
      (fun (acc_binds_c, ctx) (x, _) ->
        let bind_c, ctx = create_var ~need_decl:true x ctx in
        (acc_binds_c @ [ bind_c ], ctx))
      ([], ctx) binds
  in
  (* mark them as todo *)
  let todo_inits =
    List.map
      (fun x -> make_assign (VARIABLE x) (CALL (ff_make_placeholder, [])))
      binds_c
  in
  (* initialize letrec bindings *)
  let letrec_init =
    [
      C.COMPUTATION
        (CALL
           ( ff_fill_letrec_closure,
             [
               make_compound ff_obj_typename
                 (List.map (fun x -> C.VARIABLE x) fvs_c);
               CONSTANT (CONST_INT (string_of_int (List.length fvs)));
               make_compound ff_obj_typename
                 (List.map
                    (fun (_, cfunc) -> C.VARIABLE (to_c_ident cfunc))
                    binds);
               CONSTANT (CONST_INT (string_of_int (List.length binds)));
               make_compound (C.PTR ff_obj_typename)
                 (List.map (fun x -> C.(UNARY (ADDROF, VARIABLE x))) binds_c);
             ] ));
    ]
  in
  (binds_c, todo_inits @ letrec_init, ctx)

and trans_fn (cfunc, fvs, para, e) =
  Ident.refresh ();
  let ctx, c_fvs = make_context fvs in
  let cfn_name = to_c_ident cfunc in
  let c_para, ctx = create_var ~need_decl:false para ctx in
  let fvs_para, ctx = create_var ~need_decl:false "fvs" ctx in
  let var, body = trans_expr ctx e in
  let var_decls = get_var_decls ctx in
  let cfn_body =
    List.mapi
      (fun i cfv ->
        C.(
          COMPUTATION
            (BINARY
               ( ASSIGN,
                 VARIABLE cfv,
                 INDEX
                   (VARIABLE fvs_para, CONSTANT (CONST_INT (string_of_int i)))
               ))))
      c_fvs
    @ body
    @ [ C.RETURN (C.VARIABLE var) ]
  in
  let proto =
    C.PROTO
      ( ff_obj_typename,
        [ (fvs_para, ff_fvs_typename); (c_para, ff_obj_typename) ] )
  in
  ( C.FUNDEF
      ( (cfn_name, proto),
        ( var_decls,
          List_utils.fold_left_first
            (fun s0 s1 -> C.SEQUENCE (s0, s1))
            cfn_body ) ),
    C.DECDEF (NO_STORAGE, (cfn_name, proto)) )

let create_main e =
  (* main doesn't have parameter, so we add a special function to handle
     this *)
  Ident.refresh ();
  let ctx, _ = make_context [] in
  let cfn_name =
    make_c_ident
      "_ff_main" (* fixme: this name may clash with existing name *)
  in
  let var, body = trans_expr ctx e in
  let var_decls = get_var_decls ctx in
  let cfn_body = List.append body [ C.RETURN (C.VARIABLE var) ] in
  let proto = C.PROTO (ff_obj_typename, []) in
  ( cfn_name,
    C.FUNDEF
      ( (cfn_name, proto),
        ( var_decls,
          List_utils.fold_left_first
            (fun s0 s1 -> C.SEQUENCE (s0, s1))
            cfn_body ) ),
    C.DECDEF (NO_STORAGE, (cfn_name, proto)) )

let translate (e, (fns : func list)) =
  let main_name, main_def, main_decl = create_main e in
  let fn_defs, fn_decls = List.split (List.map trans_fn fns) in
  let fn_defs = main_def :: fn_defs in
  let fn_decls = main_decl :: fn_decls in
  let buf = Buffer.create 50 in
  Cprint1.print buf fn_decls;
  Cprint1.print buf fn_defs;
  let prog = Buffer.contents buf in
  let driver =
    Printf.sprintf {|
int main()
{
  test_rt();
  %s();
}
|} main_name
  in
  header ^ prog ^ driver

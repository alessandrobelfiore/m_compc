open Symbol_table
open Ast
open Utils
exception Semantic_error of Location.code_pos * string

let (|@|) node ttype = { node = node; annot = ttype }

let gamma = ref empty_table (** global scope *)

let interfaces_t = ref empty_interfaces_t  (* table InterfaceName -> list of decl *)

let has_returned : bool ref = ref false (* flag to check if a function has returned *)

let can_deref : bool ref = ref false (* flag to check if we can auto dereference ptrs while checking exprs *)

(* Adds the interface app with method main *)
let init = 
  gamma := add_entry "App" ((TInterface("App")), Some "App") !gamma Location.dummy_code_pos;
  let main_decl = 
    [FunDecl 
      {
        rtype = TInt;
        fname = "main";
        formals = [];
        body = None;
      } |@| TInt
    ] in
  add_methods "App" main_decl !interfaces_t Location.dummy_code_pos

(** returns the type of the values in the array being accessed *)
let type_of_array_acc lv loc =
  match type_of_node lv with
  | TArray (t, _) -> 
    if !can_deref then
      (
        match t with
        | TRef(typ) -> typ
        | typ -> typ
      )
    else t
  | _ -> raise (Semantic_error (loc, "Value is not an array, can't be accessed"))

(** analyses lvalue lv in the env gamma,
  returns the typed lvalue if it's well typed *)
let rec check_lv lv =
  let loc = lv.annot in
  let node = lv.node in
  match node with
  | AccVar(_, id) ->
    let t = lookup id !gamma loc in
    (
      match t with
      | TRef(t), interface_name ->
        if !can_deref then
          AccVar(interface_name, id) |@| t
        else AccVar(interface_name, id) |@| TRef(t)
      | t, interface_name -> AccVar(interface_name, id) |@| t
    )
  | AccIndex(lv, e) ->
    let typed_e = check_expr e in
    let typed_lv = check_lv lv in
    begin
    match (typed_e.annot) with
    | TInt ->
      begin
      match typed_e.node with
      | ILiteral (i) ->
        if i >= 0 then
        (* extrapolates the type of values contained in the array *)
        let t = type_of_array_acc typed_lv loc in
        (
          match (lv.node) with
          | AccIndex(_, _) ->
            raise (Semantic_error (loc, "Multidimensional array not permitted"))
          | _ ->
            (
              match typed_lv.annot with
                | TArray(_, None)     ->
                  AccIndex(typed_lv, typed_e) |@| t
                | TArray(_, Some dim) -> 
                  if dim <= i then
                    raise (Semantic_error (loc, "Array access out of bounds"))
                  else AccIndex(typed_lv, typed_e) |@| t
                | _ -> raise (Semantic_error (loc, "Should not be reachable (arr acc)"))
            )
        )
        else raise (Semantic_error (loc, "Array index must be a positive number"))
      | _ ->
        let t = type_of_array_acc typed_lv loc in
        AccIndex(typed_lv, typed_e) |@| t
      end
    | _ -> raise (Semantic_error (loc, "Array index must be an int"))
    end

(** analyses expression e in the env gamma,
    returns the typed expr if it's well typed *)
and check_expr (e : Location.code_pos Ast.expr) : Ast.typ Ast.expr =
  let loc = e.annot in
  let node = e.node in
  if !has_returned 
    then raise (Semantic_error (loc, "Code following a return is not permitted"))
  else
  match node with
  | ILiteral(i) -> ILiteral(i) |@| TInt
  | CLiteral(c) -> CLiteral(c) |@| TChar
  | BLiteral(b) -> BLiteral(b) |@| TBool
  | BinaryOp(op, e1, e2) ->
    (
      match op with
      | Add
      | Sub
      | Mult
      | Div
      | Mod ->
        let typed_e1 = check_expr e1 in
        let typed_e2 = check_expr e2 in
        let t1 = type_of_node typed_e1 in
        let t2 = type_of_node typed_e2 in
        if t1 = TInt && t2 = TInt then
          BinaryOp(op, typed_e1, typed_e2) |@| TInt
        else raise (Semantic_error (loc, "Both operator must be integers"))
      | Less
      | Leq
      | Greater
      | Geq ->
        let typed_e1 = check_expr e1 in
        let typed_e2 = check_expr e2 in
        let t1 = type_of_node typed_e1 in
        let t2 = type_of_node typed_e2 in
        if t1 = TInt && t2 = TInt then
          BinaryOp(op, typed_e1, typed_e2) |@| TBool
        else raise (Semantic_error (loc, "Both operator must be integers"))
      | Equal
      | Neq ->
        let typed_e1 = check_expr e1 in
        let typed_e2 = check_expr e2 in
        let t1 = type_of_node typed_e1 in
        let t2 = type_of_node typed_e2 in
        if t1 = t2 then
          BinaryOp(op, typed_e1, typed_e2) |@| TBool
        else raise (Semantic_error (loc, "Both operator must be of the same type"))
      | And 
      | Or ->
        let typed_e1 = check_expr e1 in
        let typed_e2 = check_expr e2 in
        let t1 = type_of_node typed_e1 in
        let t2 = type_of_node typed_e2 in
        if t1 = TBool && t2 = TBool then
          BinaryOp(op, typed_e1, typed_e2) |@| TBool
        else raise (Semantic_error (loc, "Both operator must be booleans"))
    )
  | UnaryOp(op, e) ->
    (
      match op with
      | Neg
      | PreIncr
      | PreSub
      | PostIncr
      | PostSub ->
        let typed_e = check_expr e in
        let t = type_of_node typed_e in
        if t = TInt then
          UnaryOp(op, typed_e) |@| TInt
        else raise (Semantic_error (loc, "Required an int, found: " ^ Ast.show_typ t))
      | Not ->
        let typed_e = check_expr e in
        let t = type_of_node typed_e in
        if t = TBool then
          UnaryOp(Not, typed_e) |@| TBool
        else raise (Semantic_error (loc, "Required a bool, found: " ^ Ast.show_typ t))
    )
  | LV(lv) ->
    let typed_lv = check_lv lv in
    let t = type_of_node typed_lv in
    LV (typed_lv) |@| t
  | Address(lv) ->
    let tmp = !can_deref in
    can_deref := false;
    let typed_lv = check_lv lv in
    let t = type_of_node typed_lv in
    can_deref := tmp;
    if check_basic_type t then
      Address(typed_lv) |@| TRef(t)
    else raise (Semantic_error (loc, "Cannot address the location of non-base type " ^ Ast.show_typ t))
  | Assign(lv, e) ->
    let typed_lv = check_lv lv in
    let t = type_of_node typed_lv in
    can_deref := true;  (* automatic dereferencing *)
    let typed_e = check_expr e in
    let t2 = type_of_node typed_e in
    can_deref := false;
    (
      match t with
      | TArray(_) -> raise (Semantic_error (loc, "Array cannot be assigned"))
      | TRef(t1) -> (* automatic dereferencing (ptr = ptr + val) *)
        if t2 = TRef(t1) || t1 = t2 then Assign(typed_lv, typed_e) |@| t2
        else raise (Semantic_error (loc, "1Assignment requires same type: lhs " ^ (Ast.show_typ t1) 
        ^ " rhs: " ^ (Ast.show_typ t2) ))
      | t1 ->
        if t1 = t2 then Assign(typed_lv, typed_e) |@| t1
        else raise (Semantic_error (loc, "1Assignment requires same type: lhs " ^ (Ast.show_typ t1) 
        ^ " rhs: " ^ (Ast.show_typ t2) ))
    )
  | Call(_, id, list) ->
    let t = lookup id !gamma loc in
    can_deref := true;
    let typed_list = List.map check_expr list in
    can_deref := false;
    let types_list = List.map type_of_node_in_f typed_list in
    (
      match t with
      | TFun(tx, tr), interface_name ->
        if tx = types_list || (tx = [TVoid] && types_list = []) then (* check if arguments in call match definition *)
          Call(interface_name, id, typed_list) |@| tr
        else
          raise (Semantic_error (loc, "Type mismatch in function call"))
      | _ -> raise (Semantic_error (loc, "Calling a non functional argument"))
    )
(** checks stmt or dec, returns typed stmt or dec *)
let rec check_stmt_or_dec (stmt : Location.code_pos Ast.stmtordec) : Ast.typ Ast.stmtordec =
  let loc = stmt.annot in
  let node = stmt.node in
  match node with
  | LocalDecl(id, t) ->
    let _ = add_entry id (t, None) !gamma loc in
    LocalDecl(id, t) |@| t
  | Stmt(s) ->
    let typed_s = check_stmt s in
    let t = type_of_node typed_s in
    Stmt(typed_s) |@| t

(** type checks one statement and returns the typed stmt *)
and check_stmt (stmt : Location.code_pos Ast.stmt) : Ast.typ Ast.stmt =
  let loc = stmt.annot in
  let node = stmt.node in
  match node with
  | Expr(e) ->
    let typed_e = check_expr e in
    let t = type_of_node typed_e in
    Expr(typed_e) |@| t
  | If(e, s1, s2) ->
    let typed_e = check_expr e in
    let t = type_of_node typed_e in
    if t = TBool then
      (
        gamma := begin_block !gamma None;
        let typed_s1 = check_stmt s1 in
        (has_returned := false);  (* reset the return flag exiting the branch *)
        (gamma := end_block !gamma);
        gamma := begin_block !gamma None;
        let typed_s2 = check_stmt s2 in
        (gamma := end_block !gamma);
        If(typed_e, typed_s1, typed_s2) |@| TVoid
      )
    else raise (Semantic_error (loc, "If condition must be boolean, found  " ^ Ast.show_typ t))
  | DoWhile(e, s) ->
    let typed_e = check_expr e in
    let t = type_of_node typed_e in
    if t = TBool then
      (
        gamma := begin_block !gamma None;
        let typed_s = check_stmt s in
        gamma := end_block !gamma;
        DoWhile(typed_e, typed_s) |@| TVoid
      )
    else raise (Semantic_error (loc, "While condition must be a boolean, found  " ^ Ast.show_typ t ))
  | While(e, s) ->
    let typed_e = check_expr e in
    let t = type_of_node typed_e in
    if t = TBool then
      (
        gamma := begin_block !gamma None;
        let typed_s = check_stmt s in
        gamma := end_block !gamma;
        While(typed_e, typed_s) |@| TVoid
      )
    else raise (Semantic_error (loc, "While condition must be a boolean, found  " ^ Ast.show_typ t ))
  | For(e1, e2, e3, s) ->
    let typed_e1 =
      match e1 with
      | None -> None
      | Some exp ->
        check_expr_is_assign exp loc;
        Some (check_expr exp) in
    let typed_e3 =
      match e3 with
      | None -> None
      | Some exp ->
        check_expr_is_assign exp loc;
        Some (check_expr exp) in
    let typed_e2 = check_expr e2 in
    let t2 = type_of_node typed_e2 in
    if t2 = TBool then
      (
        gamma := begin_block !gamma None;
        let typed_s = check_stmt s in
        gamma := end_block !gamma;
        For(typed_e1, typed_e2, typed_e3, typed_s) |@| TVoid
      )
    else raise (Semantic_error (loc, "For condition must be a boolean"))
  | Return(e) ->
    (
      let rtyp = get_rtype !gamma in
      match e with
      | None ->
        has_returned := true;
        if rtyp = Some TVoid then
          Return (None) |@| TVoid
        else raise (Semantic_error (loc, "Expected non-void return type"))
      | Some e ->
        let typed_e = check_expr e in
        let t = type_of_node typed_e in
        has_returned := true;
        if rtyp = Some t then
          Return(Some typed_e) |@| t
        else raise (Semantic_error (loc, "Wrong return type"))
    )
  | Block(stmts) ->
    gamma := begin_block !gamma None;
    let typed_block = List.map check_stmt_or_dec stmts in
    (
      gamma := end_block !gamma;
      Block(typed_block) |@| TVoid
    )
  | Skip -> Skip |@| TVoid

(* checks that the component implements all functions provided *)
let rec check_provides (interfaces : Ast.identifier list) (decls : Ast.typ Ast.member_decl list) loc names_seen : unit =
  match interfaces with
  | [] -> ()
  | interface :: xs ->
    if List.find_opt (fun name -> name = interface) names_seen != None
      then raise (Semantic_error (loc, "Repetition in the interfaces provided"))
    else
      let fun_to_implement = get_methods interface !interfaces_t in (* get methods to implement for one provided interface *)
      List.iter (find_decl_in_list decls loc) fun_to_implement; (* foreach el in fun_to_implement try to find it in decls *)
      check_provides xs decls loc (interface :: names_seen)

(* add paramenters to the given scope *)
let rec add_params param_list scope loc =
  match param_list with
  | [] -> ()
  | (id, typ) :: xs ->
    check_vardecl_type true typ loc;
    let _ = add_entry id (typ, None) scope loc in
    add_params xs scope loc

(** checks member decl *)
let check_member_decl (is_proto : bool) (decl : Location.code_pos Ast.member_decl) : Ast.typ Ast.member_decl =
  let loc = decl.annot in
  let node = decl.node in
  match node with
  | VarDecl(id, t) -> (* should not happen *)
    check_vardecl_type false t loc;
    let _ = add_entry id (t, None) !gamma loc in 
    VarDecl(id, t) |@| t
  | FunDecl
    {
      rtype = rtyp;
      fname = name;
      formals = params;
      body = b
    } ->
      gamma := begin_block !gamma (Some rtyp);
      add_params params !gamma loc; (* adding the formal parameters to the function scope *)
      has_returned := false;
      let typed_body =
        match b with
        | None -> None
        | Some s -> Some (check_stmt s) 
      in
      gamma := end_block !gamma;
      if !has_returned = false && is_proto = false && rtyp != TVoid
        then raise (Semantic_error (loc, "Missing return statement"))
      else
        FunDecl
        {
          rtype = rtyp;
          fname = name;
          formals = params;
          body = typed_body
        } |@| rtyp

(* adds the function prototypes to the current scope (to enable recursive definitions) *)
let add_local_protos decl =
  let loc = decl.annot in
  let node = decl.node in
  match node with
  | VarDecl _ ->()
  | FunDecl
    {
      rtype = rtyp;
      fname = name;
      formals = params;
      _
    } ->
    let tlist = List.map type_of_vdecl params in
    if check_basic_type rtyp || rtyp = TVoid then
      let _ = add_entry name ((TFun(tlist, rtyp)), None) !gamma loc in (* add function name to the scope *)
      ()
    else raise (Semantic_error (loc, "Illegal return type, " ^ Ast.show_typ rtyp))

(** type checks one interface declaration in the current global scope *)
let check_inter (i : Location.code_pos Ast.interface_decl) : Ast.typ Ast.interface_decl =
  let loc = i.annot in
  let node = i.node in
  match node with
  | InterfaceDecl {
    iname = name;
    declarations = decl
    } ->
      (* add list of decls in hashtable *)
      let _ = add_entry name ((TInterface name), None) !gamma loc in
      gamma := begin_block !gamma (None);
      let typed_decl = List.map (check_member_decl true) decl in
      add_methods name typed_decl !interfaces_t loc;
      gamma := end_block !gamma;
      InterfaceDecl {
        iname = name;
        declarations = typed_decl;
      } |@| (Ast.TInterface (name))

(** type checks one component declaration in the current global scope *)
let check_comp (c : Location.code_pos Ast.component_decl) : Ast.typ Ast.component_decl =
  let loc = c.annot in
  let node = c.node in 
  match node with
  | ComponentDecl {
    cname = name;
    uses  = use;
    provides = prov;
    definitions = defs;
  } ->
    let _ = add_entry name ((TComponent name), None) !gamma loc in
    gamma := begin_block !gamma (None);
    add_methods_from_use use !gamma loc !interfaces_t []; (* add all methods present in uses *)
    add_prelude !gamma loc; (* adds the methods in Prelude *)
    List.iter add_local_protos defs; (* adds local prototypes for recursive defs *)
    let typed_defs = List.map (check_member_decl false) defs in
    check_provides prov typed_defs loc [];
    gamma := end_block !gamma; 
    ComponentDecl {
      cname = name;
      uses  = use;
      provides = prov;
      definitions = typed_defs;
    } |@| (Ast.TComponent name)

(** type checks the entire ast in the global scope *)
let type_check (ast : Ast.located_compilation_unit) : Ast.typed_compilation_unit =
  init;
  match ast with
  | CompilationUnit {
    interfaces = i;
    components = c;
    connections = l } ->
    let typed_i = List.map check_inter i in
    let typed_c = List.map check_comp c in
    CompilationUnit {
      interfaces = typed_i;
      components = typed_c;
      connections = l }
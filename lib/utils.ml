open Ast
exception Semantic_error of Location.code_pos * string

(* returns the type of the Ast.node node *)
let type_of_node node =
  match node with
  | { node = _; annot = a } -> a
(* returns the type of the Ast.node node in a function call *)
let type_of_node_in_f node =
  match node with
  | { node = _; annot = a } ->
    (match a with
    | TArray(t, _) -> TArray(t, None)
    | t -> t)
(* returns the type of the variable decl vdecl *)
let type_of_vdecl vdecl =
  match vdecl with
  | (_, typ) -> typ
(* checks if 2 function signatures match, returns a bool *)
let match_proto_decl proto decl =
  match proto.node, decl.node with
  | FunDecl
    {
      rtype = typ;
      fname = name;
      formals = params;
      _
    } ,
    FunDecl
    {
      rtype = typ2;
      fname = name2;
      formals = params2;
      _
    } -> 
      (if typ = typ2
        && name = name2
        && List.map type_of_vdecl params = List.map type_of_vdecl params2
        then true
      else false)
  | _ ->
    false
(* returns the name of the given member if it's a function *)
let get_fname_from_member member =
  match member.node with
  | FunDecl
   {
     fname = name;
     _
   } -> name
  | _ -> failwith "error using this utility function!"
(* raises an exception if we don't find decl in the given list, returns () otherwise *)  
let rec find_decl_in_list (list : Ast.typ Ast.member_decl list) loc (decl : Ast.typ Ast.member_decl) : unit =
  match list with
  | [] ->
    raise (Semantic_error (loc, "Missing implementation of method: " ^ get_fname_from_member decl))
  | x :: xs ->
    if match_proto_decl x decl then ()
    else find_decl_in_list xs loc decl
(* returns true if typ is a basic type, false otherwise *)
let check_basic_type (typ : Ast.typ) : bool =
  match typ with
  | TInt
  | TChar
  | TBool -> true
  | _ -> false
(* returns () if typ is a valid type for a variable declaration, raises an exception otherwise *)
let check_vardecl_type is_param (typ : Ast.typ) loc : unit=
  if check_basic_type typ then ()
  else match typ with
  | TArray (t, dim) -> 
    if check_basic_type t && (is_param || dim > (Some 0)) then ()
    else raise (Semantic_error (loc, "Illegal type in variable declaration " ^ Ast.show_typ t))
  | TRef (t) ->
    if check_basic_type t then ()
    else raise (Semantic_error (loc, "Illegal type in variable declaration"))
  | _ -> raise (Semantic_error (loc, "Unrecognized type"))
(* adds in the given scope the methods defined in the interfaces used *)
let rec add_methods_from_use (list: Ast.identifier list) scope loc table names_seen =
  match list with
  | [] -> ()
  | interface :: xs ->
    if interface = "App"
      then raise (Semantic_error (loc, "Cannot use interface App"))
    else
    (
      if List.find_opt (fun name -> name = interface) names_seen != None
        then raise (Semantic_error (loc, "Repetition in the interfaces used"))
      else
        let methods = Symbol_table.get_methods interface table in
        List.iter (add_member_decl interface scope loc) methods;
        add_methods_from_use xs scope loc table (interface :: names_seen)
    )
(* adds the given member decl in the scope if it's a function *)
and add_member_decl interface_name scope loc decl =
  match decl.node with
  | FunDecl
    {
      rtype = typ;
      fname = name;
      formals = params;
      _
    } ->
      let params_type = List.map type_of_vdecl params in
      let _ = Symbol_table.add_entry name (TFun(params_type, typ), Some interface_name) scope loc in
      ()
    | _ -> failwith "error"
(* adds the methods getint() and print() to the component scope *)
let add_prelude scope loc =
  let type_of_print   = TFun([TInt], TVoid) in
  let _ = Symbol_table.add_entry "print" (type_of_print, Some "Prelude") scope loc in
  let type_of_getint  = TFun([TVoid], TInt) in
  let _ = Symbol_table.add_entry "getint" (type_of_getint, Some "Prelude") scope loc in
  ()
(* raises an exception if the given expr is not an Assign *)  
let check_expr_is_assign expr loc =
  match expr.node with
  | Assign(_) -> ()
  | _ -> raise (Semantic_error(loc, "Ill-formed for"))
(* returns true if s2 is contained in s1, false otherwise *)  
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0) ;
    true
  with Not_found -> false
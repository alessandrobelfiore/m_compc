exception Semantic_error of Location.code_pos * string
exception DuplicateEntry of Ast.identifier
exception SymbolNotFound of Ast.identifier

type scope = 
  {
    rtyp   : Ast.typ option;
    vars    : (Ast.identifier, Ast.typ * Ast.identifier option) Hashtbl.t;
    parent  : scope option
  }

type interface_t =
  {
    decls : (Ast.identifier, Ast.typ Ast.member_decl list) Hashtbl.t
  }

let empty_interfaces_t =
  { decls = Hashtbl.create 0 }

let add_methods id meths table loc =
  let interface_name = Hashtbl.find_opt table.decls id in
  match interface_name with
  | Some _  -> raise (Semantic_error (loc, "Duplicate id: " ^ id))
  | None    -> Hashtbl.add table.decls id meths

let get_methods id table =
  let interface_name = Hashtbl.find_opt table.decls id in
  match interface_name with
  | Some v  -> v
  | None    -> raise (SymbolNotFound id)

let empty_table =
  { rtyp = None; vars = Hashtbl.create 0; parent = None }

let begin_block table rtyp =
  { rtyp = rtyp; vars = Hashtbl.create 0; parent = Some table }

let end_block table =
  match table.parent with
  | Some p  -> p
  | None    -> table

let add_entry id typ table loc =
  let var = Hashtbl.find_opt table.vars id in
  match var with
  | Some _  -> raise (Semantic_error (loc, "Duplicate id: " ^ id))
  | None    -> Hashtbl.add table.vars id typ;
  table

let rec lookup id table loc =
  let var = Hashtbl.find_opt table.vars id in
  match var with
  | Some v  -> v
  | None    ->
    match table.parent with
    | Some p  -> lookup id p loc
    | None    -> raise (Semantic_error (loc, "Symbol not found: " ^ id))

let rec get_rtype table =
  match table.rtyp with
  | Some t  -> Some t
  | None    ->
    match table.parent with
    | None -> None
    | Some p -> get_rtype p
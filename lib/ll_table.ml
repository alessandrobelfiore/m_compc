exception SymbolNotFound of Ast.identifier
exception DuplicateEntry of Ast.identifier

type var_value = { llvalue : Llvm.llvalue; is_arr : bool }

type scope =
  {
    name : string;
    variables : (Ast.identifier, var_value) Hashtbl.t;
    parent: scope option
  }

let empty_table =
  {
    name = "global";
    variables = Hashtbl.create 0;
    parent = None
  }

let begin_block table scope_name =
  {
    name = scope_name;
    variables = Hashtbl.create 0;
    parent = Some table
  }

let end_block table =
  match table.parent with
  | Some t -> t
  | None -> table

let add_entry id llval is_array table =
  let var = Hashtbl.find_opt table.variables id in
  match var with
  | Some _  -> raise (DuplicateEntry ("duplicate " ^ id))
  | None    -> 
    Hashtbl.add table.variables id ({llvalue = llval; is_arr = is_array});
  table

let rec lookup_is_array id table =
  let var = Hashtbl.find_opt table.variables id in
  match var with
  | Some v  -> v.is_arr
  | None    ->
    match table.parent with
    | Some p  -> lookup_is_array id p
    | None    -> raise (SymbolNotFound ("not found " ^ id))

let rec lookup_val id table =
  let var = Hashtbl.find_opt table.variables id in
  match var with
  | Some v  -> v.llvalue
  | None    ->
    match table.parent with
    | Some p  -> lookup_val id p
    | None    -> raise (SymbolNotFound ("not found " ^ id))

let is_function table =
  match table.name with
  | "if" | "else" | "while" | "for" | "do-while" | "block" -> false
  | _ -> true

let rec get_scope_name table =
  if is_function table 
    then table.name
  else
    match table.parent with
    | Some v -> get_scope_name v
    | None -> raise (SymbolNotFound "scope w/o name")
exception SymbolNotFound of Ast.identifier
exception DuplicateEntry of Ast.identifier

type scope
type var_value

val empty_table : scope (* creates a new empty table *)
val begin_block : scope -> Ast.identifier -> scope  (* creates a new scope with the given id *)
val end_block : scope -> scope  (* deletes the last scope *)
val add_entry : Ast.identifier -> Llvm.llvalue -> bool -> scope -> scope    (* add a new llvalue with the flag is_arr to the scope *)
val lookup_is_array : Ast.identifier -> scope -> bool   (* returns if the id is a an array as formal parameter *)
val lookup_val : Ast.identifier -> scope -> Llvm.llvalue    (* returns the llval with the given id *)
val is_function : scope -> bool (* returns if the given scope is a function scope *)
val get_scope_name : scope -> Ast.identifier    (* returns the scope name *)
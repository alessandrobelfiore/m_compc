exception SymbolNotFound of Ast.identifier
exception DuplicateEntry of Ast.identifier

type scope
type var_value

(** Returns a new empty table *)
val empty_table : scope

(** Returns a new scope with the given id *)
val begin_block : scope -> Ast.identifier -> scope  

(** Deletes the last scope *)
val end_block : scope -> scope  

(** Adds a new llvalue with the flag is_arr to the scope *)
val add_entry : Ast.identifier -> Llvm.llvalue -> bool -> scope -> scope    

(** Returns if the id is a an array as formal parameter *)
val lookup_is_array : Ast.identifier -> scope -> bool   

(** Returns the llval with the given id *)
val lookup_val : Ast.identifier -> scope -> Llvm.llvalue    

(** Returns if the given scope is a function scope *)
val is_function : scope -> bool 

(** Returns the scope name *)
val get_scope_name : scope -> Ast.identifier    
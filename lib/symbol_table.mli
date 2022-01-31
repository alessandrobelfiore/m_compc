exception DuplicateEntry of Ast.identifier
exception Semantic_error of Location.code_pos * string
type scope
type interface_t

(** Creates an empty scope *)
val empty_table : scope 

(** Adds a new scope with the given optional return type *)
val begin_block : scope -> Ast.typ option -> scope  

(** Deletes the last scope *)
val end_block : scope -> scope  

(** Adds a new typed id to the scope *)
val add_entry : Ast.identifier -> (Ast.typ * Ast.identifier option) -> scope -> Location.code_pos -> scope  

(** Returns the type bound to the given id *)
val lookup : Ast.identifier -> scope -> Location.code_pos -> (Ast.typ * Ast.identifier option)  

(** Returns the return type of the given scope *)
val get_rtype: scope -> Ast.typ option  

(** Creates an empty interfaces table *)
val empty_interfaces_t : interface_t    

(** Adds a list of member decl to the given interface*)
val add_methods : Ast.identifier -> Ast.typ Ast.member_decl list -> interface_t -> Location.code_pos -> unit    

(** Returns a list of member decl bound to the given id in the scope *)
val get_methods : Ast.identifier -> interface_t -> Ast.typ Ast.member_decl list 
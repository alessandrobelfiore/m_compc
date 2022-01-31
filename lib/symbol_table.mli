exception DuplicateEntry of Ast.identifier
exception Semantic_error of Location.code_pos * string
type scope
type interface_t

val empty_table : scope (* creates an empty scope *)
val begin_block : scope -> Ast.typ option -> scope  (* adds a new scope with the given optional return type *)
val end_block : scope -> scope  (* deletes the last scope *)
val add_entry : Ast.identifier -> (Ast.typ * Ast.identifier option) -> scope -> Location.code_pos -> scope  (* adds a new typed id to the scope *)
val lookup : Ast.identifier -> scope -> Location.code_pos -> (Ast.typ * Ast.identifier option)  (* returns the type bound to the given id *)
val get_rtype: scope -> Ast.typ option  (* returns the return type of the doven scope *)
val empty_interfaces_t : interface_t    (* creates an empty interfaces table *)
val add_methods : Ast.identifier -> Ast.typ Ast.member_decl list -> interface_t -> Location.code_pos -> unit    
(* adds a list of member decl to the given interface*)
val get_methods : Ast.identifier -> interface_t -> Ast.typ Ast.member_decl list (* returns a list of member decl bound to the given id in the scope *)
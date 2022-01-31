exception Semantic_error of Location.code_pos * string


(** returns the type of the Ast.node node *)
val type_of_node : ('a, Ast.typ) Ast.annotated_node -> Ast.typ

(** returns the type of the Ast.node node in a function call *)
val type_of_node_in_f : ('a, Ast.typ) Ast.annotated_node -> Ast.typ

(** returns the type of the variable decl vdecl *)
val type_of_vdecl : Ast.vdecl -> Ast.typ

(** checks if two function signatures match, returns a bool *)
val match_proto_decl : 'a Ast.member_decl -> 'a Ast.member_decl -> bool

(** returns the name of the given member if it's a function *)
val get_fname_from_member : 'a Ast.member_decl -> Ast.identifier

(** raises an exception if we don't find decl in the given list, returns () otherwise *)  
val find_decl_in_list : Ast.typ Ast.member_decl list -> Location.code_pos -> Ast.typ Ast.member_decl -> unit

(*** returns true if typ is a basic type, false otherwise *)
val check_basic_type : Ast.typ -> bool

(** returns () if typ is a valid type for a variable declaration, raises an exception otherwise *)
val check_vardecl_type : bool -> Ast.typ -> Location.code_pos -> unit

(** adds in the given scope the methods defined in the interfaces used *)
val add_methods_from_use : Ast.identifier list -> Symbol_table.scope -> Location.code_pos -> Symbol_table.interface_t -> Ast.identifier list -> unit

(** adds the given member decl in the scope if it's a function *)
val add_member_decl : Ast.identifier -> Symbol_table.scope -> Location.code_pos -> Ast.typ Ast.member_decl -> unit

(** adds the methods getint() and print() to the component scope *)
val add_prelude : Symbol_table.scope -> Location.code_pos -> unit

(** raises an exception if the given expr is not an Assign *)  
val check_expr_is_assign : 'a Ast.expr -> Location.code_pos -> unit

(** returns true if s2 is contained in s1, false otherwise *)  
val contains : string -> string -> bool
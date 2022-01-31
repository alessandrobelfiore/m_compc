exception Semantic_error of Location.code_pos * string

val type_of_node : ('a, Ast.typ) Ast.annotated_node -> Ast.typ
val type_of_node_in_f : ('a, Ast.typ) Ast.annotated_node -> Ast.typ
val type_of_vdecl : Ast.vdecl -> Ast.typ
val match_proto_decl : 'a Ast.member_decl -> 'a Ast.member_decl -> bool
val get_fname_from_member : 'a Ast.member_decl -> Ast.identifier
val find_decl_in_list : Ast.typ Ast.member_decl list -> Location.code_pos -> Ast.typ Ast.member_decl -> unit
val check_basic_type : Ast.typ -> bool
val check_vardecl_type : bool -> Ast.typ -> Location.code_pos -> unit
val add_methods_from_use : Ast.identifier list -> Symbol_table.scope -> Location.code_pos -> Symbol_table.interface_t -> Ast.identifier list -> unit
val add_member_decl : Ast.identifier -> Symbol_table.scope -> Location.code_pos -> Ast.typ Ast.member_decl -> unit
val add_prelude : Symbol_table.scope -> Location.code_pos -> unit
val check_expr_is_assign : 'a Ast.expr -> Location.code_pos -> unit
val contains : string -> string -> bool
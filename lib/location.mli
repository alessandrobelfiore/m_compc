type lexeme_pos = { line : int; start_column : int; end_column : int }
[@@deriving show,ord,eq]
type code_pos = { start_line : int; start_column: int ; end_line: int; end_column : int} 
[@@deriving show,ord,eq]


(** Returns the given lexing position formatted as a lexeme postion *)
val to_lexeme_position : Lexing.lexbuf -> lexeme_pos

(** Returns the given lexing position formatted as a code postion *)
val to_code_position : Lexing.position * Lexing.position -> code_pos

(** Returns a dummy value of all zeroes as a code position *)
val dummy_code_pos : code_pos

(** Returns a dummy value of all zeroes as a lexeme position *)
val dummy_lexeme_pos : lexeme_pos
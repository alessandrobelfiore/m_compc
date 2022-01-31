exception Syntax_error of Location.lexeme_pos * string

let parse = Parser.compilation_unit
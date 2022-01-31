{
 (* OCaml declaration*)   
 exception Lexing_error of Location.lexeme_pos * string
 open Parser

 let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl 

  let keyword_table =
    create_hashtable 17 [
      ("var", VAR);
      ("if", IF);
      ("return", RETURN);
      ("else", ELSE);
      ("while", WHILE);
      ("int", INT);
      ("char", CHAR);
      ("void", VOID);
      ("bool", BOOL);
      ("interface", INTERFACE);
      ("uses", USES);
      ("provides", PROVIDES);
      ("component", COMPONENT);
      ("connect", CONNECT);
      ("def", DEF);
      ("for", FOR);
      ("do", DO)
    ]
}

(* Declaration of regular expressions *)
let letter = ['a' - 'z'] | ['A' - 'Z']
let digit = ['0' - '9']
let identifier = ('_' | letter) (digit | letter | '_')*
let hexa = '0' ['x' 'X'] (['a' - 'f' 'A' - 'F']+ | digit+)
let int = digit+ | hexa
let char = (("'") (( [' ' - '~' ] ) as char_v) ("'"))

let bool = "true" | "false"
let op = '&' | '+' | '-' | '*' | '/'
            '%' | '=' | "==" | "!=" |
            '<' | '>' | "<=" | ">=" |
            "&&" | "||" | '!'

(* Declaration of scanner functions *)
rule next_token = parse
  | bool as b
    { let value = bool_of_string b
      in BOOL_VAL value }
  | char
    { CHAR_VAL char_v }
  | identifier as word
    { try
      let token = Hashtbl.find keyword_table word in
      token
      with Not_found ->
      let length = String.length word in
      (
        if length > 64 then
        raise (Lexing_error (Location.to_lexeme_position lexbuf, "identifier size overflow"));
        ID word
      )
      }
  | int as v
    { try 
        let value = int_of_string v in
          if (Int32.to_int (Int32.max_int) > value) && (Int32.to_int (Int32.min_int) < value) then INT_VAL value
          else raise (Lexing_error (Location.to_lexeme_position lexbuf, "int overflow"));
      with Failure _ ->
        raise (Lexing_error (Location.to_lexeme_position lexbuf, "int overflow")); 
    }
  (* Operators *)
  | "+"                 { PLUS }
  | "-"                 { MINUS }
  | "*"                 { TIMES }
  | "/"                 { DIV }
  | "%"                 { PERCENT }
  | "="                 { ASSIGN }
  | "=="                { EQUAL }
  | "!="                { NOTEQUAL }
  | "&"                 { REF }
  | "<"                 { LESS }
  | "<="                { LEQ }
  | ">"                 { GREATER }
  | ">="                { GEQ }
  | "&&"                { AND }
  | "||"                { OR }
  | '!'                 { NOT }
  | "++"                { INCR }
  | "--"                { SUB }
  (* Punctuation *)
  | '('                 { LBRACE }
  | ')'                 { RBRACE }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '{'                 { LCURLY }
  | '}'                 { RCURLY }
  | '.'                 { FULLSTOP }
  | ':'                 { COLON }
  | ';'                 { SEMICOLON }
  | ','                 { COMMA }
  (* Special characters *)
  | [' ' '\t']          { next_token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; next_token lexbuf }
  | "\'"                { next_token lexbuf }
  | "\b"                { next_token lexbuf }
  | "\\"                { next_token lexbuf }
  | "\r"                { next_token lexbuf }
  (* Other symbols *)
  | "<-"                { LINK }
  | "//" [^ '\n']*      { next_token lexbuf }
  | "/*"                { comments lexbuf }
  | eof                 { EOF }
  | _                   { raise (Lexing_error (Location.to_lexeme_position lexbuf, "Unrecognized character")) }

and comments = parse
  | "*/"                { next_token lexbuf }
  | _                   { comments lexbuf }
  | eof                 { raise (Lexing_error (Location.to_lexeme_position lexbuf, "Comment section not ended")) }
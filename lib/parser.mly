/**
 mComp parser specification 
 */
%{
  open Ast
  let (|@|) node loc = { node = node; annot = loc }
  (* exception Syntax_error of Location.lexeme_pos * string *)
  
  (* creates a CompilationUnit from a list of TopDecl *)
  let distribute_list list =
    let rec aux comp list =
      match list with
      | [] -> comp
      | x :: xs -> 
        match comp with
        | CompilationUnit
          {
            interfaces = i;
            components = c;
            connections = ll;
          } ->
          match x with
          | TopDeclComponent(y) ->
            aux
            (CompilationUnit {
              interfaces = i;
              components = y :: c;
              connections = ll;
            })
            xs
          | TopDeclInterface(y) ->
            aux
            (CompilationUnit {
              interfaces = y :: i;
              components = c;
              connections = ll;               
            })
            xs
          | TopDeclConnection(y) ->
            aux
            (CompilationUnit {
              interfaces = i;
              components = c;
              connections = y @ ll;               
            })
            xs
    in aux (CompilationUnit { interfaces = []; components = []; connections = []; }) list
%} 

/* Token declarations */
(* Keywords *)
%token VAR
%token IF
%token RETURN
%token ELSE
%token WHILE
%token INT
%token CHAR
%token VOID
%token BOOL
%token INTERFACE
%token USES
%token PROVIDES
%token COMPONENT
%token CONNECT
%token DEF
%token FOR
%token DO
%token EOF

(* Values *)
%token <string> ID
%token <int> INT_VAL
%token <bool> BOOL_VAL
%token <char> CHAR_VAL

(* Punctuation *)
%token LBRACE     "("
%token RBRACE     ")"
%token LBRACK     "["
%token RBRACK     "]"
%token LCURLY     "{"
%token RCURLY     "}"
%token FULLSTOP   "."
%token COLON      ":"
%token SEMICOLON  ";"
%token COMMA      ","

(* Operators *)
%token PLUS         "+"
%token MINUS        "-"
%token TIMES        "*"
%token DIV          "/"
%token PERCENT      "%"
%token ASSIGN       "="
%token EQUAL        "=="
%token NOTEQUAL     "!="
%token LESS         "<"
%token LEQ          "<="
%token GREATER      ">"
%token GEQ          ">="
%token REF          "&"
%token AND          "&&"
%token OR           "||"
%token NOT          "!"
%token SUB          "--"
%token INCR         "++"

%token LINK

/* Precedence and associativity specification */
%nonassoc     no_prec
%nonassoc     ELSE
%right    "="               
%left     "||"
%left     "&&"
%left     "=="  "!=" 
%nonassoc ">"  "<"  ">="  "<="
%left     "+"  "-" 
%left     "*"  "/"  "%"
%nonassoc "!"
%nonassoc NEG /* highest precedence  */

/* Start symbol */
%start compilation_unit
%type <Ast.located_compilation_unit> compilation_unit

%% 

/* Grammar Specification */
compilation_unit:
  | t = topDecl+ EOF
    { distribute_list t }

topDecl:
  | i = interfaces
    { TopDeclInterface i }
  | c = components
    { TopDeclComponent c }
  | l = links
    { TopDeclConnection l }

interfaces:
  | INTERFACE i = ID LCURLY decl = iMemberDecl+ RCURLY
    { InterfaceDecl{
      iname = i;
      declarations = decl;
    } |@| (Location.to_code_position $loc) }

components:
  | COMPONENT i = ID provide = provideClause? use = useClause? LCURLY def = cMemberDecl+ RCURLY
    { let prov = match provide with 
      | None -> []
      | Some(v) -> v
      in
      let us = match use with 
      | None -> []
      | Some(v) -> v
      in
      ComponentDecl{
      cname = i;
      uses = us;
      provides = prov;
      definitions = def; 
    } |@| (Location.to_code_position $loc) }

links:
  | CONNECT l = link
    { [l] }
  | CONNECT LCURLY l = link+ RCURLY
    { l }
  
link:
  | id1 = ID FULLSTOP id2 = ID LINK id3 = ID FULLSTOP id4 = ID SEMICOLON?
    { Link(id1, id2, id3, id4) }

iMemberDecl:
  | VAR v = vdecl SEMICOLON
    { VarDecl(v) |@| (Location.to_code_position $loc) }
  | f = funProto SEMICOLON
    { FunDecl(f) |@| (Location.to_code_position $loc) }

provideClause:
  | PROVIDES list = separated_nonempty_list(COMMA, ID)
    { list }

useClause:
  | USES list = separated_nonempty_list(COMMA, ID)
    { list } 

vdecl:
  | id = ID COLON t = typ
    { (id, t) }
  
funProto:
  | DEF i = ID LBRACE list = separated_list(COMMA, vdecl) RBRACE t = returnType? (* allow only basic types FIXME *)
    { let tt = match t with
      | None    -> TVoid
      | Some(v) -> v in
      {
        rtype = tt;
        fname = i;
        formals = list;
        body = None
      }
    }

returnType:
  | COLON t = typ
    { t }

cMemberDecl:
  | VAR v = vdecl SEMICOLON
    { VarDecl(v) |@| (Location.to_code_position $loc) }
  | f = funDecl
    { FunDecl(f) |@| (Location.to_code_position $loc) }

funDecl:
  | DEF i = ID LBRACE list = separated_list(COMMA, vdecl) RBRACE t = returnType? b = stmt (* allow only basic types FIXME *)
    { let tt = match t with
      | None    -> TVoid
      | Some(v) -> v in
      {
      rtype = tt;
      fname = i;
      formals = list;
      body = Some (b);
    } }

typ:
  | t = basic_type
    { t }
  | REF t = basic_type
    { TRef (t) }
  | t = typ LBRACK RBRACK
    { TArray(t, None) }
  | t = typ LBRACK i = INT_VAL RBRACK
    { TArray(t, Some i) }
  | INTERFACE i = ID
    { TInterface(i) }
  | COMPONENT i = ID
    { TComponent(i) }

basic_type:
  | INT
    { TInt }
  | CHAR
    { TChar }
  | VOID
    { TVoid }
  | BOOL
    { TBool }

stmt:
  | RETURN e = expr? SEMICOLON
    { Return(e) |@| (Location.to_code_position $loc) }
  | e = expr SEMICOLON
    { Expr(e) |@| (Location.to_code_position $loc) }
  | LCURLY s = stmtordec+ RCURLY
    { Block(s) |@| (Location.to_code_position $loc) }
  | DO s = stmt WHILE LBRACE e = expr RBRACE SEMICOLON
    { DoWhile(e, s) |@| (Location.to_code_position $loc) }
  | WHILE LBRACE e = expr RBRACE s = stmt
    { While(e, s) |@| (Location.to_code_position $loc) }
  | FOR LBRACE e1 = expr? SEMICOLON e2 = expr SEMICOLON e3 = expr? RBRACE s = stmt
    { For(e1, e2, e3, s) |@| (Location.to_code_position $loc) }
  | FOR LBRACE e1 = expr? SEMICOLON e2 = expr SEMICOLON e3 = expr? RBRACE SEMICOLON
    { For(e1, e2, e3, Skip |@| (Location.to_code_position $loc) ) |@| (Location.to_code_position $loc) }
  | IF LBRACE e = expr RBRACE s1 = stmt ELSE s2 = stmt
    { If(e, s1, s2) |@| (Location.to_code_position $loc) }
  | IF LBRACE e = expr RBRACE s = stmt %prec no_prec
    { If(e, s, Skip |@| (Location.to_code_position $loc) ) |@| (Location.to_code_position $loc) }
  | LCURLY RCURLY
    { Skip |@| (Location.to_code_position $loc) }

stmtordec:
  | VAR v = vdecl SEMICOLON
    { LocalDecl(v) |@| (Location.to_code_position $loc) }
  | s = stmt
    { Stmt(s) |@| (Location.to_code_position $loc) }

expr:
  | i = INT_VAL
    { ILiteral i |@| (Location.to_code_position $loc) }
  | c = CHAR_VAL
    { CLiteral c |@| (Location.to_code_position $loc) }
  | b = BOOL_VAL
    { BLiteral b |@| (Location.to_code_position $loc) }
  | LBRACE e = expr RBRACE 
    { e }
  | REF lv = lvalue
    { Address(lv) |@| (Location.to_code_position $loc) }
  | lv = lvalue ASSIGN e = expr
    { Assign(lv, e) |@| (Location.to_code_position $loc) }
  | f = ID LBRACE list = separated_list(COMMA, expr) RBRACE
    { Call(None, f, list) |@| (Location.to_code_position $loc) }
  | lv = lvalue 
    { LV(lv) |@| (Location.to_code_position $loc) }
  (* Unary Ops *)
  | NOT e = expr
    { UnaryOp(Not, e) |@| (Location.to_code_position $loc) }
  | MINUS e = expr %prec NEG
    { UnaryOp(Neg, e) |@| (Location.to_code_position $loc) }
  | INCR lv = lvalue
    { UnaryOp(PreIncr, LV(lv) |@| (Location.to_code_position $loc)) |@| (Location.to_code_position $loc) }
  | SUB lv = lvalue
    { UnaryOp(PreSub, LV(lv) |@| (Location.to_code_position $loc)) |@| (Location.to_code_position $loc) }
  | lv = lvalue INCR
    { UnaryOp(PostIncr, LV(lv) |@| (Location.to_code_position $loc)) |@| (Location.to_code_position $loc) }
  | lv = lvalue SUB
    { UnaryOp(PostSub, LV(lv) |@| (Location.to_code_position $loc)) |@| (Location.to_code_position $loc) }
  (* Binary Ops *)
  | e1 = expr PLUS e2 = expr
    { BinaryOp(Add, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr MINUS e2 = expr
    { BinaryOp(Sub, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr TIMES e2 = expr
    { BinaryOp(Mult, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr DIV e2 = expr
    { BinaryOp(Div, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr PERCENT e2 = expr
    { BinaryOp(Mod, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr EQUAL e2 = expr
    { BinaryOp(Equal, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr NOTEQUAL e2 = expr
    { BinaryOp(Neq, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr LESS e2 = expr
    { BinaryOp(Less, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr LEQ e2 = expr
    { BinaryOp(Leq, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr GREATER e2 = expr
    { BinaryOp(Greater, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr GEQ e2 = expr
    { BinaryOp(Geq, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr AND e2 = expr
    { BinaryOp(And, e1, e2) |@| (Location.to_code_position $loc) }
  | e1 = expr OR e2 = expr
    { BinaryOp(Or, e1, e2) |@| (Location.to_code_position $loc) }

lvalue:
  | i = ID
    { AccVar(None, i) |@| (Location.to_code_position $loc) }
  | i = lvalue LBRACK e = expr RBRACK
    { AccIndex(i, e) |@| (Location.to_code_position $loc) }
open Ast
exception LinkingError of string

let (|@|) node ttype = { node = node; annot = ttype }

(* returns true if interface is used by component in comp_decls *)
let rec find_use interface component comp_decls =
  match comp_decls with
  | [] -> false
  | c :: xs ->
    match c.node with
    | ComponentDecl 
      {
        cname = name;
        uses = use;
        _
      } ->
        if name <> component then
          find_use interface component xs
        else if List.find_opt (fun name -> name = interface) use != None then
          true
        else false
(* returns true if interface is provided by component in comp_decls *)
let rec find_provide interface component comp_decls = 
  match comp_decls with
  | [] -> false
  | c :: xs ->
    match c.node with
    | ComponentDecl 
      {
        cname = name;
        provides = provide;
        _
      } ->
        if name <> component then
          find_provide interface component xs
        else if List.find_opt (fun name -> name = interface) provide != None then
          true
        else false
(* checks if a link is well typed *)
let check_connection components link = 
  match link with
  | Link(a, b, c, d) ->
    if b <> d then
      raise (LinkingError("Interfaces must match: " ^ b ^ " : " ^ d))
    else if a = c then
      raise (LinkingError("Must link two different components"))
    else if (find_use b a components) = false then
      raise (LinkingError("Linked Interface must be used by the component"))
    else if (find_provide d c components) = false then
      raise (LinkingError("Linked Interface must be provided by the component"))
    else ()
(* returns an Ast.lv qualified with component, if they were qualified with interface *)
let rec qualify_lv lv interface component =
  let t = lv.annot in
  match lv.node with
  | AccIndex(lv, e) ->
    let qualified_lv = qualify_lv lv interface component in
    let qualified_e = qualify_expr interface component e in
    AccIndex(qualified_lv, qualified_e) |@| t
  | AccVar(_interface, id) ->
    match _interface with
    | None -> AccVar(_interface, id) |@| t
    | Some _interface_n ->
      if _interface_n = interface 
        then AccVar(Some component, id) |@| t
      else AccVar(_interface, id) |@| t
(* returns an Ast.stmtordec qualified with component, if they were qualified with interface *)
and qualify_stmt_or_dec interface component stmt =
  let t = stmt.annot in
  match stmt.node with
  | LocalDecl(vdecl) ->
    LocalDecl(vdecl) |@| t
  | Stmt(s) ->
    let qualified_s = qualify_stmt s interface component in
    Stmt(qualified_s) |@| t
(* returns an Ast.expr qualified with component, if they were qualified with interface *)
and qualify_expr interface component exp =
  let t = exp.annot in
  match exp.node with
  | LV(lv) ->
    let qualified_lv = qualify_lv lv interface component in
    LV(qualified_lv) |@| t
  | Assign(lv, e) ->
    let qualified_lv = qualify_lv lv interface component in
    let qualified_e = qualify_expr interface component e in
    Assign(qualified_lv, qualified_e) |@| t
  | ILiteral(v) -> ILiteral(v) |@| t
  | CLiteral(v) -> CLiteral(v) |@| t
  | BLiteral(v) -> BLiteral(v) |@| t
  | UnaryOp(op, e) ->
    let qualified_e = qualify_expr interface component e in
    UnaryOp(op, qualified_e) |@| t
  | BinaryOp(op, e1, e2) ->
    let qualified_e1 = qualify_expr interface component e1 in
    let qualified_e2 = qualify_expr interface component e2 in
    BinaryOp(op, qualified_e1, qualified_e2) |@| t
  | Address(lv) ->
    let qualified_lv = qualify_lv lv interface component in
    Address(qualified_lv) |@| t
  | Call(_interface, fname, args) ->
    let qualified_args = List.map (qualify_expr interface component) args in
    match _interface with
    | None -> Call(_interface, fname, qualified_args) |@| t
    | Some _interface_n ->
      if _interface_n = interface
        then Call(Some component, fname, qualified_args) |@| t
      else Call(_interface, fname, qualified_args) |@| t
(* returns an Ast.stmt qualified with component, if they were qualified with interface *)
and qualify_stmt (stmt: Ast.typ Ast.stmt) interface component =
  let t = stmt.annot in
  match stmt.node with
  | If(e, s1, s2) ->
    let qualified_e = qualify_expr interface component e in
    let qualified_s1 = qualify_stmt s1 interface component in
    let qualified_s2 = qualify_stmt s2 interface component in
    If(qualified_e, qualified_s1, qualified_s2) |@| t
  | While(e, s) ->
    let qualified_e = qualify_expr interface component e in
    let qualified_s = qualify_stmt s interface component in
    While(qualified_e, qualified_s) |@| t
  | For(e1, e2, e3, s) ->
    let qualified_e1 =
      match e1 with
      | None -> None
      | Some exp ->
        Some (qualify_expr interface component exp) in
    let qualified_e3 =
      match e3 with
      | None -> None
      | Some exp ->
        Some (qualify_expr interface component exp) in
    let qualified_e2 = qualify_expr interface component e2 in
    let qualified_s = qualify_stmt s interface component in
    For(qualified_e1, qualified_e2, qualified_e3, qualified_s) |@| t
  | Expr(e) ->
    let qualified_e = qualify_expr interface component e in
    Expr(qualified_e) |@| t
  | Return(e) ->
    (
    match e with
    | None -> Return(None) |@| t
    | Some exp -> Return(Some (qualify_expr interface component exp)) |@| t)
  | Block(ss) ->
    let qualified_ss = List.map (qualify_stmt_or_dec interface component) ss in
    Block(qualified_ss) |@| t
  | _ -> stmt
(* returns an Ast.member_decl qualified with component, if they were qualified with interface *)
let qualify_member_decl interface component decl =
  match decl.node with
  | FunDecl
    {
      rtype = typ;
      fname = name;
      formals = f;
      body = b;
    } ->  
    let qualified_body = 
      (
        match b with
        | None -> None
        | Some bb -> Some (qualify_stmt bb interface component)
      ) in
      FunDecl
      {
        rtype = typ;
        fname = name;
        formals = f;
        body = qualified_body;
      } |@| decl.annot
    | VarDecl(vdecl) ->
      VarDecl(vdecl) |@| decl.annot
(* qualify names in the given component using the links *)
let qualify_component links component =
  match component.node with
  | ComponentDecl
  {
    cname = name;
    uses = use;
    provides = provide;
    definitions = defs;
  } ->
  (* foreach link if a == name *)
  let qualified_defs =
    let rec aux _links q_defs =
      match _links with
      | [] -> q_defs
      | Link(a, b, c, _) :: xs ->
        if a = name 
          then aux xs (List.map (qualify_member_decl b c) q_defs)
        else aux xs q_defs
    in aux links defs
  in
  ComponentDecl
  {
    cname = name;
    uses = use;
    provides = provide;
    definitions = qualified_defs;
  } |@| component.annot

let wire_components ast =
  match ast with
  | CompilationUnit
    {
      interfaces = i;
      components = comps;
      connections = links;
    } ->
      (* foreach connection a.b <- c.b *)
      (* find in component a all methods qualified by b (uses b) and substitute with c *)
      let _ = List.iter (check_connection comps) links in
      let qualified_components = List.map (qualify_component links) comps in
      CompilationUnit
      {
        interfaces = i;
        components = qualified_components;
        connections = links;
      }
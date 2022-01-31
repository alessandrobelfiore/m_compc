open Ast
open Utils

let (|@|) node ttype = { node = node; annot = ttype }

(* Shorthand for Llvm module *)
module L = Llvm

(** Global LLVM context *)
let llcontext = L.global_context ()

(** LLVM Mcomp module *)
let llvm_mcomp = L.create_module llcontext "Mcomp-module"

(** Environment *)
let global_env = ref Ll_table.empty_table

(* global env for qualified named functions *)
let fun_env = ref Ll_table.empty_table

(* name of the component currently analyzed *)
let current_component : string ref = ref ""

(* flag to manage automatic dereferencing *)
let can_deref : bool ref = ref false

(* LLVM IR types *)
let int_type  = L.i32_type llcontext
let bool_type = L.i1_type llcontext
let char_type = L.i8_type llcontext
let void_type = L.void_type llcontext
let ref_type lltype = L.pointer_type lltype
let array_type lltype dim =
  match dim with
  | None    -> L.array_type lltype 1
  | Some v  -> L.array_type lltype v

(* Adding the prelude functions to the module *)
let init = 
  let print_type = L.function_type void_type [|int_type|] in
  let print_f = L.declare_function "Prelude_print" print_type llvm_mcomp in
  fun_env := Ll_table.add_entry "Prelude_print" print_f false !fun_env;
  let getint_type = L.function_type int_type [||] in
  let getint_f = L.declare_function "Prelude_getint" getint_type llvm_mcomp in
  fun_env := Ll_table.add_entry "Prelude_getint" getint_f false !fun_env;
  ()

(* A table mapping a binary operator in the LLVM function that implements it + label *)
let binary_operators = 
  [ Add,    (L.build_add, "add")
  ; Mult,    (L.build_mul, "mul")
  ; Sub,    (L.build_sub, "sub")
  ; Div,    (L.build_sdiv, "div")
  ; Mod,    (L.build_srem, "mod")
  ; Less,    (L.build_icmp L.Icmp.Slt, "less")
  ; Leq,   (L.build_icmp L.Icmp.Sle, "less_equal")
  ; Greater,    (L.build_icmp L.Icmp.Sgt, "greater")
  ; Geq,   (L.build_icmp L.Icmp.Sge, "greater_equal")
  ; Equal,   (L.build_icmp L.Icmp.Eq, "equal")
  ; Neq,   (L.build_icmp L.Icmp.Ne, "not_equal")
  ; And,   (L.build_and, "and")
  ; Or,   (L.build_or, "or")
  ]

(* Translate a Mcomp type into a LLVM IR one, 
    returns lltype*)
let rec lltype_of_type = function 
  | Ast.TInt   -> int_type
  | Ast.TBool  -> bool_type
  | Ast.TChar  -> char_type
  | Ast.TVoid  -> void_type
  | Ast.TRef t -> ref_type (lltype_of_type t)
  | Ast.TArray (t, dim) -> array_type (lltype_of_type t) dim
  | _ -> failwith "not implemented"
(* Converts types to lltypes, using ref type to represent arrays
    (expects a reference to the first element in the array) *)
let lltype_of_param = function
  | Ast.TArray (t, _) -> ref_type (lltype_of_type t)
  | t -> lltype_of_type t
(* Converts expression passed in a function call to the correct llvm type *)
let rec translate_fun_exp_to_llvm exp builder =
  match exp.node with
  | Ast.LV acc -> translate_fun_acc_to_llvm acc builder
  | _ -> codegen_exp exp builder
(* Generates the correct variable access depending on the
    type of the parameter passed to the function call *)
and translate_fun_acc_to_llvm acc builder =
  match acc.node with
  | Ast.AccVar (_, id) -> (
    let llval = Ll_table.lookup_val id !global_env in
    let is_arr = Ll_table.lookup_is_array id !global_env in
    let lltyp = L.type_of llval in
    let pos = L.const_int int_type 0 in
    match L.classify_type lltyp with
    | L.TypeKind.Pointer -> ( 
      match L.classify_type (L.element_type lltyp) with
      | L.TypeKind.Array -> (* passing local array*)
        L.build_in_bounds_gep llval [|pos; pos|] "" builder
        (* trying to auto-dereference, an array present as formal parameter
          is represented as a pointer, hence we must check that we
          do not accidentally try to dereference it *)
      | L.TypeKind.Pointer ->
        ( if is_arr then 
          L.build_load llval "" builder
        else  
          let ptr = L.build_load llval "" builder in
          L.build_load ptr "" builder
        )
      | _ -> L.build_load llval "" builder )
    | _ -> L.build_load llval "" builder)
  | _ ->
    codegen_exp (LV (acc) |@| acc.annot) builder
    (* let llval = gen_lv_access acc builder in
    let lltyp = L.type_of llval in
      match L.classify_type lltyp with
      (* trying to auto-dereference *)
      | L.TypeKind.Pointer ->
        (match L.classify_type (L.element_type lltyp) with
        | L.TypeKind.Pointer ->
          let ptr = L.build_load llval "" builder in
          L.build_load ptr "" builder
        | _ -> L.build_load llval "" builder)
      | _ -> 
        L.build_load llval "" builder *)

(* returns the default llvalue of a type *)
and default_value_of_type ast_type =
  match ast_type with
  | TInt -> L.const_int int_type 0
  | TBool -> L.const_int bool_type 0
  | TChar -> L.const_int char_type 0
  | TRef t ->
    L.const_pointer_null (ref_type (lltype_of_type t))
  | TArray (t, dim) -> (
    (match dim with
    | Some v ->
        let size = L.const_int int_type v in
        L.const_array (lltype_of_type t) [|size|]
    | None ->
        let size = L.const_int int_type 0 in
        L.const_array (lltype_of_type t) [|size|] ))
  | _ -> failwith "trying to get a non-permitted def value"
(* Generates code for an access from a Ast.LV, 
    returns llvalue *)
and gen_lv_access lvalue builder =
  match lvalue.node with
  | Ast.AccVar (component, id) ->
    (
      match component with
      | None -> Ll_table.lookup_val id !global_env
      | Some c -> Ll_table.lookup_val (c ^ id) !global_env
    )
  | Ast.AccIndex (lv, e) ->
    match lv.node with
    | Ast.AccVar (component, id) ->
      let name = 
        (match component with
        | None -> id
        | Some c -> c ^ id
        ) in
      let llvar = Ll_table.lookup_val name !global_env in
      let offset = codegen_exp e builder in
      let zero_index = L.const_int int_type 0 in
      let index = L.build_sext offset int_type "" builder in
      let variable_type = L.type_of llvar in
      (match L.classify_type variable_type with
      | L.TypeKind.Pointer -> (
        match L.classify_type (L.element_type variable_type) with
        | L.TypeKind.Array -> (* accessing local array *)
          Llvm.build_in_bounds_gep llvar [|zero_index; index|] "" builder
        | _ ->  (* accessing formal parameter array *)
          let load_val = Llvm.build_load llvar "" builder in
          let pointer = Llvm.build_in_bounds_gep load_val [|index|] "" builder in
          pointer )
      | _ ->
        let pointer = Llvm.build_in_bounds_gep llvar [|zero_index; index|] "" builder in
        pointer)
  | _ -> failwith "should not be reachable"
    
(* Generates code for an Ast.expr expression,
    returns an llvalue*)
and codegen_exp exp builder =
  match exp.node with
  | Ast.ILiteral i ->
    L.const_int int_type i
  | Ast.CLiteral c ->
    L.const_int char_type (int_of_char c)
  | Ast.BLiteral b ->
    L.const_int bool_type (if b then 1 else 0)
  | Ast.BinaryOp (op, e1, e2) ->
    let gen_exp1 = codegen_exp e1 builder in
    let gen_exp2 = codegen_exp e2 builder in
    let (llvm_operator, label) = List.assoc op binary_operators in 
    llvm_operator gen_exp1 gen_exp2 label builder
  | Ast.UnaryOp (op, e) -> 
    (
      match op with
      | Ast.Neg ->
        let gen_exp = codegen_exp e builder in
        L.build_neg gen_exp "" builder
      | Ast.Not ->
        let gen_exp = codegen_exp e builder in
        L.build_not gen_exp "not" builder
      | Ast.PreIncr ->
        let var =
          match e.node with
          | LV lv -> gen_lv_access lv builder
          | _ -> failwith "Should have been caught in semant check"
        in
        let old_value = Llvm.build_load var "" builder in
        let constant_one = Llvm.const_int int_type 1 in
        let new_value = Llvm.build_add old_value constant_one "" builder in
        let _ = Llvm.build_store new_value var builder in
        new_value
      | Ast.PreSub ->
        let var =
          match e.node with
          | LV lv -> gen_lv_access lv builder
          | _ -> failwith "Should have been caught in semant check"
        in
        let old_value = Llvm.build_load var "" builder in
        let constant_one = Llvm.const_int int_type 1 in
        let new_value = Llvm.build_sub old_value constant_one "" builder in
        let _ = Llvm.build_store new_value var builder in
        new_value
      | Ast.PostIncr ->
        let var =
          match e.node with
          | LV lv -> gen_lv_access lv builder
          | _ -> failwith "Should have been caught in semant check"
        in
        let old_value = Llvm.build_load var "" builder in
        let constant_one = Llvm.const_int int_type 1 in
        let new_value = Llvm.build_add old_value constant_one "" builder in
        let _ = Llvm.build_store new_value var builder in
        old_value
      | Ast.PostSub ->
        let var =
          match e.node with
          | LV lv -> gen_lv_access lv builder
          | _ -> failwith "Should have been caught in semant check"
        in
        let old_value = Llvm.build_load var "" builder in
        let constant_one = Llvm.const_int int_type 1 in
        let new_value = Llvm.build_sub old_value constant_one "" builder in
        let _ = Llvm.build_store new_value var builder in
        old_value 
    )
  | Ast.Assign (lvalue, e) ->
    can_deref := true;  (* set auto-dereferencing (val = ptr) *)
    let gen_exp = codegen_exp e builder in
    can_deref := false;
    let typ_exp = type_of_node e in
    let typ_lv = type_of_node lvalue in
    (
      match typ_lv with
      | TRef(t) -> (* automatic dereferencing (ptr = ptr + val) *)
        if t = typ_exp then
          let ptr = gen_lv_access lvalue builder in
          let loc = L.build_load ptr "" builder in
          let _ = L.build_store gen_exp loc builder in
          gen_exp
        else 
          let gen_var = gen_lv_access lvalue builder in
          let _ = L.build_store gen_exp gen_var builder in
          gen_exp
      | _ -> 
        let gen_var = gen_lv_access lvalue builder in
        let _ = L.build_store gen_exp gen_var builder in
        gen_exp
    )
  | Ast.Address access ->
    let gen_lv = gen_lv_access access builder in
    gen_lv
  | Ast.LV lv ->
    let llval = gen_lv_access lv builder in
    (* if we are analyzing rhs of an assignment, auto-deref ptrs *)
    if !can_deref then
      let lltyp = L.type_of llval in
      match L.classify_type lltyp with
      | L.TypeKind.Pointer ->
        (match L.classify_type (L.element_type lltyp) with
        | L.TypeKind.Pointer ->
          let ptr = L.build_load llval "" builder in
          L.build_load ptr "" builder
        | _ -> L.build_load llval "" builder)
      | _ -> 
        L.build_load llval "" builder
    else L.build_load llval "" builder
  | Ast.Call (component, fname, params) ->
    let qualified_n =
      (* function "main" in unique and not qualified with a component *)
      (match component, fname with
      | None, "main" -> fname
      | None, _ -> !current_component ^ "_" ^ fname 
      | Some _, "main" -> fname
      | Some c, _ -> c ^ "_" ^ fname)
      in
    let fun_decl = Ll_table.lookup_val qualified_n !fun_env in
    let fun_type = L.type_of fun_decl in
    let stringyfied_t = L.string_of_lltype fun_type in
    can_deref := true;
    let ll_params = List.map (fun e -> translate_fun_exp_to_llvm e builder) params in
    can_deref := false;
    if contains stringyfied_t "void" then
      L.build_call fun_decl (Array.of_list ll_params) "" builder
    else 
      L.build_call fun_decl (Array.of_list ll_params) qualified_n builder

(* Codegens a local decl, returns a builder *)
and codegen_stmt_or_dec stmt builder =
  match stmt.node with
  | Ast.LocalDecl (id, t) -> (
    match t with
    | Ast.TArray (arr_t, size) ->
        let ll_array = array_type (lltype_of_type arr_t) size in
        let ll_value = L.build_alloca ll_array id builder in
        global_env := Ll_table.add_entry id ll_value true !global_env;
        builder
    | _ ->
        let ll_value = L.build_alloca (lltype_of_type t) id builder in
        global_env := Ll_table.add_entry id ll_value false !global_env;
        builder
    )
  | Ast.Stmt stmt -> codegen_stmt stmt builder

(* Codegens a Ast.stmt, returns builder *)
and codegen_stmt stmt builder =
  match stmt.node with 
  | Ast.If (e, s1, s2) ->
    let f_name = Ll_table.get_scope_name !global_env in
    let fun_def = Ll_table.lookup_val f_name !global_env in
    (* open a new scope "if" for then branch *)
    global_env := Ll_table.begin_block !global_env "if";
    let b_then = L.append_block llcontext "then" fun_def in
    let b_else = L.append_block llcontext "else" fun_def in
    let b_cont = L.append_block llcontext "cont" fun_def in
    let guard = codegen_exp e builder in
    let builder_then = L.builder_at_end llcontext b_then in
    let new_builder_then = codegen_stmt s1 builder_then in
    add_br_terminal new_builder_then b_cont;
    global_env := Ll_table.end_block !global_env;
    (* close "if" scope *)
    (* open a new scope "if" for else branch *)
    global_env := Ll_table.begin_block !global_env "if";
    let builder_else = L.builder_at_end llcontext b_else in
    let new_builder_else = codegen_stmt s2 builder_else in
    add_br_terminal new_builder_else b_cont;
    (* close "if" scope *)
    global_env := Ll_table.end_block !global_env;
    let _ = L.build_cond_br guard b_then b_else builder in
    L.builder_at_end llcontext b_cont
  | Ast.While (e, s) ->
    let f_name = Ll_table.get_scope_name !global_env in
    let fun_def = Ll_table.lookup_val f_name !global_env in
    (* open a new scope "while" *)
    global_env := Ll_table.begin_block !global_env "while";
    let b_guard = L.append_block llcontext "guard" fun_def in
    let b_while = L.append_block llcontext "while" fun_def in
    let b_cont = L.append_block llcontext "cont" fun_def in
    let _ = L.build_br b_guard builder in
    let builder_while = L.builder_at_end llcontext b_while in
    let new_builder_while = codegen_stmt s builder_while in
    add_br_terminal new_builder_while b_guard;
    (* close "while" scope *)
    global_env := Ll_table.end_block !global_env;
    let builder_guard = L.builder_at_end llcontext b_guard in
    let guard = codegen_exp e builder_guard in
    let _ = L.build_cond_br guard b_while b_cont builder_guard in
    L.builder_at_end llcontext b_cont
  | Ast.DoWhile (e, s) ->
    let f_name = Ll_table.get_scope_name !global_env in
    let fun_def = Ll_table.lookup_val f_name !global_env in
    (* open a new scope "while" *)
    global_env := Ll_table.begin_block !global_env "do_while";
    let b_guard = L.append_block llcontext "guard" fun_def in
    let b_while = L.append_block llcontext "do_while" fun_def in
    let b_cont = L.append_block llcontext "cont" fun_def in
    let _ = L.build_br b_while builder in
    let builder_while = L.builder_at_end llcontext b_while in
    let new_builder_while = codegen_stmt s builder_while in
    add_br_terminal new_builder_while b_guard;
    (* close "while" scope *)
    global_env := Ll_table.end_block !global_env;
    let builder_guard = L.builder_at_end llcontext b_guard in
    let guard = codegen_exp e builder_guard in
    let _ = L.build_cond_br guard b_while b_cont builder_guard in
    L.builder_at_end llcontext b_cont
  | Ast.For (e1, e2, e3, s) ->
    let f_name = Ll_table.get_scope_name !global_env in
    let fun_def = Ll_table.lookup_val f_name !global_env in
    (* open a new scope "for" *)
    global_env := Ll_table.begin_block !global_env "for";
    (* find iterator via the guard expression *)
    let iterator = find_lv_in_guard e2.node in
    let default_start = Assign(iterator, (ILiteral 0) |@| TInt) |@| TInt in
    let _ = 
      (match e1 with
        | None    -> codegen_exp default_start builder
        | Some v  -> codegen_exp v builder) in
    let b_guard = L.append_block llcontext "guard" fun_def in
    let b_for = L.append_block llcontext "for" fun_def in
    let b_cont = L.append_block llcontext "cont" fun_def in
    let _ = L.build_br b_guard builder in
    (* builds the for body *)
    let builder_for = L.builder_at_end llcontext b_for in
    let new_builder_for = codegen_stmt s builder_for in
    (* close "for" scope *)
    (* builds the step and guard *)
    let default_step = Assign (iterator, (BinaryOp(Add, LV(iterator) |@| TInt, (ILiteral 1) |@| TInt)) |@| TInt) |@| TInt in
    let _ = 
      (match e3 with
        | None    -> codegen_exp default_step new_builder_for
        | Some v  -> codegen_exp v new_builder_for) in
    add_br_terminal new_builder_for b_guard;
    global_env := Ll_table.end_block !global_env;
    let builder_guard = L.builder_at_end llcontext b_guard in
    let guard = codegen_exp e2 builder_guard in
    let _ = L.build_cond_br guard b_for b_cont builder_guard in
    L.builder_at_end llcontext b_cont
  | Ast.Expr e ->
    let _ = codegen_exp e builder in
    builder
  | Ast.Return e ->
    (
      match e with 
      | Some exp -> 
        let r_value = codegen_exp exp builder in
        let _ = L.build_ret r_value builder in
        builder
      | None ->
        let _ = L.build_ret_void builder in
        builder
    )
  | Ast.Block decls ->
    let rec iterate declarations builder =
      match declarations with
      | [] -> builder
      | x :: xs ->
        let new_builder = codegen_stmt_or_dec x builder in
        iterate xs new_builder
    in
    global_env := Ll_table.begin_block !global_env "block";
    let new_builder = iterate decls builder in
    global_env := Ll_table.end_block !global_env;
    new_builder
  | Ast.Skip -> builder

(* Adds a br command to the specified label, if the block is not terminated *)
and add_br_terminal builder label =
  match Llvm.block_terminator (Llvm.insertion_block builder) with
  | Some _ -> ()
  | None -> ignore (L.build_br label builder)

(* Finds the variable used in the lhs of a binary operation *)
and find_lv_in_guard expr =
  match expr with 
  | BinaryOp(_, e, _) ->
    (match e.node with
    | LV(var) -> var
    | exp -> find_lv_in_guard exp)
  | _ -> failwith "invalid for"

(* Allocate for list of params, returns () *)
let setup_params formals fun_decl builder =
  let fun_params = Llvm.params fun_decl in
  List.iteri
    (
      fun index (id, typ) ->
      match typ with
      | Ast.TArray (t, _) ->
          let pointer = ref_type (lltype_of_type t) in
          let param = Llvm.build_alloca pointer id builder in
          let _ = L.build_store fun_params.(index) param builder in
          global_env := Ll_table.add_entry id param true !global_env
      | _ ->
          let param = L.build_alloca (lltype_of_type typ) id builder in
          let _ = L.build_store fun_params.(index) param builder in
          global_env := Ll_table.add_entry id param false !global_env
    )
    formals

(* codegen a function definition, returns () *)
let rec codegen_func_dec c_name fun_def =
  let f_name = 
    (match fun_def.fname with
    | "main" -> "main"
    | _ -> c_name ^ "_" ^ fun_def.fname) 
  in
  let function_proto =  
    (match L.lookup_function f_name llvm_mcomp with
    | None -> failwith "error"
    | Some f -> f)
  in
  let bb = L.append_block llcontext "entry" function_proto in
  global_env := Ll_table.begin_block !global_env f_name;
  let builder = L.builder_at_end llcontext bb in
  setup_params fun_def.formals function_proto builder;
  let body = 
    (match fun_def.body with
    | None -> Skip |@| TVoid
    | Some b -> b)
  in
  let last = codegen_stmt body builder in
  check_terminal last fun_def.rtype;
  global_env := Ll_table.end_block !global_env;
  ()

(* if return is omitted, add it *)
and check_terminal builder r_type =
  match L.block_terminator (L.insertion_block builder) with
  | Some _ -> ()
  | None ->
    match r_type with
    | TVoid -> 
      ignore (L.build_ret_void builder)
    | _ ->
      let llval = L.const_int (lltype_of_type r_type) 0 in
      ignore (L.build_ret llval builder)

(* codegens a member declaration *)
let codegen_member_decl c_name decl =
  match decl.node with
  | FunDecl d ->
    codegen_func_dec c_name d
  | VarDecl(id, typ) ->
    let default = default_value_of_type typ in
    let llval = L.define_global id default llvm_mcomp in
    let _ = Ll_table.add_entry id llval false !global_env in
    ()

(* codegens a component *)
let codegen_component component =
  match component.node with
  | ComponentDecl
    {
      cname = id;
      definitions = defs;
      _
    } ->
      current_component := id;
      let _ = Ll_table.begin_block !global_env id in
      List.iter (codegen_member_decl id) defs;             (* codegens all members *)
      let _ = Ll_table.end_block !global_env in
      ()

(* codegens a function declaration *)
let declare_function cname fun_def =
  let f_name = 
    (match fun_def.fname with
    | "main" -> "main"
    | _ -> cname ^ "_" ^ fun_def.fname )in
  let r_type = lltype_of_type fun_def.rtype in
  let p_types = List.map lltype_of_param (List.map Utils.type_of_vdecl fun_def.formals) in
  let f_type = L.function_type r_type (Array.of_list p_types) in
  let fun_decl = L.declare_function f_name f_type llvm_mcomp in
  fun_env := Ll_table.add_entry f_name fun_decl false !fun_env;
  ()

(* codegens a function declaration in a member *)
let declare_functions_in_member cname member =
  match member.node with
  | FunDecl d ->
    declare_function cname d
  | VarDecl _ -> ()

(* codegens all function declarations in a component *)
let declare_functions_in_component component =
  match component.node with
  | ComponentDecl
    {
      cname = id;
      definitions = defs;
      _
    } ->
      List.iter (declare_functions_in_member id) defs;
      ()

(* codegens the given ast and returns the llvm module *)
let to_llvm_module ast =
  match ast with
  | CompilationUnit
    {
      components = c;
      _
    } ->
      init;   (* add prelude functions to the env *)
      List.iter declare_functions_in_component c;
      List.iter codegen_component c;
      llvm_mcomp
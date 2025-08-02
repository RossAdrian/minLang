(*
The minLang compiler.
*)

(* ----- The Lexer ----- *)
type token =
  | LET
  | IDENTIFIER of string
  | FN
  | INT
  | CHAR
  | PTR
  | RETURN
  | IF
  | ELSE
  | WHILE
  | LOOP
  | BREAK
  | CONTINUE
  | VOID
  | NUMBER of int
  | STRING of string
  | CHARACTER of char
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LARROW
  | RARROW
  | EQUALS
  | ASSIGN
  | BANG
  | STAR
  | AND
  | MODULO
  | DIVIDE
  | MINUS
  | PLUS
  | LOWEREQUALS
  | GREATEREQUALS

let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let keywords =
  [
    ("let", LET);
    ("fn", FN);
    ("int", INT);
    ("char", CHAR);
    ("ptr", PTR);
    ("return", RETURN);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("loop", LOOP);
    ("break", BREAK);
    ("continue", CONTINUE);
    ("void", VOID);
  ]

let rec lex' cs =
  match cs with
  | [] -> []
  | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: rest -> lex' rest
  (* Identifiers and keywords *)
  | c :: rest when is_letter c -> (
      let rec take_ident acc = function
        | x :: xs when is_letter x || is_digit x -> take_ident (x :: acc) xs
        | xs -> (List.rev acc, xs)
      in
      let ident_chars, rest' = take_ident [ c ] rest in
      let ident = String.of_seq (List.to_seq ident_chars) in
      match List.assoc_opt ident keywords with
      | Some kw -> kw :: lex' rest'
      | None -> IDENTIFIER ident :: lex' rest')
  (* Numbers *)
  | c :: rest when is_digit c ->
      let rec take_num acc = function
        | x :: xs when is_digit x -> take_num (x :: acc) xs
        | xs -> (List.rev acc, xs)
      in
      let num_chars, rest' = take_num [ c ] rest in
      let num = int_of_string (String.of_seq (List.to_seq num_chars)) in
      NUMBER num :: lex' rest'
  (* String literals *)
  | '"' :: rest ->
      let rec take_string acc = function
        | '"' :: xs -> (List.rev acc, xs)
        | '\\' :: '"' :: xs -> take_string ('"' :: acc) xs
        | x :: xs -> take_string (x :: acc) xs
        | [] -> failwith "Unterminated string literal"
      in
      let str_chars, rest' = take_string [] rest in
      STRING (String.of_seq (List.to_seq str_chars)) :: lex' rest'
  (* Character literals *)
  | '\'' :: c :: '\'' :: rest -> CHARACTER c :: lex' rest
  | '\'' :: _ -> failwith "Malformed character literal"
  (* Symbols and operators *)
  | ':' :: rest -> COLON :: lex' rest
  | ',' :: rest -> COMMA :: lex' rest
  | ';' :: rest -> SEMICOLON :: lex' rest
  | '(' :: rest -> LPAREN :: lex' rest
  | ')' :: rest -> RPAREN :: lex' rest
  | '{' :: rest -> LBRACE :: lex' rest
  | '}' :: rest -> RBRACE :: lex' rest
  | '=' :: '=' :: rest -> EQUALS :: lex' rest
  | '=' :: rest -> ASSIGN :: lex' rest
  | '!' :: rest -> BANG :: lex' rest
  | '*' :: rest -> STAR :: lex' rest
  | '&' :: rest -> AND :: lex' rest
  | '%' :: rest -> MODULO :: lex' rest
  | '/' :: rest -> DIVIDE :: lex' rest
  | '<' :: '=' :: rest -> LOWEREQUALS :: lex' rest
  | '>' :: '=' :: rest -> GREATEREQUALS :: lex' rest
  | '-' :: rest -> MINUS :: lex' rest
  | '+' :: rest -> PLUS :: lex' rest
  | '>' :: rest -> RARROW :: lex' rest
  | '<' :: rest -> LARROW :: lex' rest
  | _ -> failwith "Unknown character in input"

let lex (s : string) : token list =
  let cs = List.init (String.length s) (String.get s) in
  lex' cs

(* ----- The AST ----- *)

type ty = FuncTy of ty * ty list | Void | Int | Char | Ptr of ty

type binop =
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Equals
  | NotEquals
  | Lower
  | Greater
  | LowerEquals
  | GreaterEquals
  | Assign

type uop = Not | Indir | Addr

type expr =
  | CInt of int
  | CChar of char
  | CString of string
  | Binary of binop * expr * expr
  | Unary of uop * expr
  | Call of string * expr list
  | Ident of string

type statement =
  | Break
  | Continue
  | Expr of expr
  | Decl of string * ty * expr
  | DeclInfer of string * expr
  | DeclUninit of string * ty
  | Return of expr option
  | Block of statement list
  | While of expr * statement
  | If of expr * statement
  | IfElse of expr * statement * statement
  | Loop of statement

type globalDecl =
  | GlobalDecl of string * ty
  | FuncDef of ty * string * (ty * string) list * statement

(* ----- The Parser ----- *)

(* Type parser *)
let rec parse_ty = function
  | INT :: ts -> (Int, ts)
  | CHAR :: ts -> (Char, ts)
  | PTR :: LARROW :: ts -> (
      let t, ts = parse_incomplete_ty ts in
      match ts with
      | RARROW :: ts -> (Ptr t, ts)
      | _ -> failwith "Missing `>` in pointer type.")
  | _ -> failwith "Not a type."

and parse_incomplete_ty = function
  | VOID :: ts -> (Void, ts)
  | ts -> parse_ty ts

(* ----- Expression Parser ----- *)

let token_to_binop = function
  | PLUS -> Some Plus
  | MINUS -> Some Minus
  | STAR -> Some Mul
  | DIVIDE -> Some Div
  | MODULO -> Some Mod
  | EQUALS -> Some Equals
  | LOWEREQUALS -> Some LowerEquals
  | GREATEREQUALS -> Some GreaterEquals
  | LARROW -> Some Lower
  | RARROW -> Some Greater
  | ASSIGN -> Some Assign
  | _ -> None

let token_to_uop = function
  | BANG -> Some Not
  | STAR -> Some Indir
  | AND -> Some Addr
  | _ -> None

let binop_precedence = function
  | Assign -> 1
  | Equals | NotEquals -> 2
  | Lower | Greater | LowerEquals | GreaterEquals -> 3
  | Plus | Minus -> 4
  | Mul | Div | Mod -> 5

let is_right_assoc = function Assign -> true | _ -> false
let peek = function [] -> None | t :: _ -> Some t

let consume = function
  | [] -> failwith "Unexpected end of input"
  | _ :: ts -> ts

let expect tkn = function
  | t :: ts when t = tkn -> ts
  | _ -> failwith "Unexpected token"

let rec parse_primary = function
  | NUMBER n :: ts -> (CInt n, ts)
  | CHARACTER c :: ts -> (CChar c, ts)
  | STRING s :: ts -> (CString s, ts)
  | IDENTIFIER name :: LPAREN :: ts ->
      let rec parse_args acc ts =
        match ts with
        | RPAREN :: ts' -> (List.rev acc, ts')
        | _ -> (
            let arg, ts' = parse_expr ts in
            match ts' with
            | COMMA :: ts'' -> parse_args (arg :: acc) ts''
            | RPAREN :: ts'' -> (List.rev (arg :: acc), ts'')
            | _ -> failwith "Expected ',' or ')'")
      in
      let args, ts' = parse_args [] ts in
      (Call (name, args), ts')
  | IDENTIFIER name :: ts -> (Ident name, ts)
  | LPAREN :: ts ->
      let e, ts' = parse_expr ts in
      let ts'' = expect RPAREN ts' in
      (e, ts'')
  | _ -> failwith "Expected primary expression"

and parse_unary ts =
  match peek ts with
  | Some tkn when token_to_uop tkn <> None ->
      let uop = Option.get (token_to_uop tkn) in
      let ts' = consume ts in
      let e, ts'' = parse_unary ts' in
      (Unary (uop, e), ts'')
  | _ -> parse_primary ts

and parse_bin_rhs min_prec lhs ts =
  let rec loop lhs ts =
    match peek ts with
    | Some tkn when token_to_binop tkn <> None ->
        let op = Option.get (token_to_binop tkn) in
        let prec = binop_precedence op in
        let right_assoc = is_right_assoc op in
        if prec < min_prec then (lhs, ts)
        else
          let ts' = consume ts in
          let rhs, ts'' = parse_unary ts' in
          let rec_prec = if right_assoc then prec else prec + 1 in
          let rhs', ts''' = parse_bin_rhs rec_prec rhs ts'' in
          loop (Binary (op, lhs, rhs')) ts'''
    | _ -> (lhs, ts)
  in
  loop lhs ts

and parse_expr ts =
  let lhs, ts' = parse_unary ts in
  parse_bin_rhs 1 lhs ts'

(* statement parser *)
let rec parse_stmt = function
  | RETURN :: ts -> (
      match ts with
      | SEMICOLON :: ts -> (Return None, ts)
      | _ -> (
          let e, ts = parse_expr ts in
          match ts with
          | SEMICOLON :: ts -> (Return (Some e), ts)
          | _ -> failwith "Expected `;` after return statement."))
  | LOOP :: ts ->
      let s, ts = parse_stmt ts in
      (Loop s, ts)
  | WHILE :: ts ->
      let e, ts = parse_expr ts in
      let s, ts = parse_stmt ts in
      (While (e, s), ts)
  | IF :: ts -> (
      let e, ts = parse_expr ts in
      let s, ts = parse_stmt ts in
      match ts with
      | ELSE :: ts ->
          let selse, ts = parse_stmt ts in
          (IfElse (e, s, selse), ts)
      | _ -> (If (e, s), ts))
  | LBRACE :: ts -> parse_block [] ts
  | LET :: IDENTIFIER id :: COLON :: ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | SEMICOLON :: ts -> (DeclUninit (id, t), ts)
      | ASSIGN :: ts -> (
          let e, ts = parse_expr ts in
          match ts with
          | SEMICOLON :: ts -> (Decl (id, t, e), ts)
          | _ -> failwith "Expected `;` after declaration.")
      | _ -> failwith "Expected `=` or `;` after declaration.")
  | LET :: IDENTIFIER id :: ASSIGN :: ts -> (
      let e, ts = parse_expr ts in
      match ts with
      | SEMICOLON :: ts -> (DeclInfer (id, e), ts)
      | _ -> failwith "Expected `;` after declaration.")
  | BREAK :: SEMICOLON :: ts -> (Break, ts)
  | CONTINUE :: SEMICOLON :: ts -> (Continue, ts)
  | ts -> (
      let e, ts = parse_expr ts in
      match ts with
      | SEMICOLON :: ts -> (Expr e, ts)
      | _ -> failwith "Expected semicolon after expression statement.")

and parse_block l = function
  | RBRACE :: ts -> (Block l, ts)
  | ts ->
      let s, ts = parse_stmt ts in
      parse_block (l @ [ s ]) ts

(* Global declaration parser *)
let rec parse_global_decl = function
  | FN :: IDENTIFIER id :: LPAREN :: ts -> parse_fn id ts
  | LET :: IDENTIFIER id :: COLON :: ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | SEMICOLON :: ts -> (GlobalDecl (id, t), ts)
      | _ -> failwith "Expected `;` after global variable declaration.")
  | _ -> failwith "Unexpected token in global space."

and parse_fn id ts = parse_fn_list id [] ts

and parse_fn_list id (l : (ty * string) list) = function
  | RPAREN :: SEMICOLON :: ts ->
      (GlobalDecl (id, FuncTy (Void, List.map (fun (t, _) -> t) l)), ts)
  | RPAREN :: COLON :: ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | SEMICOLON :: ts ->
          (GlobalDecl (id, FuncTy (t, List.map (fun (t, _) -> t) l)), ts)
      | LBRACE :: ts ->
          let s, ts = parse_block [] ts in
          (FuncDef (t, id, l, s), ts)
      | _ -> failwith "Expected `{` or `;` after function declaration.")
  | RPAREN :: LBRACE :: ts ->
      let s, ts = parse_block [] ts in
      (FuncDef (Void, id, l, s), ts)
  | IDENTIFIER i :: COLON :: ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | COMMA :: ts -> parse_fn_list id (l @ [ (t, i) ]) ts
      | RPAREN :: _ -> parse_fn_list id (l @ [ (t, i) ]) ts
      | _ -> failwith "Expected `,` or `)`.")
  | ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | COMMA :: ts ->
          parse_fn_decl_list id (List.map (fun (t, _) -> t) l @ [ t ]) ts
      | RPAREN :: _ ->
          parse_fn_decl_list id (List.map (fun (t, _) -> t) l @ [ t ]) ts
      | _ -> failwith "Expected `,` or `)`.")

and parse_fn_decl_list id l = function
  | RPAREN :: SEMICOLON :: ts -> (GlobalDecl (id, FuncTy (Void, l)), ts)
  | RPAREN :: COLON :: ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | SEMICOLON :: ts -> (GlobalDecl (id, FuncTy (t, l)), ts)
      | _ -> failwith "Expected `;` after function declaration.")
  | IDENTIFIER _ :: COLON :: ts -> parse_fn_decl_list id l ts
  | ts -> (
      let t, ts = parse_ty ts in
      match ts with
      | COMMA :: ts -> parse_fn_decl_list id (l @ [ t ]) ts
      | RPAREN :: _ -> parse_fn_decl_list id (l @ [ t ]) ts
      | _ -> failwith "Expected `,` or `)`.")

type program = globalDecl list

let parse (ts : token list) : program =
  let rec parser l = function
    | [] -> l
    | ts ->
        let g, ts = parse_global_decl ts in
        parser (l @ [ g ]) ts
  in
  parser [] ts

(* ---- Semantic analysis ---- *)

(*
Special symbol table entries:

"return" * ty * 0: The return type.
"label" * Void * -2: The break label.
"label" * Void * -3: The continue label.
"offsetnum" * Void * -4: The stack alignment
*)

type symbol = string * ty * int
type symbolTable = symbol list

let rec stackOff (s : symbolTable) : int =
  match s with
  | (_, _, off) :: _ when off >= 0 -> off
  | _ :: s -> stackOff s
  | _ -> 0

let rec lookup id = function
  | [] -> failwith ("Symbol " ^ id ^ " undefined.")
  | (id', t, off) :: _ when id = id' -> (id', t, off)
  | _ :: tb -> lookup id tb

let lookup_ty id tb = match lookup id tb with _, t, _ -> t
let lookup_return_ty tb = lookup_ty "return" tb

let rec lookup_stack off = function
  | [] -> failwith ("No such special entry for " ^ string_of_int off)
  | (id, _, off') :: _ when off = off' -> id
  | _ :: tb -> lookup_stack off tb

let lookup_continue tb = lookup_stack (-3) tb
let lookup_break tb = lookup_stack (-2) tb
let stackAlign (s : symbolTable) : int = lookup_stack (-4) s |> int_of_string

let pushTable isGlobal (s : string) (t : ty) (tb : symbolTable) : symbolTable =
  if isGlobal then (s, t, -1) :: tb
  else (s, t, stackOff tb + stackAlign tb) :: tb

let pushStack = pushTable false

let rec insert_global (st : symbolTable) (id, t) =
  match st with
  | [] -> [ (id, t, -1) ]
  | (id', t', x) :: _ when id = id' ->
      if t = t' then st else failwith "Declaration with other type."
  | s :: st -> s :: insert_global st (id, t)

(* Annotated AST *)

type sexpr =
  | SCInt of int
  | SCChar of char
  | SCString of string
  | SBinary of binop * sexpr * sexpr * ty
  | SUnary of uop * sexpr * ty
  | SCall of string * sexpr list
  | SIdent of symbol

type sstatement =
  | SBreak of string
  | SContinue of string
  | SExpr of sexpr
  | SDecl of string * ty * sexpr option
  | SReturn of sexpr option
  | SBlock of sstatement list * int
  | SWhile of sexpr * sstatement * string * string
  | SIf of sexpr * sstatement
  | SIfElse of sexpr * sstatement * sstatement
  | SLoop of sstatement * string * string

type sglobalDecl =
  | SGlobalDecl of string * ty
  | SFuncDef of ty * string * (ty * string) list * sstatement

(* Analysis *)

let isScalarTy t = match t with Int | Char | Ptr _ -> true | _ -> false

let next_number =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

let next_label () = () |> next_number |> string_of_int |> ( ^ ) ".L"
let resolve_ty ty = match ty with Char -> Int | _ -> ty

let rec check_semantic_expr (st : symbolTable) = function
  | CInt i -> (SCInt i, Int)
  | CChar c -> (SCChar c, Char)
  | CString s -> (SCString s, Ptr Char)
  | Binary (o, lhs, rhs) -> (
      let lhs, lty = check_semantic_expr st lhs in
      let rhs, rty = check_semantic_expr st rhs in
      let lty = resolve_ty lty in
      let rty = resolve_ty rty in
      match (lty, rty, o) with
      | Int, Int, _
      | Ptr _, Int, Plus
      | Ptr _, Int, Minus
      | Ptr _, Ptr _, Equals
      | Ptr _, Ptr _, NotEquals
      | Ptr Void, Ptr _, Assign
      | Ptr _, Ptr Void, Assign ->
          screate_binary o lhs rhs lty
      | x, y, Assign when x = y -> screate_binary o lhs rhs lty
      | _ -> failwith "Operation not defined on this binary expression.")
  | Unary (o, e) -> (
      let e, t = check_semantic_expr st e in
      match (o, t, e) with
      | Not, _, _ -> (SUnary (o, e, t), Int)
      | Addr, _, SIdent _ | Addr, _, SUnary (Indir, _, _) ->
          (SUnary (o, e, t), Ptr t)
      | Indir, Ptr t, _ -> (SUnary (o, e, t), t)
      | _ -> failwith "Illegal unary operation typing.")
  | Call (callee, el) -> (
      let fty = lookup_ty callee st in
      match fty with
      | FuncTy (r, lty) ->
          let sl = check_param_list st (el, lty) in
          (SCall (callee, sl), r)
      | _ -> failwith "Expected function type to call a function.")
  | Ident id -> (
      match lookup id st with id, t, off -> (SIdent (id, t, off), t))

and screate_binary o lhs rhs lty = (SBinary (o, lhs, rhs, lty), lty)

and check_param_list st = function
  | e :: el, t :: tl -> (
      let e, t' = check_semantic_expr st e in
      match (resolve_ty t, resolve_ty t') with
      | x, y when x = y -> e :: check_param_list st (el, tl)
      | Ptr _, Ptr Void -> e :: check_param_list st (el, tl)
      | Ptr Void, Ptr _ -> e :: check_param_list st (el, tl)
      | _ -> failwith "Typing error in function call.")
  | [], [] -> []
  | [], _ -> failwith "Too few arguments in function call."
  | _ -> failwith "Too many arguments in function call."

let rec check_semantic_stmt (st : symbolTable) = function
  | [] -> []
  | Break :: sl -> SBreak (lookup_break st) :: check_semantic_stmt st sl
  | Continue :: sl ->
      SContinue (lookup_continue st) :: check_semantic_stmt st sl
  | Expr e :: sl ->
      SExpr (e |> check_semantic_expr st |> fst) :: check_semantic_stmt st sl
  | Decl (id, t, e) :: sl -> (
      let se, t' = check_semantic_expr st e in
      match (resolve_ty t, resolve_ty t') with
      | x, y when x = y -> check_semantic_decl st (Some se) t id sl
      | Ptr _, Ptr Void -> check_semantic_decl st (Some se) t id sl
      | Ptr Void, Ptr _ -> check_semantic_decl st (Some se) t id sl
      | _ -> failwith "Typing error in declaration.")
  | DeclInfer (id, e) :: sl ->
      let se, t' = check_semantic_expr st e in
      check_semantic_decl st (Some se) t' id sl
  | DeclUninit (id, t) :: sl -> check_semantic_decl st None t id sl
  | Return (Some e) :: sl -> (
      let se, t' = check_semantic_expr st e in
      match (resolve_ty t', st |> lookup_return_ty |> resolve_ty) with
      | x, y when x = y -> SReturn (Some se) :: check_semantic_stmt st sl
      | Ptr _, Ptr Void -> SReturn (Some se) :: check_semantic_stmt st sl
      | Ptr Void, Ptr _ -> SReturn (Some se) :: check_semantic_stmt st sl
      | _ -> failwith "Typing error in return statement.")
  | Return None :: sl ->
      if st |> lookup_return_ty |> ( = ) Void then
        SReturn None :: check_semantic_stmt st sl
      else
        failwith
          "Expected expression for return statement. Return type is not Void."
  | Block sl' :: sl ->
      let off = stackOff st in
      SBlock (sl' |> check_semantic_stmt st, off) :: check_semantic_stmt st sl
  | While (e, s) :: sl ->
      let blabel = next_label () in
      let elabel = next_label () in
      let st' = (blabel, Void, -3) :: (elabel, Void, -2) :: st in
      let se, t = check_semantic_expr st e in
      if not (isScalarTy t) then
        failwith "Expected pointer or int in while condition"
      else
        let s = [ s ] |> check_semantic_stmt st' |> List.hd in
        SWhile (se, s, blabel, elabel) :: check_semantic_stmt st sl
  | If (e, s) :: sl ->
      let se, t = check_semantic_expr st e in
      if not (isScalarTy t) then
        failwith "Expected pointer or int in if condition"
      else
        SIf (se, [ s ] |> check_semantic_stmt st |> List.hd)
        :: check_semantic_stmt st sl
  | IfElse (e, s, selse) :: sl ->
      let se, t = check_semantic_expr st e in
      if not (isScalarTy t) then
        failwith "Expected pointer or int in if condition"
      else
        SIfElse
          ( se,
            [ s ] |> check_semantic_stmt st |> List.hd,
            [ selse ] |> check_semantic_stmt st |> List.hd )
        :: check_semantic_stmt st sl
  | Loop s :: sl ->
      let blabel = next_label () in
      let elabel = next_label () in
      let st' = (blabel, Void, -3) :: (elabel, Void, -2) :: st in
      let s = [ s ] |> check_semantic_stmt st' |> List.hd in
      SLoop (s, blabel, elabel) :: check_semantic_stmt st sl

and check_semantic_decl st se t id sl =
  SDecl (id, t, se) :: check_semantic_stmt (st |> pushStack id t) sl

let rec check_semantic_gdecl (st : symbolTable) = function
  | [] -> []
  | GlobalDecl (id, t) :: sl ->
      SGlobalDecl (id, t) :: check_semantic_gdecl (insert_global st (id, t)) sl
  | FuncDef (rty, id, args, s) :: sl ->
      let t = FuncTy (rty, List.map (fun x -> fst x) args) in
      let st = insert_global st (id, t) in
      let st' = ("return", rty, 0) :: st in
      let st' = pushArgs st' args in
      let s = [ s ] |> check_semantic_stmt st' |> List.hd in
      SFuncDef (rty, id, args, s) :: check_semantic_gdecl st sl

and pushArgs st = function
  | [] -> st
  | (t, id) :: args -> pushArgs (pushStack id t st) args

type sprogram = sglobalDecl list

let semantic_check (off : int) : program -> sprogram =
  let st = [ (string_of_int off, Void, -4) ] in
  check_semantic_gdecl st

(* ---- Code Generation ---- *)

type reg =
  | SP
  | FP
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6

let string_of_reg = function
  | SP -> "sp"
  | FP -> "fp"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | T3 -> "t3"
  | T4 -> "t4"
  | T5 -> "t5"
  | T6 -> "t6"
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"

type reg_alloc = reg list

let rec reg_is_free r = function
  | [] -> false
  | r' :: _ when r' = r -> true
  | _ :: rl -> reg_is_free r rl

type ir =
  | Label of string * bool
  | Asciiz of string
  | Word of int
  | Section of string
  | Extern of string
  | IJmp of string
  | IJmpIf of reg * string
  | IAdd of reg * reg * reg * reg_alloc
  | ILi of reg * int * reg_alloc
  | ILa of reg * string * reg_alloc
  | IMul of reg * reg * reg * reg_alloc
  | IDiv of reg * reg * reg * reg_alloc
  | ISub of reg * reg * reg * reg_alloc
  | IMod of reg * reg * reg * reg_alloc
  | IEquals of reg * reg * reg * reg_alloc
  | INotEquals of reg * reg * reg * reg_alloc
  | ILower of reg * reg * reg * reg_alloc
  | IGreater of reg * reg * reg * reg_alloc
  | ILowerEquals of reg * reg * reg * reg_alloc
  | IGreaterEquals of reg * reg * reg * reg_alloc
  | IMov of reg * reg * reg_alloc
  | ICall of string * int * reg_alloc
  | IReturn of reg option
  | IEnter
  | INot of reg * reg
  | ILoadInt of reg * int * reg
  | ILoadByte of reg * int * reg
  | ILoadPtr of reg * int * reg
  | IStoreInt of reg * int * reg
  | IStoreChar of reg * int * reg
  | IStorePtr of reg * int * reg

let string_of_ir = function
  | Section s -> ".section " ^ s
  | Label (s, glob) -> (if glob then ".global " ^ s ^ "\n" else "\n") ^ s ^ ":"
  | Asciiz s -> ".asciiz " ^ s
  | Word i -> ".word " ^ string_of_int i
  | IJmp s -> "    jmp           " ^ s
  | IJmpIf (r, s) -> "    jmpif         " ^ string_of_reg r ^ " " ^ s
  | IAdd (out, lhs, rhs, _) ->
      "    add           " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | ILi (r, i, _) ->
      "    li            " ^ string_of_reg r ^ " " ^ string_of_int i
  | ILa (r, s, _) -> "    la            " ^ string_of_reg r ^ " " ^ s
  | IMul (out, lhs, rhs, _) ->
      "    mul           " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | IDiv (out, lhs, rhs, _) ->
      "    div           " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | ISub (out, lhs, rhs, _) ->
      "    sub           " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | IMod (out, lhs, rhs, _) ->
      "    mod           " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | IEquals (out, lhs, rhs, _) ->
      "    equals        " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | INotEquals (out, lhs, rhs, _) ->
      "    notequals     " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | ILower (out, lhs, rhs, _) ->
      "    lower         " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | IGreater (out, lhs, rhs, _) ->
      "    greater       " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | ILowerEquals (out, lhs, rhs, _) ->
      "    lowerequals   " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | IGreaterEquals (out, lhs, rhs, _) ->
      "    greaterequals " ^ string_of_reg out ^ " " ^ string_of_reg lhs ^ " "
      ^ string_of_reg rhs
  | IMov (out, src, _) ->
      "    mov           " ^ string_of_reg out ^ " " ^ string_of_reg src
  | ICall (id, _, _) -> "    call          " ^ id
  | IReturn (Some r) -> "    ret           " ^ string_of_reg r
  | IReturn None -> "    ret"
  | IEnter -> "    enter"
  | ILoadInt (r, i, r2) ->
      "    lw            " ^ string_of_reg r ^ " " ^ string_of_int i ^ "("
      ^ string_of_reg r2 ^ ")"
  | ILoadByte (r, i, r2) ->
      "    lb            " ^ string_of_reg r ^ " " ^ string_of_int i ^ "("
      ^ string_of_reg r2 ^ ")"
  | ILoadPtr (r, i, r2) ->
      "    lptr           " ^ string_of_reg r ^ " " ^ string_of_int i ^ "("
      ^ string_of_reg r2 ^ ")"
  | IStoreInt (r, i, r2) ->
      "    sw            " ^ string_of_reg r ^ " " ^ string_of_int i ^ "("
      ^ string_of_reg r2 ^ ")"
  | IStoreChar (r, i, r2) ->
      "    sb            " ^ string_of_reg r ^ " " ^ string_of_int i ^ "("
      ^ string_of_reg r2 ^ ")"
  | IStorePtr (r, i, r2) ->
      "    sptr          " ^ string_of_reg r ^ " " ^ string_of_int i ^ "("
      ^ string_of_reg r2 ^ ")"
  | INot (r, r2) ->
      "    not           " ^ string_of_reg r ^ " " ^ string_of_reg r2
  | Extern s -> ""

let translate_ir acc i =
  "\n" ^ List.fold_right (fun a b -> string_of_ir a ^ "\n" ^ b) i acc

let string_of_ir_list i = translate_ir "" i

type codegenCtx = reg_alloc * int

let data_section : ir list ref = ref []

let alloc_reg : reg_alloc -> reg * reg_alloc = function
  | [] -> failwith "Codegen out of registers."
  | r :: all -> (r, all)

let rec codegen_expr ((alloc, align) : codegenCtx) (se : sexpr) :
    ir list * codegenCtx * reg =
  match se with
  | SCInt i ->
      let r, ctxx = alloc_reg alloc in
      ([ ILi (r, i, ctxx) ], (ctxx, align), r)
  | SCChar i ->
      let r, ctxx = alloc_reg alloc in
      ([ ILi (r, int_of_char i, ctxx) ], (ctxx, align), r)
  | SCString s ->
      let label = next_label () in
      let _ =
        let data = !data_section in
        let data = Label (label, false) :: Asciiz s :: data in
        data_section := data
      in
      let r, ctxx = alloc_reg alloc in
      ([ ILa (r, label, ctxx) ], (ctxx, align), r)
  | SIdent (id, Char, _) ->
      let i, ctxx, r = codegen_expr_leval (alloc, align) se in
      (i @ [ ILoadByte (r, 0, r) ], ctxx, r)
  | SIdent (id, Int, _) ->
      let i, ctxx, r = codegen_expr_leval (alloc, align) se in
      (i @ [ ILoadInt (r, 0, r) ], ctxx, r)
  | SIdent (id, _, _) ->
      let i, ctxx, r = codegen_expr_leval (alloc, align) se in
      (i @ [ ILoadPtr (r, 0, r) ], ctxx, r)
  | SUnary (Addr, e, _) -> codegen_expr_leval (alloc, align) e
  | SUnary (Not, e, _) ->
      let i, ctxx, r = e |> codegen_expr (alloc, align) in
      (i @ [ INot (r, r) ], ctxx, r)
  | SUnary (Indir, e, Char) ->
      let i, ctxx, r = e |> codegen_expr (alloc, align) in
      (i @ [ ILoadByte (r, 0, r) ], ctxx, r)
  | SUnary (Indir, e, Int) ->
      let i, ctxx, r = e |> codegen_expr (alloc, align) in
      (i @ [ ILoadInt (r, 0, r) ], ctxx, r)
  | SUnary (Indir, e, _) ->
      let i, ctxx, r = e |> codegen_expr (alloc, align) in
      (i @ [ ILoadPtr (r, 0, r) ], ctxx, r)
  | SBinary (Assign, lhs, rhs, t) ->
      let ils, ctxx, rl = codegen_expr_leval (alloc, align) lhs in
      let irs, ctxx, rr = codegen_expr ctxx rhs in
      let outp, ctxx1 = alloc_reg alloc in
      let i =
        ils @ irs @ codegen_create_binop outp rl rr (fst ctxx) t align Assign
      in
      (i, (ctxx1, align), outp)
  | SBinary (op, lhs, rhs, t) ->
      let ils, ctxx, rl = codegen_expr (alloc, align) lhs in
      let irs, ctxx, rr = codegen_expr ctxx rhs in
      let outp, ctxx1 = alloc_reg alloc in
      let i =
        ils @ irs @ codegen_create_binop outp rl rr (fst ctxx) t align op
      in
      (i, (ctxx1, align), outp)
  | SCall (f, el) ->
      let all_regs = fill_regs alloc in
      let rec codegen_binary = function
        | _, [] -> []
        | rx :: rl, e :: el ->
            let i, _, r = codegen_expr (all_regs, align) e in
            i @ [ IMov (rx, r, all_regs) ] @ codegen_binary (rl, el)
        | _ -> failwith "Out of parameters."
      in
      let i = codegen_binary ([ A0; A1; A2; A3; A4; A5; A6 ], el) in
      let rout, alloc = alloc_reg alloc in
      ( push_regs alloc align @ i
        @ [ ICall (f, List.length el, alloc) ]
        @ pop_regs alloc align
        @ [ IMov (rout, A0, alloc) ],
        (alloc, align),
        rout )

and codegen_expr_leval ((alloc, align) : codegenCtx) :
    sexpr -> ir list * codegenCtx * reg = function
  | SUnary (Indir, e, _) -> codegen_expr (alloc, align) e
  | SIdent (id, t, -1) ->
      let r, ctxx = alloc_reg alloc in
      ([ ILa (r, id, ctxx) ], (ctxx, align), r)
  | SIdent (id, t, off) ->
      let r, ctxx = alloc_reg alloc in
      ([ ILi (r, off, ctxx); ISub (r, FP, r, ctxx) ], (ctxx, align), r)
  | _ -> failwith "Not supported LEval!"

and codegen_create_binop outp lhs rhs ctx t align = function
  | Plus -> (
      let ireg, alloc = alloc_reg ctx in
      match t with
      | Ptr Int ->
          [
            ILi (ireg, 4, alloc);
            IMul (ireg, ireg, rhs, alloc);
            IAdd (outp, lhs, ireg, ctx);
          ]
      | Ptr (Ptr _) ->
          [
            ILi (ireg, align, alloc);
            IMul (ireg, ireg, rhs, alloc);
            IAdd (outp, lhs, ireg, ctx);
          ]
      | _ -> [ IAdd (outp, lhs, rhs, ctx) ])
  | Minus -> (
      let ireg, alloc = alloc_reg ctx in
      match t with
      | Ptr Int ->
          [
            ILi (ireg, 4, alloc);
            IMul (ireg, ireg, rhs, alloc);
            ISub (outp, lhs, ireg, ctx);
          ]
      | Ptr (Ptr _) ->
          [
            ILi (ireg, align, alloc);
            IMul (ireg, ireg, rhs, alloc);
            ISub (outp, lhs, ireg, ctx);
          ]
      | _ -> [ ISub (outp, lhs, rhs, ctx) ])
  | Mul -> [ IMul (outp, lhs, rhs, ctx) ]
  | Div -> [ IDiv (outp, lhs, rhs, ctx) ]
  | Mod -> [ IMod (outp, lhs, rhs, ctx) ]
  | Equals -> [ IEquals (outp, lhs, rhs, ctx) ]
  | NotEquals -> [ INotEquals (outp, lhs, rhs, ctx) ]
  | Lower -> [ ILower (outp, lhs, rhs, ctx) ]
  | LowerEquals -> [ ILowerEquals (outp, lhs, rhs, ctx) ]
  | Greater -> [ IGreater (outp, lhs, rhs, ctx) ]
  | GreaterEquals -> [ IGreaterEquals (outp, lhs, rhs, ctx) ]
  | Assign -> (
      match t with
      | Char -> [ IStoreChar (rhs, 0, lhs); IMov (outp, rhs, ctx) ]
      | Int -> [ IStoreInt (rhs, 0, lhs); IMov (outp, rhs, ctx) ]
      | _ -> [ IStorePtr (rhs, 0, lhs); IMov (outp, rhs, ctx) ])

and int_of_reg = function
  | T0 :: _ -> 0
  | T1 :: _ -> 1
  | T2 :: _ -> 2
  | T3 :: _ -> 3
  | T4 :: _ -> 4
  | T5 :: _ -> 5
  | T6 :: _ -> 6
  | _ -> 7

and reg_of_int = function
  | 0 -> T0
  | 1 -> T1
  | 2 -> T2
  | 3 -> T3
  | 4 -> T4
  | 5 -> T5
  | _ -> T6

and push_regs alloc align =
  let rxx = List.hd alloc in
  let i = int_of_reg alloc in
  let rec create_push i =
    if i <= 0 then []
    else
      [
        ILi (rxx, align, alloc);
        ISub (SP, SP, rxx, List.tl alloc);
        IStorePtr (reg_of_int (i - 1), 0, SP);
      ]
      @ create_push (i - 1)
  in
  create_push i

and pop_regs alloc align =
  let rxx = List.hd alloc in
  let i = int_of_reg alloc in
  let rec create_pop i =
    if i <= 0 then []
    else
      create_pop (i - 1)
      @ [
          ILoadPtr (reg_of_int (i - 1), 0, SP);
          ILi (rxx, align, alloc);
          IAdd (SP, SP, rxx, List.tl alloc);
        ]
  in
  create_pop i

and fill_regs alloc =
  let i = int_of_reg alloc in
  if i <= 0 then alloc else fill_regs (reg_of_int (i - 1) :: alloc)

let rec codegen_stmt ((alloc, align) : codegenCtx) = function
  | [] -> []
  | SBreak l :: sl -> IJmp l :: codegen_stmt (alloc, align) sl
  | SContinue l :: sl -> IJmp l :: codegen_stmt (alloc, align) sl
  | SExpr e :: sl ->
      (match codegen_expr (alloc, align) e with i, _, _ -> i)
      @ codegen_stmt (alloc, align) sl
  | SDecl (id, t, None) :: sl ->
      let r, ctx = alloc_reg alloc in
      ILi (r, align, ctx)
      :: ISub (SP, SP, r, ctx)
      :: codegen_stmt (alloc, align) sl
  | SDecl (id, t, Some e) :: sl ->
      let r, ctx = alloc_reg alloc in
      ILi (r, align, ctx)
      :: ISub (SP, SP, r, ctx)
      ::
      (match codegen_expr (alloc, align) e with
      | i, _, r -> (
          i
          @
          match t with
          | Char -> [ IStoreChar (r, 0, SP) ]
          | Int -> [ IStoreInt (r, 0, SP) ]
          | _ -> [ IStorePtr (r, 0, SP) ]))
      @ codegen_stmt (alloc, align) sl
  | SReturn None :: sl -> IReturn None :: codegen_stmt (alloc, align) sl
  | SReturn (Some e) :: sl ->
      (match codegen_expr (alloc, align) e with
      | i, _, rx -> i @ [ IReturn (Some rx) ])
      @ codegen_stmt (alloc, align) sl
  | SBlock (sl', i) :: sl ->
      (codegen_stmt (alloc, align) sl'
      @
      let r, ctx = alloc_reg alloc in
      [ ILi (r, i, ctx); ISub (SP, FP, r, ctx) ])
      @ codegen_stmt (alloc, align) sl
  | SWhile (e, sl', bl, el) :: sl ->
      (Label (bl, false)
       ::
       (let i, _, r = codegen_expr (alloc, align) e in
        i @ [ INot (r, r); IJmpIf (r, el) ])
      @ codegen_stmt (alloc, align) [ sl' ]
      @ [ IJmp bl; Label (el, false) ])
      @ codegen_stmt (alloc, align) sl
  | SIf (e, sl') :: sl ->
      (let i, _, r = codegen_expr (alloc, align) e in
       let l = next_label () in
       i
       @ [ INot (r, r); IJmpIf (r, l) ]
       @ codegen_stmt (alloc, align) [ sl' ]
       @ [ Label (l, false) ])
      @ codegen_stmt (alloc, align) sl
  | SIfElse (e, sl', sl'') :: sl ->
      (let i, _, r = codegen_expr (alloc, align) e in
       let lelse = next_label () in
       let lend = next_label () in
       i
       @ [ INot (r, r); IJmpIf (r, lelse) ]
       @ codegen_stmt (alloc, align) [ sl' ]
       @ [ IJmp lend; Label (lelse, false) ]
       @ codegen_stmt (alloc, align) [ sl'' ]
       @ [ Label (lend, false) ])
      @ codegen_stmt (alloc, align) sl
  | SLoop (sl', lbegin, lend) :: sl ->
      ((Label (lbegin, false) :: codegen_stmt (alloc, align) [ sl' ])
      @ [ IJmp lbegin; Label (lend, false) ])
      @ codegen_stmt (alloc, align) sl

let rec codegen_gdecl ((alloc, align) : codegenCtx) = function
  | [] -> []
  | SGlobalDecl (id, FuncTy (_, _)) :: sl ->
      Extern id :: codegen_gdecl (alloc, align) sl
  | SGlobalDecl (id, _) :: sl ->
      data_section := !data_section @ [ Label (id, true); Word 0 ];
      codegen_gdecl (alloc, align) sl
  | SFuncDef (_, id, args, sl') :: sl ->
      Label (id, true)
      :: IEnter
      ::
      (let r, ctx = alloc_reg alloc in
       let rec push_args n l =
         if n = l then []
         else
           [
             ILi (r, align, ctx);
             ISub (SP, SP, r, ctx);
             IStorePtr (reg_arg n, 0, SP);
           ]
           @ push_args (n + 1) l
       in
       push_args 0 (List.length args))
      @ codegen_stmt (alloc, align) [ sl' ]
      @ [ IReturn None ]
      @ codegen_gdecl (alloc, align) sl

and reg_arg = function
  | 0 -> A0
  | 1 -> A1
  | 2 -> A2
  | 3 -> A3
  | 4 -> A4
  | 5 -> A5
  | 6 -> A6
  | _ -> A0

let codegen (ctx : codegenCtx) (p : sprogram) : ir list =
  Section "data"
  ::
  (let data = !data_section in
   let _ = data_section := [] in
   data)
  @ [ Section "text" ] @ codegen_gdecl ctx p

(* ---- RISC-V (32bit) code generation ---- *)

let translate_riscv_single : ir -> string = function
  | Extern id -> ""
  | Label (s, true) -> "        .globl " ^ s ^ "\n" ^ s ^ ":"
  | Label (s, false) -> s ^ ":"
  | Section s -> "        ." ^ s
  | Word i -> "        .word " ^ string_of_int i
  | Asciiz s -> "        .asciiz \"" ^ s ^ "\""
  | IEnter ->
      "        addi    sp sp -8\n" ^ "        sw      ra 4(sp)\n"
      ^ "        sw      s0 0(sp)\n" ^ "        addi    s0 sp 0"
  | IJmp s -> "        j       " ^ s
  | IJmpIf (r, s) -> "        bnz     " ^ string_of_reg r ^ " " ^ s
  | ILower (out, r1, r2, _) ->
      "        slt     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IGreater (out, r1, r2, _) ->
      "        sgt     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | ILowerEquals (out, r2, r1, _) ->
      "        sgt     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IGreaterEquals (out, r2, r1, _) ->
      "        slt     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IEquals (out, r1, r2, _) ->
      "        xor     " ^ string_of_reg out ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2 ^ "\n" ^ "        seqz    " ^ string_of_reg out ^ " "
      ^ string_of_reg out
  | INotEquals (out, r1, r2, _) ->
      "        xor     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r1 ^ "\n" ^ "        snez    " ^ string_of_reg out ^ " "
      ^ string_of_reg out
  | IAdd (out, r1, r2, _) ->
      "        add     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | ISub (out, r1, r2, _) ->
      "        sub     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IMul (out, r1, r2, _) ->
      "        mul     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IDiv (out, r1, r2, _) ->
      "        div     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IMod (out, r1, r2, _) ->
      "        mod     " ^ string_of_reg out ^ " " ^ string_of_reg r1 ^ " "
      ^ string_of_reg r2
  | IReturn s ->
      (match s with
      | None -> ""
      | Some r -> "        mv      a0 " ^ string_of_reg r ^ "\n")
      ^ "        addi    sp s0 0\n" ^ "        lw      s0 0(sp)\n"
      ^ "        lw      ra 4(sp)\n" ^ "        addi    sp sp 8\n"
      ^ "        jr      ra"
  | ILi (r, i, _) ->
      "        li      " ^ string_of_reg r ^ " " ^ string_of_int i
  | ILa (r, id, _) -> "        la      " ^ string_of_reg r ^ " " ^ id
  | IMov (out, r1, _) ->
      "        mv      " ^ string_of_reg out ^ " " ^ string_of_reg r1
  | ICall (f, _, _) -> "        jal     " ^ f
  | INot (out, r1) ->
      "        not     " ^ string_of_reg out ^ " " ^ string_of_reg r1
  | ILoadByte (r, off, d) ->
      "        lb      " ^ string_of_reg r ^ " " ^ string_of_int off ^ "("
      ^ string_of_reg d ^ ")"
  | ILoadInt (r, off, d) ->
      "        lw      " ^ string_of_reg r ^ " " ^ string_of_int off ^ "("
      ^ string_of_reg d ^ ")"
  | ILoadPtr (r, off, d) ->
      "        lw      " ^ string_of_reg r ^ " " ^ string_of_int off ^ "("
      ^ string_of_reg d ^ ")"
  | IStoreChar (r, off, d) ->
      "        sb      " ^ string_of_reg r ^ " " ^ string_of_int off ^ "("
      ^ string_of_reg d ^ ")"
  | IStoreInt (r, off, d) ->
      "        sw      " ^ string_of_reg r ^ " " ^ string_of_int off ^ "("
      ^ string_of_reg d ^ ")"
  | IStorePtr (r, off, d) ->
      "        sw      " ^ string_of_reg r ^ " " ^ string_of_int off ^ "("
      ^ string_of_reg d ^ ")"

let rec translate_riscv (acc : string) : ir list -> string = function
  | [] -> acc
  | ILi (r, x, _) :: IMov (rx, r', _) :: sl when r = r' ->
      translate_riscv
        (acc ^ "        addi    " ^ string_of_reg rx ^ " x0 " ^ string_of_int x
       ^ "\n")
        sl
  | ILi (r, x, _) :: IReturn (Some r') :: sl when r = r' ->
      translate_riscv
        (acc ^ "        addi    a0 x0 " ^ string_of_int x ^ "\n")
        (IReturn None :: sl)
  | x :: sl -> translate_riscv (acc ^ translate_riscv_single x ^ "\n") sl

(* ---- x64 NASM code generation ---- *)

let rec register_from_reg s r : string =
  let aux = function
    | 1, T0 -> "al"
    | 4, T0 -> "eax"
    | 8, T0 -> "rax"
    | 1, T1 -> "bl"
    | 4, T1 -> "ebx"
    | 8, T1 -> "rbx"
    | 1, T2 -> "cl"
    | 4, T2 -> "ecx"
    | 8, T2 -> "rcx"
    | 1, T3 -> "dl"
    | 4, T3 -> "edx"
    | 8, T3 -> "rdx"
    | 1, T4 -> "dil"
    | 4, T4 -> "edi"
    | 8, T4 -> "rdi"
    | 1, T5 -> "sil"
    | 4, T5 -> "esi"
    | 8, T5 -> "rsi"
    | 4, SP -> "esp"
    | 8, SP -> "rsp"
    | 4, FP -> "ebp"
    | 8, FP -> "rbp"
    (* Argument registers *)
    | 8, A0 -> "rdi"
    | 8, A1 -> "rsi"
    | 8, A2 -> "rdx"
    | 8, A3 -> "rcx"
    | 8, A4 -> "r8"
    | 8, A5 -> "r9"
    | _ -> "; Not supported register"
  in
  aux (s, r)

let nasm_string_chunks (s : string) : string =
  let len = String.length s in
  let buffer = Buffer.create len in
  let flush_literal lit =
    if lit <> "" then Buffer.add_string buffer ("\"" ^ lit ^ "\", ")
  in
  let rec loop i literal =
    if i >= len then (
      flush_literal literal;
      Buffer.add_string buffer "0";
      Buffer.contents buffer)
    else
      match s.[i] with
      | '\\' when i + 1 < len -> (
          let next = s.[i + 1] in
          let expanded =
            match next with
            | 'n' -> Some 10
            | 't' -> Some 9
            | 'r' -> Some 13
            | '\\' -> Some (Char.code '\\')
            | '"' -> Some (Char.code '"')
            | '\'' -> Some (Char.code '\'')
            | '0' -> Some 0
            | _ -> None
          in
          match expanded with
          | Some code ->
              flush_literal literal;
              Buffer.add_string buffer (string_of_int code ^ ", ");
              loop (i + 2) ""
          | None -> loop (i + 1) (literal ^ String.make 1 s.[i]))
      | c -> loop (i + 1) (literal ^ String.make 1 c)
  in
  loop 0 ""

let rec translate_nasm_x64_single : ir -> string = function
  | Label (s, true) -> "        global " ^ s ^ "\n" ^ s ^ ":"
  | Label (s, false) -> s ^ ":"
  | Section s -> "section ." ^ s
  | Extern s -> "extern " ^ s
  | Word r -> "        dq " ^ string_of_int r
  | Asciiz s -> "        db " ^ nasm_string_chunks s
  | IEnter -> "        push    rbp\n" ^ "        mov     rbp, rsp"
  | IReturn None ->
      "        mov     rsp, rbp\n" ^ "        pop     rbp\n" ^ "        ret"
  | IReturn (Some r) ->
      if r = T0 then translate_nasm_x64_single (IReturn None)
      else
        "    mov     rax, " ^ register_from_reg 8 r ^ "\n"
        ^ translate_nasm_x64_single (IReturn None)
  | IJmp s -> "        jmp     " ^ s
  | IJmpIf (r, s) ->
      "        cmp     " ^ register_from_reg 8 r ^ ", 0\n" ^ "        jne     "
      ^ s
  | IAdd (out, r1, r2, _) when r1 = out ->
      "        add     " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2
  | IAdd (out, r1, r2, _) ->
      "        add     " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2 ^ "\n" ^ "        mov     "
      ^ register_from_reg 8 out ^ ", " ^ register_from_reg 8 r1
  | ISub (out, r1, r2, _) when r1 = out ->
      "        sub     " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2
  | ISub (out, r1, r2, _) ->
      "        sub     " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2 ^ "\n" ^ "        mov     "
      ^ register_from_reg 8 out ^ ", " ^ register_from_reg 8 r1
  | IMul (out, r1, r2, _) when r1 = out ->
      "        imul    " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2
  | IMul (out, r1, r2, _) ->
      "        imul    " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2 ^ "\n" ^ "        mov     "
      ^ register_from_reg 8 out ^ ", " ^ register_from_reg 8 r1
  | ILa (r, s, _) -> "        mov     " ^ register_from_reg 8 r ^ ", " ^ s
  | ILi (r, i, _) ->
      "        mov     " ^ register_from_reg 8 r ^ ", " ^ string_of_int i
  | IMov (r, A0, _) -> "        mov     " ^ register_from_reg 8 r ^ ", r9"
  | IMov (a, r, _) when is_arg a <> -1 ->
      "        push    " ^ register_from_reg 8 r
  | IMov (r1, r2, _) when register_from_reg 8 r1 = register_from_reg 8 r2 -> ""
  | IMov (r1, r2, _) ->
      "        mov     " ^ register_from_reg 8 r1 ^ ", "
      ^ register_from_reg 8 r2
  | ILoadByte (r1, 0, r2) ->
      "        mov     " ^ register_from_reg 1 r1 ^ ", ["
      ^ register_from_reg 8 r2 ^ "]\n" ^ "        movzx   "
      ^ register_from_reg 8 r1 ^ ", " ^ register_from_reg 1 r1
  | ILoadInt (r1, 0, r2) ->
      "        mov     " ^ register_from_reg 4 r1 ^ ", ["
      ^ register_from_reg 8 r2 ^ "]"
  | ILoadPtr (r1, 0, r2) ->
      "        mov     " ^ register_from_reg 8 r1 ^ ", ["
      ^ register_from_reg 8 r2 ^ "]"
  | IStoreChar (r1, 0, r2) ->
      "        mov     [" ^ register_from_reg 8 r2 ^ "], "
      ^ register_from_reg 1 r1
  | IStoreInt (r1, 0, r2) ->
      "        mov     [" ^ register_from_reg 8 r2 ^ "], "
      ^ register_from_reg 4 r1
  | IStorePtr (r1, 0, r2) ->
      "        mov     [" ^ register_from_reg 8 r2 ^ "], "
      ^ register_from_reg 8 r1
  | INot (out, r) ->
      "        cmp     " ^ register_from_reg 8 r ^ ", 0\n" ^ "        sete    "
      ^ register_from_reg 1 out ^ "\n" ^ "        movzx   "
      ^ register_from_reg 8 out ^ ", " ^ register_from_reg 1 out
  | ICall (f, n, _) ->
      let rec pop_args n =
        if n = 0 then ""
        else
          "        pop     "
          ^ register_from_reg 8 (arg_of_int (n - 1))
          ^ "\n"
          ^ pop_args (n - 1)
      in
      pop_args n ^ "        call    " ^ f ^ "\n" ^ "        mov     r9, rax"
  | IDiv (T0, T0, r2, free_regs) when reg_is_free T3 free_regs ->
      "        cqo\n" ^ "        idiv    " ^ register_from_reg 8 r2
  | IDiv (T0, T0, r2, free_regs) ->
      let sr = free_regs |> alloc_reg |> fst in
      "        mov     " ^ register_from_reg 8 sr ^ ", rdx\n" ^ "        cqo\n"
      ^ "        idiv    " ^ register_from_reg 8 r2 ^ "\n"
      ^ "        mov     rdx, " ^ register_from_reg 8 sr
  | IDiv (out, r1, T3, r2 :: free_regs) ->
      let raxx, _ = alloc_reg free_regs in
      "        mov     " ^ register_from_reg 8 raxx ^ ", rax\n"
      ^ "        mov     " ^ register_from_reg 8 r2 ^ ", rdx\n"
      ^ "        mov     rax, " ^ register_from_reg 8 r1 ^ "\n"
      ^ "        cqo\n" ^ "        idiv    " ^ register_from_reg 8 r2 ^ "\n"
      ^ "        mov     " ^ register_from_reg 8 out ^ ", rax\n"
      ^ "        mov     rax, " ^ register_from_reg 8 raxx
  | IDiv (out, r1, r2, free_regs) ->
      let raxx, free_regs = alloc_reg free_regs in
      let rdxx, _ = alloc_reg free_regs in
      "        mov     " ^ register_from_reg 8 raxx ^ ", rax\n"
      ^ "        mov     " ^ register_from_reg 8 rdxx ^ ", rdx\n"
      ^ "        mov     rax, " ^ register_from_reg 8 r1 ^ "\n"
      ^ "        cqo\n" ^ "        idiv    " ^ register_from_reg 8 r2 ^ "\n"
      ^ "        mov     " ^ register_from_reg 8 out ^ ", rax\n"
      ^ "        mov     rdx, " ^ register_from_reg 8 rdxx ^ "\n"
      ^ "        mov     rax, " ^ register_from_reg 8 raxx
  | ILowerEquals(out, r1, r2, _) -> (
    "        cmp     " ^ register_from_reg 8 r1 ^ ", " ^ register_from_reg 8 r2 ^ "\n" ^
    "        setle   " ^ register_from_reg 1 out ^ "\n" ^
    "        movzx   " ^ register_from_reg 8 out ^ ", " ^ register_from_reg 1 out
  )
  | _ -> "; Not implemented!"

and is_arg = function
  | A0 -> 0
  | A1 -> 1
  | A2 -> 2
  | A3 -> 3
  | A4 -> 4
  | A5 -> 5
  | A6 -> 6
  | _ -> -1

and arg_of_int = function
  | 0 -> A0
  | 1 -> A1
  | 2 -> A2
  | 3 -> A3
  | 4 -> A4
  | 5 -> A5
  | 6 -> A6
  | _ -> A0

let rec translate_x64_nasm (acc : string) : ir list -> string = function
  | [] -> acc
  | ILi (r1, i, _) :: ISub (r2, r3, r4, _) :: sl when r2 = r3 && r1 = r4 ->
      let i =
        "        sub     " ^ register_from_reg 8 r2 ^ ", " ^ string_of_int i
      in
      translate_x64_nasm (acc ^ i ^ "\n") sl
  | ILi (r1, i, _) :: IAdd (r2, r3, r4, _) :: sl when r2 = r3 && r1 = r4 ->
      let i =
        "        add     " ^ register_from_reg 8 r2 ^ ", " ^ string_of_int i
      in
      translate_x64_nasm (acc ^ i ^ "\n") sl
  | ISub (r1, r2, r3, ctx) :: sl when r1 <> r2 ->
      let c = List.hd ctx in
      let i =
        "        mov     " ^ register_from_reg 8 c ^ ", "
        ^ register_from_reg 8 r2 ^ "\n" ^ "        sub     "
        ^ register_from_reg 8 c ^ ", " ^ register_from_reg 8 r3 ^ "\n"
        ^ "        mov     " ^ register_from_reg 8 r1 ^ ", "
        ^ register_from_reg 8 c ^ "\n"
      in
      translate_x64_nasm (acc ^ i) sl
  | x :: sl -> translate_x64_nasm (acc ^ translate_nasm_x64_single x ^ "\n") sl

(* ---- Compiler Pipeline ---- *)

(*
A platform backend.

Name * Codegen function * bit count * registers
*)
type ccbackend = string * (string -> ir list -> string) * int * reg list

let backends : ccbackend list =
  [
    ("riscv", translate_riscv, 4, [ T0; T1; T2; T3; T4; T5; T6 ]);
    ("nasm", translate_x64_nasm, 8, [ T0; T1; T2; T3; T4; T5 ]);
    ("ir", translate_ir, 4, [ T0; T1; T2; T3; T4; T5; T6 ]);
  ]

let lookup_backend s =
  let rec aux = function
    | [] -> List.hd backends
    | (n, f, a, r) :: _ when n = s -> (n, f, a, r)
    | _ :: l -> aux l
  in
  aux backends

(*
The function to run the compiler.

All phases are passed:
- Lexer
- Parser
- Semantical Analysis
- IR Code Generation (IR = Intermediate Representation)
- Platform specific code generation
*)
let compile ((_, translate, align, regs) : ccbackend) (src : string) : string =
  src |> lex |> parse |> semantic_check align
  |> codegen (regs, align)
  |> translate ""

(* ---- The commandline frontend ---- *)

let input_file = ref ""
let output_file = ref ""
let asm_dialect = ref "riscv"

let speclist =
  [
    ("-c", Arg.String (fun s -> input_file := s), "Input source file");
    ("-o", Arg.String (fun s -> output_file := s), "Output assembler file");
    ("-a", Arg.String (fun s -> asm_dialect := s), "Output assembler dialect");
  ]

let usage_msg = "Usage: mincc -c <input> -o <output> [-a <dialect>]"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* Read input file *)
  let input_content =
    if !input_file = "" then failwith "No input file specified."
    else
      let ic = open_in !input_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
  in

  let outp = input_content |> compile (lookup_backend !asm_dialect) in

  (* Write to output file *)
  if !output_file = "" then failwith "No output file specified."
  else
    let oc = open_out !output_file in
    output_string oc outp;
    close_out oc

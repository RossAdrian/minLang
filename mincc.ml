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
| GREATEREQUALS;;

let is_digit c = c >= '0' && c <= '9';;
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_';;

let keywords = [
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
  ("void", VOID)
];;

let rec lex' cs =
  match cs with
  | [] -> []
  | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: rest -> lex' rest

  (* Identifiers and keywords *)
  | c :: rest when is_letter c ->
      let rec take_ident acc = function
        | x :: xs when is_letter x || is_digit x -> take_ident (x::acc) xs
        | xs -> (List.rev acc, xs)
      in
      let (ident_chars, rest') = take_ident [c] rest in
      let ident = String.of_seq (List.to_seq ident_chars) in
      (match List.assoc_opt ident keywords with
      | Some kw -> kw :: lex' rest'
      | None -> IDENTIFIER ident :: lex' rest')

  (* Numbers *)
  | c :: rest when is_digit c ->
      let rec take_num acc = function
        | x :: xs when is_digit x -> take_num (x::acc) xs
        | xs -> (List.rev acc, xs)
      in
      let (num_chars, rest') = take_num [c] rest in
      let num = int_of_string (String.of_seq (List.to_seq num_chars)) in
      NUMBER num :: lex' rest'

  (* String literals *)
  | '"' :: rest ->
      let rec take_string acc = function
        | '"' :: xs -> (List.rev acc, xs)
        | '\\' :: '"' :: xs -> take_string ('"'::acc) xs
        | x :: xs -> take_string (x::acc) xs
        | [] -> failwith "Unterminated string literal"
      in
      let (str_chars, rest') = take_string [] rest in
      STRING (String.of_seq (List.to_seq str_chars)) :: lex' rest'

  (* Character literals *)
  | '\'' :: c :: '\'' :: rest -> CHARACTER c :: lex' rest
  | '\'' :: _ -> failwith "Malformed character literal"

  (* Symbols and operators *)
  | ':' :: rest -> COLON :: lex' rest
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
  | '-' :: '>' :: rest -> RARROW :: lex' rest
  | '<' :: '-' :: rest -> LARROW :: lex' rest
  | '<' :: '=' :: rest -> LOWEREQUALS :: lex' rest
  | '>' :: '=' :: rest -> GREATEREQUALS :: lex' rest
  | '-' :: rest -> MINUS :: lex' rest
  | '+' :: rest -> PLUS :: lex' rest

  | _ -> failwith "Unknown character in input";;

let lex (s: string) : token list =
  let cs = List.init (String.length s) (String.get s) in
  lex' cs;;

(* ----- The AST ----- *)

type ty =
| FuncTy of ty * ty list
| Void
| Int
| Char
| Ptr of ty;;

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
| Assign;;

type uop =
| Not
| Indir
| Addr;;

type expr =
| CInt of int
| CChar of char
| CString of string
| Binary of binop * expr * expr
| Unary of uop * expr * expr
| Call of string * expr list
| Ident of string
| Conditional of expr * expr * expr;;

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
| Loop of statement;;

type globalDecl =
| Decl of string * ty
| FuncDef of ty * string * (ty * string) list * statement;;

(* ----- The Parser ----- *)

(* Type parser *)
let rec parse_ty = function
| INT :: ts -> (Int, ts)
| CHAR :: ts -> (Char, ts)
| PTR :: LARROW :: ts -> (
    let (t, ts) = parse_incomplete_ty ts in
    match ts with
    | RARROW :: ts -> (Ptr t, ts)
    | _ -> failwith "Missing `>` in pointer type."
  )
| _ -> failwith "Not a type."
and parse_incomplete_ty = function
| VOID :: ts -> (Void, ts)
| ts -> parse_ty ts;;

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

let is_right_assoc = function
  | Assign -> true
  | _ -> false

let peek = function
  | [] -> None
  | t :: _ -> Some t

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
        | _ ->
            let (arg, ts') = parse_expr ts in
            (match ts' with
            | COMMA :: ts'' -> parse_args (arg::acc) ts''
            | RPAREN :: ts'' -> (List.rev (arg::acc), ts'')
            | _ -> failwith "Expected ',' or ')'")
      in
      let (args, ts') = parse_args [] ts in
      (Call (name, args), ts')
  | IDENTIFIER name :: ts -> (Ident name, ts)
  | LPAREN :: ts ->
      let (e, ts') = parse_expr ts in
      let ts'' = expect RPAREN ts' in
      (e, ts'')
  | _ -> failwith "Expected primary expression"

and parse_unary ts =
  match peek ts with
  | Some tkn when token_to_uop tkn <> None ->
      let uop = Option.get (token_to_uop tkn) in
      let ts' = consume ts in
      let (e, ts'') = parse_unary ts' in
      (Unary (uop, e, e), ts'')
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
          let (rhs, ts'') = parse_unary ts' in
          let rec_prec = if right_assoc then prec else prec + 1 in
          let (rhs', ts''') = parse_bin_rhs rec_prec rhs ts'' in
          loop (Binary (op, lhs, rhs')) ts'''
    | _ -> (lhs, ts)
  in
  loop lhs ts

and parse_expr ts =
  let (lhs, ts') = parse_unary ts in
  parse_bin_rhs 1 lhs ts'

(* statement parser *)
let rec parse_stmt = function
| RETURN :: ts -> (
    match ts with
    | SEMICOLON :: ts -> (Return None, ts)
    | _ -> let (e, ts) = parse_expr ts in
    (match ts with
      | SEMICOLON :: ts -> (Return (Some e), ts)
      | _ -> failwith "Expected `;` after return statement."
    )
  )
| LOOP :: ts -> let (s, ts) = parse_stmt ts in (Loop (s), ts)
| WHILE :: ts -> let (e, ts) = parse_expr ts in
                             let (s, ts) = parse_stmt ts in
                             (While (e, s), ts)
| IF :: ts -> (
    let (e, ts) = parse_expr ts in
    let (s, ts) = parse_stmt ts in
    match ts with
    | ELSE :: ts -> (
                       let (selse, ts) = parse_stmt ts in
                       (IfElse (e, s, selse), ts)
                    )
    | _ -> (If (e, s), ts)
  )
| LBRACE :: ts -> parse_block [] ts
| LET :: IDENTIFIER id :: COLON :: ts -> (
    let (t, ts) = parse_ty ts in
    match ts with
    | SEMICOLON :: ts -> (DeclUninit (id, t), ts)
    | ASSIGN :: ts -> (
      let (e, ts) = parse_expr ts in
      match ts with
      | SEMICOLON :: ts -> (Decl (id, t, e), ts)
      | _ -> failwith "Expected `;` after declaration."
    )
    | _ -> failwith "Expected `=` or `;` after declaration."
  )
| LET :: IDENTIFIER id :: ASSIGN :: ts -> (
    let (e, ts) = parse_expr ts in
    match ts with
    | SEMICOLON :: ts -> (DeclInfer (id, e), ts)
    | _ -> failwith "Expected `;` after declaration."
  )
| BREAK :: SEMICOLON :: ts -> (Break, ts)
| CONTINUE :: SEMICOLON :: ts -> (Continue, ts)
| ts -> let (e, ts) = parse_expr ts in
                    match ts with
                    | SEMICOLON :: ts -> (Expr e, ts)
                    | _ -> failwith "Expected semicolon after expression statement."
and parse_block l = function
| RBRACE :: ts -> (Block l, ts)
| ts -> let (s, ts) = parse_stmt ts in
        parse_block (l @ [s]) ts;;

(* Global declaration parser *)

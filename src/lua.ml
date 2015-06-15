(** The state of the parser, a stream and a position. *)
type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line ?(n=0) lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = lcp.pos_lnum + 1;
       pos_bol = lcp.pos_cnum;
    }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme { stream } = Sedlexing.Utf8.lexeme stream


(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok

let raise_ParseError lexbuf =
  let { pos } = lexbuf in
  let tok = lexeme lexbuf in
  let open Lexing in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Printf.fprintf stderr "Parse error: %s\n" (string_of_ParseError (pos.pos_fname, line, col, tok));
  raise @@ ParseError (pos.pos_fname, line, col, tok)


let sedlex_with_menhir lexer' parser' lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lexer' lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised parser'
  in
  try
    parser lexer
  with
    | LuaParser.Error
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
      -> raise_ParseError lexbuf

open LuaParser

let ident_table = function
  | "true"     -> TRUE
  | "false"    -> FALSE
  | "until"    -> UNTIL
  | "do"       -> DO
  | "end"      -> END
  | "while"    -> WHILE
  | "repeat"   -> REPEAT
  | "if"       -> IF
  | "then"     -> THEN
  | "else"     -> ELSE
  | "elseif"   -> ELSEIF
  | "function" -> FUNCTION
  | "for"      -> FOR
  | "in"       -> IN
  | "break"    -> BREAK
  | "return"   -> RETURN
  | "local"    -> LOCAL
  | "or"       -> OR
  | "and"      -> AND
  | "nil"      -> NIL
  | "goto"     -> GOTO
  | x          -> IDENT x

let rec lexer lexbuf =
  let digit = [%sedlex.regexp? '0'..'9'] in
  let number = [%sedlex.regexp? Plus digit, Opt ('.', Plus digit)] in
  (* let string = [%sedlex.regexp? '"', Star ((Sub (any, '\\')) | "\\\\" | "\\\""), '"'] in *)
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n'                                        -> update lexbuf;
    new_line lexbuf;
    lexer lexbuf;
  | white_space                                 -> update lexbuf; lexer lexbuf
  | number                                      -> update lexbuf; NUMBER (lexeme lexbuf)
  | "::"                                        -> update lexbuf; DOUBLECOLON
  | ':'                                         -> update lexbuf; COLON
  | ';'                                         -> update lexbuf; SEMI
  | '.'                                         -> update lexbuf; DOT
  | ".."                                        -> update lexbuf; DOUBLEDOT
  | "..."                                       -> update lexbuf; TRIPLEDOT
  | ','                                         -> update lexbuf; COMMA
  | '<'                                         -> update lexbuf; LT
  | "<="                                        -> update lexbuf; LTE
  | '>'                                         -> update lexbuf; GT
  | ">="                                        -> update lexbuf; GTE
  | "=="                                        -> update lexbuf; EQ
  | "~="                                        -> update lexbuf; DIFF
  | '='                                         -> update lexbuf; ASSIGN
  | '#'                                         -> update lexbuf; OCTO
  | '+'                                         -> update lexbuf; PLUS
  | '-'                                         -> update lexbuf; MINUS
  | '*'                                         -> update lexbuf; TIMES
  | '/'                                         -> update lexbuf; DIV
  | '%'                                         -> update lexbuf; MOD
  | '^'                                         -> update lexbuf; EXP
  | "("                                         -> update lexbuf; OPAR
  | ")"                                         -> update lexbuf; CPAR
  | "["                                         -> update lexbuf; OBRA
  | "]"                                         -> update lexbuf; CBRA
  | "{"                                         -> update lexbuf; OCUR
  | "}"                                         -> update lexbuf; CCUR
  | ('A' .. 'Z' | 'a' .. 'z'),
    Star ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9') -> update lexbuf; ident_table @@ lexeme lexbuf
  | eof                                         -> update lexbuf; EOF
  | _                                           -> update lexbuf; raise_ParseError lexbuf

let () = create_lexbuf @@ Sedlexing.Utf8.from_string "foo[bar] = function(a, b) local c={d = 10}; return a*b+c.d end"
         |> sedlex_with_menhir lexer chunk
         |> Ast.show_block |> print_endline

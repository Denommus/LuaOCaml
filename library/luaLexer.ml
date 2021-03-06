open LuaParser
open SedlexMenhir

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

let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit, Opt ('.', Plus digit)]
let string = [%sedlex.regexp? ('"', Star (('\\', any) | Sub (any, '"')), '"')
             | ('\'', Star (('\\', any) | Sub (any, '\'')), '\'')]

let rec lexer lexbuf =
  let buf = lexbuf_stream lexbuf in
  match%sedlex buf with
  | '\n'                                        -> update lexbuf;
                                                   new_line lexbuf;
                                                   lexer lexbuf;
  | white_space                                 -> update lexbuf; lexer lexbuf
  | number                                      -> update lexbuf; NUMBER (lexeme lexbuf)
  | string                                      -> update lexbuf; STRING (sub_lexeme lexbuf 1 @@
                                                                            (lexeme_length lexbuf) - 2)
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


type lexbuf
val lexbuf_stream : lexbuf -> Sedlexing.lexbuf
val create_lexbuf : ?file:string -> Sedlexing.lexbuf -> lexbuf
val new_line : ?_n:int -> lexbuf -> unit
val update : lexbuf -> unit
val lexeme : lexbuf -> string
val sub_lexeme : lexbuf -> int -> int -> string
val lexeme_length : lexbuf -> int
exception ParseError of (string * int * int * string)
val string_of_ParseError : string * int * int * string -> string
val raise_ParseError : lexbuf -> 'a
val sedlex_with_menhir : (lexbuf -> 'a) -> ('a, 'b) MenhirLib.Convert.traditional -> lexbuf -> 'b

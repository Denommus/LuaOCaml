type unop = UMinus | Not | Octo

val pp_unop : Format.formatter -> unop -> unit

val show_unop : unop -> string

type binop =
  And
| Or
| Plus
| Minus
| Times
| Div
| Mod
| Exp
| DoubleDot
| Lt
| Lte
| Gt
| Gte
| Eq
| Diff

val pp_binop : Format.formatter -> binop -> unit

val show_binop : binop -> string

type funcname = FuncName of var * bool

and var = Name of string | NestedVar of exp * exp

and exp =
  Nil
| False
| True
| Number of float
| String of string
| Tripledot
| FunctionDef of funcbody
| Binop of binop * exp * exp
| Unop of unop * exp
| Var of var
| ExpNested of exp * string
| FuncallPref of functioncall
| Table of field list
and functioncall = Funcall of exp * exp list

and stat =
  EmptyStat
| Assign of (var * exp) list
| FuncallStat of functioncall
| Label of string
| Break
| Goto of string
| BlockStat of block
| WhileStat of exp * block
| RepeatStat of exp * block
| IfStat of exp * block * (exp * block) list * block option
| ForStat of string * exp * exp * exp option * block
| ForInStat of string list * exp list * block
| LocalAssign of var list * exp list option

and field =
  AssignField of exp * exp
| NameField of string * exp
| ExpField of exp

and funcbody = FuncBody of (string list * bool) * block

and retstat = Retstat of exp list

and block = Block of stat list * retstat option

val pp_funcname : Format.formatter -> funcname -> unit

val show_funcname : funcname -> string

val pp_var : Format.formatter -> var -> unit

val show_var : var -> string

val pp_exp : Format.formatter -> exp -> unit

val show_exp : exp -> string

val pp_functioncall : Format.formatter -> functioncall -> unit

val show_functioncall : functioncall -> string

val pp_stat : Format.formatter -> stat -> unit

val show_stat : stat -> string

val pp_field : Format.formatter -> field -> unit

val show_field : field -> string

val pp_funcbody : Format.formatter -> funcbody -> unit

val show_funcbody : funcbody -> string

val pp_retstat : Format.formatter -> retstat -> unit

val show_retstat : retstat -> string

val pp_block : Format.formatter -> block -> unit

val show_block : block -> string

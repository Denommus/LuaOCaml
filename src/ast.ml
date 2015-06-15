type funcname = FuncName of var * bool

and var = Name of string
        | NestedVar of exp * exp

and exp =
    Nil
  | False
  | True
  | Number of string
  | String of string
  | Tripledot
  (* bool: Implicit self *)
  | FunctionDef of funcbody * bool
  | Binop of string * exp * exp
  | Unop of string * exp
  | Var of var
  | ExpNested of exp * string
  | FuncallPref of functioncall
  | Table of field list

and functioncall =
    Funcall of exp * args
  | FuncallColon of exp * string * args

and args =
    ExpArgs of exp list
  | StringArgs of string

and stat =
    EmptyStat
  | Assign of var list * exp list
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

and block = Block of stat list * retstat option [@@deriving show]

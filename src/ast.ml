type funcname = FuncName of string list * string option [@@deriving show]

and exp =
    Nil
  | False
  | True
  | Number of string
  | String of string
  | Tripledot
  | FunctionDef of funcbody
  | Binop of string * exp * exp
  | Unop of string * exp
  | Var of string
  | ExpNested of exp * string
  | FuncallPref of functioncall

and functioncall =
    Funcall of exp * args
  | FuncallColon of exp * string * args

and args =
    ExpArgs of exp list
  | StringArgs of string

and stat =
    EmptyStat
  | Assign of string list * exp list
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
  | FuncStat of funcname * funcbody
  | LocalFuncStat of string * funcbody
  | LocalAssign of string list * exp list

and funcbody = FuncBody of (string list * bool) * block

and retstat = Retstat of exp list

and block = Block of stat list * retstat option [@@deriving show]

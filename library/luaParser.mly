%{
  open Ast
  let rec zip l1 l2 = match (l1, l2) with
      ([], _) | (_, []) -> []
    | (x::xs, y::ys)    -> (x, y) :: zip xs ys
%}
%token EOF
%token GOTO
%token TRUE FALSE
%token UNTIL
%token DO END
%token WHILE
%token REPEAT
%token IF THEN ELSE ELSEIF
%token FUNCTION
%token ASSIGN (* = *)
%token FOR IN
%token BREAK
%token RETURN
%token LOCAL
%token COLON (* : *)
%token DOUBLECOLON (* :: *)
%token SEMI (* ; *)
%token DOT DOUBLEDOT TRIPLEDOT (* . .. ... *)
%token COMMA (* , *)
%token OR AND
%token LT LTE GT GTE EQ DIFF (* < <= > >= == ~= *)
%token OCTO (* # *)
%token NIL
%token <string> IDENT
%token <string> STRING
%token <string> NUMBER
%token PLUS MINUS TIMES DIV MOD EXP (* + - * / % ^ *)
%token OPAR CPAR OBRA CBRA OCUR CCUR (* ( ) [ ] { } *)
%token NOT
%left OR
%left AND
%left LT LTE GT GTE EQ DIFF
%left DOUBLEDOT
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT OCTO
%nonassoc UMINUS
%right EXP
%start chunk
%type <Ast.block> chunk

%%
chunk:
   block EOF { $1 }

block:
   list(stat) option(retstat) { Block ($1, $2) }

elseif:
   ELSEIF exp THEN block { ($2, $4) }

elseiflist:
   l = list(elseif) { l }

elsesta:
   ELSE block { $2 }

stat:
   SEMI { EmptyStat }
 | varlist ASSIGN explist { Assign (zip $1 $3) }
 | functioncall { FuncallStat $1 }
 | label { Label $1 }
 | BREAK { Break }
 | GOTO IDENT { Goto $2 }
 | DO block END { BlockStat $2 }
 | WHILE exp DO block END { WhileStat ($2, $4) }
 | REPEAT block UNTIL exp { RepeatStat ($4, $2) }
 | IF exp THEN
   block
   elseiflist
   option(elsesta) { IfStat ($2, $4, $5, $6) }
 | FOR IDENT ASSIGN exp COMMA exp DO block END { ForStat ($2, $4, $6, None, $8) }
 | FOR IDENT ASSIGN exp COMMA exp COMMA exp DO block END { ForStat ($2, $4, $6, Some $8, $10) }
 | FOR namelist IN explist DO block END { ForInStat ($2, $4, $6) }
 | FUNCTION funcname funcbody {
     let FuncName (varname, has_self) = $2 in
     let FuncBody ((paramlist, vararg), block) = $3 in
     let funcbody = FuncBody (((if has_self then "self" :: paramlist else paramlist), vararg), block) in
     Assign (zip [varname] [FunctionDef funcbody])
   }
 | LOCAL FUNCTION IDENT funcbody {
     BlockStat (Block ([LocalAssign ([Name $3], None); Assign (zip [Name $3] [FunctionDef ($4)])], None))
   }
 | LOCAL namelist option(assignexplist) { LocalAssign (List.map (fun x -> Name x) $2, $3) }

retstat:
   RETURN explist = separated_list(COMMA, exp) option(SEMI) { Retstat explist }

label: DOUBLECOLON IDENT DOUBLECOLON { $2 }

colonname: COLON IDENT { $2 }

funcname: separated_nonempty_list(DOT, IDENT) option(colonname) {
     match $2 with
       Some name -> let rec convert = function
         | x::[] -> NestedVar (Var (Name x), Var (Name name))
         | x::xs -> NestedVar (Var (Name x), Var (convert xs))
         | _ -> raise @@ Failure "weirdness" in
       FuncName (convert $1, true)
     | None -> let rec convert = function
         | x::[] -> Name x
         | x::xs -> NestedVar (Var (Name x), Var (convert xs))
         | _ -> raise @@ Failure "weirdness" in
       FuncName (convert $1, false)
   }

varlist:
   separated_nonempty_list(COMMA, var) { $1 }

var:
   IDENT { Name $1 }
 | prefixexp OBRA exp CBRA { NestedVar ($1, $3) }
 | prefixexp DOT IDENT { NestedVar ($1, Var (Name $3)) }

namelist:
   separated_nonempty_list(COMMA, IDENT) { $1 }

assignexplist:
     ASSIGN explist { $2 }

explist:
   separated_nonempty_list(COMMA, exp) { $1 }

exp:
   NIL { Nil }
 | FALSE { False }
 | TRUE { True }
 | NUMBER { Number (float_of_string $1) }
 | STRING { String $1 }
 | TRIPLEDOT { Tripledot }
 | functiondef { FunctionDef ($1) }
 | prefixexp { $1 }
 | tableconstructor { $1 }
 | e1 = exp
   b = binop
   e2 = exp { Binop (b, e1, e2) }
 | MINUS exp %prec UMINUS { Unop (UMinus, $2) }
 | NOT exp { Unop (Not, $2) }
 | OCTO exp { Unop (Octo, $2) }

prefixexp:
   var { Var $1 }
 | functioncall { FuncallPref $1 }
 | OPAR exp CPAR { $2 }

functioncall:
   prefixexp args { Funcall ($1, $2) }
 | prefixexp COLON IDENT args {
     Funcall (Var (NestedVar ($1, Var (Name $3))), Var (Name $3) :: $4)
   }

args:
   OPAR explist = separated_list(COMMA, exp) CPAR { explist }

functiondef:
   FUNCTION funcbody { $2 }

funcbody: OPAR parlist CPAR block END { FuncBody ($2, $4) }

commatripledot: COMMA TRIPLEDOT { }

parlist: n = namelist o = option(commatripledot) { n, match o with Some _ -> true | None -> false }

tableconstructor: OCUR fieldlist CCUR { Table $2 }

fieldlist: l = separated_list(fieldsep, field) { l }

field:
   OBRA exp CBRA ASSIGN exp { AssignField ($2, $5) }
 | IDENT ASSIGN exp { NameField ($1, $3) }
 | exp { ExpField $1 }

fieldsep: COMMA { } | SEMI { }

%inline binop:
 | AND { And }
 | OR { Or }
 | PLUS { Plus }
 | MINUS { Minus }
 | TIMES { Times }
 | DIV { Div }
 | MOD { Mod }
 | EXP { Exp }
 | DOUBLEDOT { DoubleDot }
 | LT { Lt }
 | LTE { Lte }
 | GT { Gt }
 | GTE { Gte }
 | EQ { Eq }
 | DIFF { Diff }

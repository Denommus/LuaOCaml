%{
  open Ast
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
%token OPAR CPAR (* ( ) *)
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
 | varlist ASSIGN explist { Assign ($1, $3) }
 | functioncall { FuncallStat $1 }
 | label { Label $1 }
 | BREAK { Break }
 | GOTO IDENT { Goto $2 }
 | DO block END { BlockStat $2 }
 | WHILE exp DO block END { WhileStat ($2, $4) }
 | REPEAT block UNTIL exp { RepeatStat ($4, $2) }
 | IF cond = exp THEN
   body = block
   elseifs = elseiflist
   el = option(elsesta) { IfStat (cond, body, elseifs, el) }
 | FOR IDENT ASSIGN exp COMMA exp DO block END { ForStat ($2, $4, $6, None, $8) }
 | FOR IDENT ASSIGN exp COMMA exp COMMA exp DO block END { ForStat ($2, $4, $6, Some $8, $10) }
 | FOR namelist IN explist DO block END { ForInStat ($2, $4, $6) }
 | FUNCTION funcname funcbody { FuncStat ($2, $3) }
 | LOCAL FUNCTION IDENT funcbody { LocalFuncStat ($3, $4) }
 | LOCAL varlist ASSIGN explist { LocalAssign($2, $4) }

retstat:
   RETURN explist = separated_list(COMMA, exp) option(SEMI) { Retstat explist }

label: DOUBLECOLON IDENT DOUBLECOLON { $2 }

colonname: COLON IDENT { $2 }

funcname: l = separated_nonempty_list(COMMA, IDENT) o = option(colonname) { FuncName (l, o) }

varlist:
   l = separated_nonempty_list(COMMA, var) { l }

var:
   IDENT { $1 }

namelist:
   l = separated_nonempty_list(COMMA, IDENT) { l }

explist:
   l = separated_nonempty_list(COMMA, exp) { l }

exp:
   NIL { Nil }
 | FALSE { False }
 | TRUE { True }
 | NUMBER { Number $1 }
 | STRING { String $1 }
 | TRIPLEDOT { Tripledot }
 | functiondef { FunctionDef $1 }
 | prefixexp { $1 }
 | e1 = exp
   b = binop
   e2 = exp { Binop (b, e1, e2) }
 | MINUS exp %prec UMINUS { Unop ("-", $2) }
 | NOT exp { Unop ("not", $2) }
 | OCTO exp { Unop ("#", $2) }

prefixexp:
   var { Var $1 }
 | functioncall { FuncallPref $1 }
 | OPAR exp CPAR { $2 }
 | prefixexp DOT IDENT { ExpNested ($1, $3) }

functioncall:
   prefixexp args { Funcall ($1, $2) }
 | prefixexp COLON IDENT args { FuncallColon ($1, $3, $4) }

args:
   OPAR explist = separated_list(COMMA, exp) CPAR { ExpArgs explist }

functiondef:
   FUNCTION funcbody { $2 }

funcbody: OPAR parlist CPAR block END { FuncBody ($2, $4) }

commatripledot: COMMA TRIPLEDOT { }

parlist: n = namelist o = option(commatripledot) { n, match o with Some _ -> true | None -> false }

%inline binop:
 | AND { "and" }
 | OR { "or" }
 | PLUS { "+" }
 | MINUS { "-" }
 | TIMES { "*" }
 | DIV { "/" }
 | MOD { "%" }
 | EXP { "^" }
 | DOUBLEDOT { ".." }
 | LT { "<" }
 | LTE { "<=" }
 | GT { ">" }
 | GTE { ">=" }
 | EQ { "==" }
 | DIFF { "=~" }

%{
%}
%token EOF
%token TRUE FALSE
%start lua
%type <bool list> lua

%%
lua:
  expressions = list(expression) EOF { expressions }

expression:
 | TRUE { true }
 | FALSE { false }

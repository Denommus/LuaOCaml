open Lua.SedlexMenhir

let () = create_lexbuf @@ Sedlexing.Utf8.from_string "foo['bar'] = function(a, b) local c={d = 10}; return a*b+c.d end"
         |> sedlex_with_menhir Lua.LuaLexer.lexer Lua.LuaParser.chunk
         |> Lua.Ast.show_block |> print_endline

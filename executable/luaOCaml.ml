open Lua.SedlexMenhir

let parse_file file =
  let channel = open_in file in
  let result = Sedlexing.Utf8.from_channel channel
               |> create_lexbuf ~file:file
               |> sedlex_with_menhir Lua.LuaLexer.lexer Lua.LuaParser.chunk in
  close_in channel;
  result

let () = let file_name = Array.get Sys.argv @@ Array.length Sys.argv - 1 in
         parse_file file_name
         |> Lua.Ast.show_block
         |> print_endline

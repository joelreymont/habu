val ast_from_string : string -> (AST.t, string) result
val ast_from_channel : in_channel -> (AST.t, string) result
val ast_from_file : string -> (AST.t, string) result


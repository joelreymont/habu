open Sexplib
open Sexplib.Conv
open Habu
open Sleigh

let%expect_test "parse endian" =
  let text = "define endian = big;" in
  let lexbuf = Lexing.from_string text in
  let res = Driver.parse text lexbuf Sleigh_parser.endian_definition in
  let maybe_tree = Result.to_option res in
  sexp_of_option Definition.sexp_of_t maybe_tree
  |> Sexp.to_string
  |> print_endline;
  [%expect
    {|((Endian((value Big)(position((start_p(""1 0 16))(end_p(""1 0 19)))))))|}]
;;

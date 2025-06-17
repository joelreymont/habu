open Sexplib
open Sexplib.Conv
open Habu
open Sleigh_tree

let%expect_test "parse endian" =
  let text = "define endian = big;" in
  let lexbuf = Lexing.from_string text in
  let res = Driver.parse text lexbuf Sleigh_parser.endian_definition in
  let def = Result.to_option res in
  sexp_of_option Definition.sexp_of_t def |> Sexp.to_string |> print_endline;
  [%expect {| ((Endian((value Big)(tag())))) |}]
;;

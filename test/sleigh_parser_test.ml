open Sexplib
open Sexplib.Conv
open Habu
open Sleigh

let%expect_test "parse endian" =
  let text = "define endian = big;" in
  let lexbuf = Lexing.from_string text in
  let res = Driver.parse text lexbuf Sleigh_parser.endian_definition in
  let def = Result.to_option res in
  let def = Option.map Definition.strip_tags def in
  sexp_of_option (Definition.sexp_of_t sexp_of_unit) def
  |> Sexp.to_string
  |> print_endline;
  [%expect {| ((Endian((value Big)(tag())))) |}]
;;

open Sexplib
open Sexplib.Conv
open Habu
open Sleigh_tree

let parse f s =
  let lexbuf = Lexing.from_string s in
  let res = Driver.parse s lexbuf f in
  Result.to_option res
;;

let%expect_test "parse endian" =
  let text = "define endian = big;" in
  let def = parse Sleigh_parser.endian_definition text in
  sexp_of_option Definition.sexp_of_t def |> Sexp.to_string |> print_endline;
  [%expect {| ((Endian((value Big)(tag())))) |}]
;;

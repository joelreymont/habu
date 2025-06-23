open Sexplib
open Habu

let parse s =
  let lexbuf = Lexing.from_string s in
  let result = Driver.parse s lexbuf Sleigh_parser.grammar in
  match result with
  | Ok tree -> Sleigh_tree.sexp_of_t tree |> Sexp.to_string |> print_endline
  | Error s -> print_endline s
;;

let%expect_test "parse endian" =
  parse {|define endian = big;|};
  [%expect {| ((Endian((value Big)(tag())))) |}]
;;

let%expect_test "parse alignment" =
  let s = {| define alignment = 4; |} in
  parse s;
  [%expect {| ((Alignment((value 4)(tag())))) |}]
;;

let%expect_test "parse space" =
  let s = {| define space ram type=ram_space size=4 wordsize=1 default; |} in
  parse s;
  [%expect
    {| ((Space((id((value ram)(tag())))(kind Ram)(size 4)(word_size 1)(is_default true)))) |}]
;;

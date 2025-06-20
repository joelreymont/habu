open Sexplib
open Sexplib.Conv
open Habu

let parse s =
  let lexbuf = Lexing.from_string s in
  let res = Driver.parse s lexbuf Sleigh_parser.grammar in
  let tree = Result.to_option res in
  sexp_of_option Sleigh_tree.sexp_of_t tree |> Sexp.to_string |> print_endline
;;

let%expect_test "parse endian" =
  parse {|define endian = big;|};
  [%expect {| (((Endian((value Big)(tag()))))) |}]
;;

let%expect_test "parse alignment" =
  let s = {|
   define endian = big;
   define alignment = 4;
  |} in
  parse s;
  [%expect {| (((Endian((value Big)(tag())))(Alignment((value 4)(tag()))))) |}]
;;

let%expect_test "parse space" =
  let s =
    {|
   define endian = big;
   define space ram type=ram_space size=4 wordsize=1 default;
  |}
  in
  parse s;
  [%expect
    {| (((Endian((value Big)(tag())))(Space((id((value ram)(tag())))(kind Ram)(size 4)(word_size 1)(is_default true))))) |}]
;;

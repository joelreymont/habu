open Sexplib
open Habu

let parse s =
  let lexbuf = Lexing.from_string s in
  let result = Driver.parse s lexbuf Sleigh_parser.grammar in
  match result with
  | Ok (tree, _, _) ->
    Sleigh_tree.sexp_of_t tree |> Sexp.to_string |> print_endline
  | Error s -> print_endline s
;;

let%expect_test "parse endian" =
  parse {|define endian = big;|};
  [%expect {| ((Endian Big)) |}]
;;

let%expect_test "parse alignment" =
  let s = {| define alignment = 4; |} in
  parse s;
  [%expect {| ((Alignment 4)) |}]
;;

let%expect_test "parse space" =
  let s = {| define space ram type=ram_space size=4 wordsize=1 default; |} in
  parse s;
  [%expect
    {|
    ((Space((id ram)(kind Ram)(size 4)(word_size 1)(is_default true))))
    |}]
;;

let%expect_test "parse varnode" =
  let s = {| define register offset=2 size=4 [a0 a1 _ a2]; |} in
  parse s;
  [%expect {|
    ((Varnode((registers(a0 a1 _ a2))(size 4)(offset 2))))
    |}]
;;

let%expect_test "parse token" =
  let s =
    {|
      define token instr32(32)
         OpSz        = (31, 31)
         Opc         = (25, 30) signed
      ;
    |}
  in
  parse s;
  [%expect
    {|
    ((Token((id instr32)(bit_size 32)(fields(((id OpSz)(start_bit 31)(end_bit 31)(is_signed false)(is_hex false))((id Opc)(start_bit 25)(end_bit 30)(is_signed true)(is_hex false)))))))
    |}]
;;

let%expect_test "parse attach variables" =
  let s = {| attach variables [Rt Rs] [ a0 a1 ]; |} in
  parse s;
  [%expect {|
    ((Varnode_attach((fields(Rt Rs))(registers(a0 a1)))))
    |}]
;;

let%expect_test "parse constructor, no mnemonic" =
  let s = {|foo: Rt, Ra, Imm15s is unimpl |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output((Id Rt)(Text ,)Whitespace(Id Ra)(Text ,)Whitespace(Id Imm15s)Whitespace))))(pattern())(context())(body()))))
    |}]
;;

let%expect_test "parse constructor, mnemonic with caret" =
  let s = {| :lmw.^LsmwBa_^LsmwId_^LsmwM_ LsmwRb, is unimpl |} in
  parse s;
  [%expect
    {|
    ((Constructor((id lmw.)(display((mnemonic((Text ^LsmwBa_^LsmwId_^LsmwM_)))(output((Id LsmwRb)(Text ,)Whitespace))))(pattern())(context())(body()))))
    |}]
;;

let%expect_test "parse constructor, context" =
  let s = {|foo: bar is [foo = 10;] unimpl |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output((Id bar)Whitespace))))(pattern())(context((Assign(Id foo)(Int 10))))(body()))))
    |}]
;;

let%expect_test "parse assignment: id" =
  let s = {| foo: Rt, Ra, Imm15s is { Rt = Ra + Imm15s; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output((Id Rt)(Text ,)Whitespace(Id Ra)(Text ,)Whitespace(Id Imm15s)Whitespace))))(pattern())(context())(body((Assign(Id Rt)(Binary PLUS(Id Ra)(Id Imm15s))))))))
    |}]
;;

let%expect_test "parse assignment: *" =
  let s = {| foo: Rt, AddrWordRaImm15s is { *AddrWordRaImm15s = Rt; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output((Id Rt)(Text ,)Whitespace(Id AddrWordRaImm15s)Whitespace))))(pattern())(context())(body((Assign(Pointer(Id AddrWordRaImm15s)())(Id Rt)))))))
    |}]
;;

let%expect_test "parse statement as expr" =
  let s = {| foo: a0 is { Lmwbi(a0); } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output((Id a0)Whitespace))))(pattern())(context())(body((Fun_call Lmwbi((Id a0))))))))
    |}]
;;

let%expect_test "parse build" =
  let s = {| foo: is { build foo; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Build foo))))))
    |}]
;;

let%expect_test "parse jump target: n" =
  let s = {| foo: is { goto 0x100; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Goto(Fixed 256())))))))
    |}]
;;

let%expect_test "parse jump target: n[space]" =
  let s = {| foo: is { goto 0x100[const]; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Goto(Fixed 256(const))))))))
    |}]
;;

let%expect_test "parse jump target: id" =
  let s = {| foo: is { goto bar; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Goto(Direct bar)))))))
    |}]
;;

let%expect_test "parse jump target: [id]" =
  let s = {| foo: is { goto [bar]; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Goto(Indirect(Id bar))))))))
    |}]
;;

let%expect_test "parse jump target: <label>" =
  let s = {| foo: is { goto <bar>; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Goto(Label bar)))))))
    |}]
;;

let%expect_test "parse if" =
  let s = {| foo: is { if(Rt == Ra) goto Rel14; } |} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Branch(Paren(Binary EQ(Id Rt)(Id Ra)))(Direct Rel14)))))))
    |}]
;;

let%expect_test "parse export #1" =
  let s = {| foo: is { export d0.lo; }|} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Export(Id d0.lo)))))))
    |}]
;;

let%expect_test "parse export #2" =
  let s = {| foo: is { export *[const]:4 off; }|} in
  parse s;
  [%expect
    {|
    ((Constructor((id foo)(display((mnemonic())(output())))(pattern())(context())(body((Export(Sized(Pointer(Id off)(const))4)))))))
    |}]
;;

let%expect_test "parse pattern" =
  let s =
    {| :fmtsr FRt, FSa is FOpSz=0 & COP=0b110101 & FRt & FSa & MxCP=0b1001 { FSa = FRt; } |}
  in
  parse s;
  [%expect
    {|
    ((Constructor((id fmtsr)(display((mnemonic())(output((Id FRt)(Text ,)Whitespace(Id FSa)Whitespace))))(pattern((Binary(Binary(Binary(Binary(Constraint(Condition FOpSz EQ(Int 0)))AND(Constraint(Condition COP EQ(Int 53))))AND(Constraint(Symbol FRt)))AND(Constraint(Symbol FSa)))AND(Constraint(Condition MxCP EQ(Int 9))))))(context())(body((Assign(Id FSa)(Id FRt)))))))
    |}]
;;

let%expect_test "parse ellipsis" =
  let s = {| :ADC OP1 is (cc=1 & aaa=3) ... & OP1 unimpl |} in
  parse s;
  [%expect
    {|
    ((Constructor((id ADC)(display((mnemonic())(output((Id OP1)Whitespace))))(pattern((Binary(Align_left(Paren(Binary(Constraint(Condition cc EQ(Int 1)))AND(Constraint(Condition aaa EQ(Int 3))))))AND(Constraint(Symbol OP1)))))(context())(body()))))
    |}]
;;

let%expect_test "parse nds32" =
  let cwd = Sys.getcwd () ^ "/../../../../../" in
  let ch = open_in_bin (cwd ^ "sleigh/spec/nds32.slaspec") in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  let lexbuf = Lexing.from_string s in
  let result = Driver.parse s lexbuf Sleigh_parser.grammar in
  (match result with
   | Ok _ -> print_endline "Alright!"
   | Error s -> print_endline s);
  [%expect {| Alright! |}]
;;

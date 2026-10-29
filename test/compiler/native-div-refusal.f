\ native-div-refusal.f - a zero divisor refuses BY NAME in tier-1 compiled code.
\ The contract does not depend on the tier: tier 1 throws the interpreter's
\ E-DIV-ZERO (src/habu/arith-abi.f), the catch resumes, `MIN-N -1 /` is MIN-N
\ and an ordinary division is unchanged. `mod` (src/compiler/native/elaborate.f
\ EXPAND-MODULO) inherits the refusal from the division's schema; literal two
\ uses a signed remainder sequence in the same expansion.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/habu/xref.f
require src/habu/code-bytes.f

package NDIVREF-TEST

public

1 set-tier
: GUARDED ( n n -- n ) / ;
: ARM-TIER-RESET ( -- )
   HB-TARGET-LINUX-X86-64? 0= if s" 0 set-tier" evaluate-closed then ;
ARM-TIER-RESET

private

$1000 constant CAP
20000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF src u OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

\ The runtime cases run in a child: `set-tier` is engine-global state, and a
\ child's own source names its tier. ARM returns to tier 0 for the catch
\ wrappers; the x64 host compiles them at its only tier. GUARDED, the one word
\ whose bytes are read, is compiled here at tier 1.
: RUN ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN src u STORE! ;

\ A case passes when the child EXITED cleanly and printed what the program says
\ it prints. The exit code alone would pass for a program that died before its
\ first division, and the output alone would pass for one that printed and then
\ crashed, so both halves are asserted.
: ASSERT-OK ( ptr u8 n -- ) {: want:ptr wu:n :}
   EXITED @ TTRUE
   RC @ 0 T=
   OUT$ want wu CONTAINS? TTRUE ;

\ The whole program every case runs, spelled once: the divisor and the operation
\ are what change between them. A quotation captures nothing, so the operands
\ travel through variables rather than on the stack.
create SRC-BUF $1000 allot
variable SRC-U

: SRC$ ( -- ptr u8 n )  SRC-BUF SRC-U @ ;

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC-BUF SRC-U @ + u BYTE-COPY
   SRC-U @ u + SRC-U ! ;

: RESET-TIER-SRC ( -- )
   HB-TARGET-LINUX-X86-64? 0= if S\" 0 set-tier\n" SRC+ then ;

: PROGRAM ( ptr u8 n -- ptr u8 n ) {: tail:ptr tu:n :}
   0 SRC-U !
   S\" : FLAG. ( bool -- ) if -1 else 0 then . ;\n1 set-tier\nvariable A\nvariable B\nvariable R\n: DZ ( n n -- n ) / ;\n: DM ( n n -- n ) mod ;\n" SRC+
   RESET-TIER-SRC
   S\" : TRY ( -- n ) [: A @ B @ DZ R ! ;] catch ;\n: TRYM ( -- n ) [: A @ B @ DM R ! ;] catch ;\n" SRC+
   tail tu SRC+
   SRC$ ;

\ Literal divisors exercise the selector's scalar proof through the real native
\ load path. Expected decimal output is supplied by the test, not computed by
\ another compiled division that could share the same lowering error.
: LITERAL-PROGRAM ( ptr u8 n -- ptr u8 n ) {: tail:ptr tu:n :}
   0 SRC-U !
   S\" 1 set-tier\nvariable A\nvariable R\n: LZ ( n -- n ) 0 / ;\n: LMZ ( n -- n ) 0 mod ;\n: L2 ( n -- n ) 2 / ;\n: LM2 ( n -- n ) -2 / ;\n: LR2 ( n -- n ) -2 mod ;\n: LN1 ( n -- n ) -1 / ;\n: LRN1 ( n -- n ) -1 mod ;\n: L64K ( n -- n ) 65536 / ;\n: LR64K ( n -- n ) 65536 mod ;\n" SRC+
   RESET-TIER-SRC
   S\" : TRY ( -- n ) [: A @ LZ R ! ;] catch ;\n: TRYM ( -- n ) [: A @ LMZ R ! ;] catch ;\n: TRY2 ( -- n ) [: A @ L2 R ! ;] catch ;\n" SRC+
   tail tu SRC+
   SRC$ ;

\ The guard is three instructions: `cbnz` of the divisor over one instruction,
\ `bl` to the engine's sealed (DIV-ZERO) helper, the routine the engine's own
\ `/` branches to, and the `sdiv`. That keeps src/compiler/native/emit.f
\ INSN-PER-OP's three instructions per operation true: fourteen discarded
\ divisions over two locals are about 22 operations, which that ceiling sizes
\ for 3 * 22 + 9 = 75 instructions, while a five-instruction guard emits
\ 5 * 14 + 8 = 78 and is refused with E-A64EMIT-CAP. Fourteen and not thirteen,
\ the first count the five-instruction guard refused, keeps one division of
\ margin.
14 constant CAP-DIVS

: CAPACITY-PROGRAM ( -- ptr u8 n )
   0 SRC-U !
   S\" 1 set-tier\n: DIVS ( n n -- n ) {: a:n b:n :}\n" SRC+
   CAP-DIVS 0 ?do S\"    a b / drop\n" SRC+ loop
   S\"    a b - ;\n0 set-tier\n7 2 DIVS . cr\n" SRC+
   SRC$ ;

\ ---- the guard as tier 1 wrote it ---------------------------------------------
\ CODE-BYTES:AT is the bounded view between an engine code address and a
\ readable pointer; this file adds none of its own.
: CODE@ ( n -- n )
   4 CODE-BYTES:AT drop {: p:ptr :}
   p c@
   p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

\ Masks over the fields DDI 0487 gives each form, so a register this file does
\ not name cannot make a word match.
$FFE0FC00 constant SDIV-MASK
$9AC00C00 constant SDIV-X                 \ sdiv Xd,Xn,Xm
$FF000000 constant CBNZ-MASK
$B5000000 constant CBNZ-X                 \ cbnz Xt,label
$FC000000 constant BL-MASK
$94000000 constant BL-OP                  \ bl label

: RM ( n -- n ) 16 rshift 31 and ;
: RT ( n -- n ) 31 and ;
: IMM19 ( n -- n ) 5 rshift $7FFFF and ;

: BL-DEST ( n -- n ) {: at:n :}
   at CODE@ $3FFFFFF and {: imm:n :}
   imm $2000000 and 0<> if imm $4000000 - else imm then  4 *  at + ;

variable DIV-AT                           \ where the last sdiv found stands

: SDIVS ( -- n )
   s" NDIVREF-TEST:GUARDED" XREF-FIND {: rec:ptr :}
   rec XREF-START {: lo:n :}
   0
   rec XREF-CODE-BYTES 4 / 0 ?do
      lo i 4 * +  dup CODE@ SDIV-MASK and SDIV-X = if DIV-AT ! 1+ else drop then
   loop ;

\ The stripped image is the helper's third reader: tools/hb-build.f compiles
\ test/compiler/native-div-image.f at tier 1 keeping only its closure, so the
\ image catches a zero divide only if the closure followed the guard's branch
\ into the helper. test/compiler/native-frame-slot-e2e.f builds and runs it.
: HELPER ( -- n ) s" (DIV-ZERO)" NDICT:HELPER-TARGET ;

: GUARD-CASES ( -- )
   s" a tier-1 division compiles to one sdiv" T-LABEL
   SDIVS 1 T=

   s" the guard is a cbnz of the divisor that jumps over one instruction" T-LABEL
   DIV-AT @ 8 - CODE@ {: guard:n :}
   guard CBNZ-MASK and CBNZ-X T=
   guard RT  DIV-AT @ CODE@ RM  T=
   guard IMM19 2 T=

   s" and that instruction is a bl to the engine's (DIV-ZERO) helper" T-LABEL
   HELPER 0 T<>
   DIV-AT @ 4 - CODE@ BL-MASK and BL-OP T=
   DIV-AT @ 4 - BL-DEST  HELPER T= ;

public

: RUN-ALL ( -- )
   T-RESET

   s" a tier-1 zero divisor is caught by its own code" T-LABEL
   S\" 7 A ! 0 B ! TRY . cr\n" PROGRAM RUN  s" -6400" ASSERT-OK

   s" a tier-1 zero remainder is caught by the same code" T-LABEL
   S\" 7 A ! 0 B ! TRYM . cr\n" PROGRAM RUN  s" -6400" ASSERT-OK

   s" the program RESUMES after the catch" T-LABEL
   S\" 7 A ! 0 B ! TRY drop 7 A ! 2 B ! TRY . R @ . cr\n" PROGRAM RUN
   S\" 0\n3\n" ASSERT-OK

   s" an ordinary tier-1 division still answers, truncating toward zero" T-LABEL
   S\" 7 2 DZ . -7 2 DZ . 7 -2 DZ . cr\n" PROGRAM RUN
   S\" 3\n-3\n-3\n" ASSERT-OK

   s" MIN-N -1 is still the modular answer and not a second refusal" T-LABEL
   S\" $8000000000000000 -1 DZ $8000000000000000 = FLAG. $8000000000000000 -1 DM . cr\n"
   PROGRAM RUN  S\" -1\n0\n" ASSERT-OK

   s" literal zero division and remainder retain catchable refusal" T-LABEL
   S\" 7 A ! TRY . TRYM . cr\n" LITERAL-PROGRAM RUN
   S\" -6400\n-6400\n" ASSERT-OK

   s" literal refusal resumes into a successful literal division" T-LABEL
   S\" 7 A ! TRY drop TRYM drop TRY2 . R @ . cr\n" LITERAL-PROGRAM RUN
   S\" 0\n3\n" ASSERT-OK

   s" literal signed divisors truncate toward zero and preserve remainder sign" T-LABEL
   S\" 7 L2 . -7 L2 . 7 LM2 . -7 LM2 . 7 LR2 . -7 LR2 . cr\n"
   LITERAL-PROGRAM RUN  S\" 3\n-3\n-3\n3\n1\n-1\n" ASSERT-OK

   s" literal minus one preserves modular extrema" T-LABEL
   S\" $8000000000000000 LN1 . $8000000000000000 LRN1 . $7FFFFFFFFFFFFFFF LN1 . cr\n"
   LITERAL-PROGRAM RUN
   S\" -9223372036854775808\n0\n-9223372036854775807\n" ASSERT-OK

   s" wide nonzero literal division and modulo preserve boundary values" T-LABEL
   S\" 65537 L64K . 65537 LR64K . -65537 L64K . -65537 LR64K . $8000000000000000 L64K . $8000000000000000 LR64K . cr\n"
   LITERAL-PROGRAM RUN  S\" 1\n1\n-1\n-1\n-140737488355328\n0\n" ASSERT-OK

   HB-TARGET-LINUX-X86-64? 0= if
      s" fourteen discarded divisions over two locals compile and answer" T-LABEL
      CAPACITY-PROGRAM RUN  S\" 5\n" ASSERT-OK
      GUARD-CASES
   then

   T-REPORT
   s" native-div-refusal: ok" type cr ;

;package

NDIVREF-TEST:RUN-ALL

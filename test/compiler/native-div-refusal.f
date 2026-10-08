\ native-div-refusal.f - a zero divisor refuses BY NAME in tier-1 compiled code.
\
\ The engine's own `/` throws E-DIV-ZERO (lib/errors.f, src/habu/arith-abi.f), so
\ an interpreted program catches a zero divisor and carries on. The native
\ compiler's lowering used to end its guard in a `brk`: the same program compiled
\ with `1 set-tier` died with the crash handler's register dump instead, and no
\ caller could tell the two apart before running. What is asserted here is that
\ the CONTRACT does not depend on the tier: the code caught is the same code, the
\ catch RESUMES - the program prints after it - and the two wrapping contracts
\ the refusal sits between (`MIN-N -1 /` is `MIN-N`, an ordinary division is
\ unchanged) are still what they were.
\
\ `mod` with an unknown or zero divisor is asserted beside `/` because its
\ division and multiply-subtract (src/compiler/native/elaborate.f EXPAND-MODULO)
\ inherits the refusal from the division's schema rather than carrying one of
\ its own. Literal two uses a signed remainder sequence in the same expansion.
\
\ THE GUARD IS THREE INSTRUCTIONS: `cbnz` of the divisor over one instruction,
\ `bl` to the engine's sealed (DIV-ZERO) helper - the routine the engine's own
\ `/` branches to - and the `sdiv`. It is read out of a word's baked span through
\ src/habu/xref.f, and it is what keeps the emitter's three instructions per
\ operation (src/compiler/native/emit.f INSN-PER-OP) true: a word of fourteen
\ discarded divisions over two locals is about 22 operations, which that ceiling
\ sizes for 3 * 22 + 9 = 75 instructions, while a five-instruction guard emits
\ 5 * 14 + 8 = 78 of them and refused the word with E-A64EMIT-CAP.
\
\ THE STRIPPED IMAGE is the helper's third reader. tools/hb-build.f compiles
\ test/compiler/native-div-image.f at tier 1 and keeps only what its closure
\ reaches, so the image catches the zero divide only if the closure followed the
\ guard's branch into the helper. The directory the case prints keeps the image.
\ The subject requires nothing, so the build's maker runs on the keyed linker
\ image (test/preloaded-engine.f).
\
\ The runtime cases run in a child process: `set-tier` is engine-global state,
\ and a child's own source names its tier. The one word whose bytes are read is
\ compiled here between `1 set-tier` and `0 set-tier`, so nothing after it moves.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/engine-candidate.f
require src/habu/xref.f
require src/habu/code-bytes.f
require test/gate-common.f
require test/preloaded-engine.f

package NDIVREF-TEST

public

1 set-tier
: GUARDED ( n n -- n ) / ;
0 set-tier

private

$1000 constant CAP
20000 constant TIMEOUT-MS
600000 constant BUILD-MS             \ one stripped build on a loaded box

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
: PROLOGUE$ ( -- ptr u8 n )
   S\" 1 set-tier\nvariable A\nvariable B\nvariable R\n: DZ ( n n -- n ) / ;\n: DM ( n n -- n ) mod ;\n0 set-tier\n: TRY ( -- n ) [: A @ B @ DZ R ! ;] catch ;\n: TRYM ( -- n ) [: A @ B @ DM R ! ;] catch ;\n" ;

\ Literal divisors exercise the selector's scalar proof through the real native
\ load path. Expected decimal output is supplied by the test, not computed by
\ another compiled division that could share the same lowering error.
: LITERAL-PROLOGUE$ ( -- ptr u8 n )
   S\" 1 set-tier\nvariable A\nvariable R\n: LZ ( n -- n ) 0 / ;\n: LMZ ( n -- n ) 0 mod ;\n: L2 ( n -- n ) 2 / ;\n: LM2 ( n -- n ) -2 / ;\n: LR2 ( n -- n ) -2 mod ;\n: LN1 ( n -- n ) -1 / ;\n: LRN1 ( n -- n ) -1 mod ;\n: L64K ( n -- n ) 65536 / ;\n: LR64K ( n -- n ) 65536 mod ;\n0 set-tier\n: TRY ( -- n ) [: A @ LZ R ! ;] catch ;\n: TRYM ( -- n ) [: A @ LMZ R ! ;] catch ;\n: TRY2 ( -- n ) [: A @ L2 R ! ;] catch ;\n" ;

create SRC-BUF $1000 allot
variable SRC-U

: SRC$ ( -- ptr u8 n )  SRC-BUF SRC-U @ ;

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC-BUF SRC-U @ + u BYTE-COPY
   SRC-U @ u + SRC-U ! ;

: JOIN-PROGRAM ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: head:ptr hu:n tail:ptr tu:n :}
   0 SRC-U !
   head hu SRC+
   tail tu SRC+
   SRC$ ;

: PROGRAM ( ptr u8 n -- ptr u8 n ) {: tail:ptr tu:n :}
   PROLOGUE$ tail tu JOIN-PROGRAM ;

: LITERAL-PROGRAM ( ptr u8 n -- ptr u8 n ) {: tail:ptr tu:n :}
   LITERAL-PROLOGUE$ tail tu JOIN-PROGRAM ;

\ Fourteen and not thirteen, the first count the five-instruction guard refused,
\ so the case keeps one division of margin over the derivation in the header.
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

\ ---- the stripped image -------------------------------------------------------
create IMAGE FS-PATH-CAP allot
variable IMAGE-U

: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: BUILD-IMAGE ( -- )
   PRELOADED-ENGINE:LINKER$ {: linker:ptr linkeru:n :}
   s" ndiv-image" HB-TMP-MKDIR GT-COPY-ROOT!
   s" ndiv-image" IMAGE GT-PATH IMAGE-U !
   GE-HB-RESET
   s" --load" GE-ARG+
   s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+
   s" test/compiler/native-div-image.f" GE-ARG+
   s" -o" GE-ARG+
   IMAGE$ GE-ARG+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN linker linkeru >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ BUILD-MS GE-RUN-ENV ;

: IMAGE-CASES ( -- )
   BUILD-IMAGE
   s" tools/hb-build.f builds the stripped division image" T-LABEL
   T-LABEL$ GE-RC@ 0 T=
   GT-ERR$ nip 0 T=
   IMAGE$ EXECUTABLE? {: built:bool :}
   built TTRUE
   built 0= if GT-ERR$ type exit then

   GE-HB-RESET
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" the stripped image catches its compiled zero divide by the code" T-LABEL
   T-LABEL$ GE-RC@ 0 T=
   GT-OUT$ S\" -6400\n-1\n0\n0\n1\n0\n0\n1\n-1\n-7\n0\n-2\n-1\n" T$=
   s" artifacts: " type GT-ROOT type cr ;

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
   S\" $8000000000000000 -1 DZ $8000000000000000 = . $8000000000000000 -1 DM . cr\n"
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

   s" fourteen discarded divisions over two locals compile and answer" T-LABEL
   CAPACITY-PROGRAM RUN  S\" 5\n" ASSERT-OK

   GUARD-CASES
   IMAGE-CASES

   T-REPORT
   s" native-div-refusal: ok" type cr ;

;package

NDIVREF-TEST:RUN-ALL

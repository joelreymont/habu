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
\ `mod` is asserted beside `/` because a compiled `mod` is a division and a
\ multiply-subtract (src/compiler/native/elaborate.f EXPAND-MODULO): it inherits
\ the refusal from the division's schema rather than carrying one of its own, so
\ a lowering that refused only where the source spelled `/` passes the first case
\ and fails the second.
\
\ EVERY CASE RUNS IN A CHILD PROCESS. `set-tier` is engine-global state, and a
\ suite that selected tier 1 in this process would decide for every file loaded
\ after it.
\ Tier-neutral by design: every case runs in a child whose own source names the
\ tier, so the tier of this row selects nothing.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package NDIVREF-TEST

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

: STORE! ( len len outcome -- )
   MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: RUN ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN STORE! ;

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

: JOIN-PROGRAM ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: head:ptr hu:n tail:ptr tu:n :}
   head SRC-BUF hu BYTE-COPY
   tail SRC-BUF hu + tu BYTE-COPY
   hu tu + SRC-U !
   SRC$ ;

: PROGRAM ( ptr u8 n -- ptr u8 n ) {: tail:ptr tu:n :}
   PROLOGUE$ tail tu JOIN-PROGRAM ;

: LITERAL-PROGRAM ( ptr u8 n -- ptr u8 n ) {: tail:ptr tu:n :}
   LITERAL-PROLOGUE$ tail tu JOIN-PROGRAM ;

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

   T-REPORT
   s" native-div-refusal: ok" type cr ;

;package

NDIVREF-TEST:RUN-ALL

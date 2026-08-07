\ codegen-callsite-inventory.f - the call-site measurement, and the ways a tool
\ that decides WHERE a data-stack access sits can be wrong while looking right.
\ One concern: tools/codegen-callsite-inventory.f.
\
\ WHY A CLASSIFIER HERE NEEDS ITS OWN SUITE. The tool decides what an emitted
\ four-byte word IS and then decides what its POSITION means, and every wrong
\ answer is silent. A decoder that read the frame's own `sub sp, sp, #16` as an
\ adjustment of the data-stack pointer would report a phantom access in every
\ framed routine; one that read the scaled load as the unscaled one would count
\ ordinary memory work as argument traffic; and a position rule that called every
\ access a call site would report the engine boundary as the convention's prize,
\ which is the one number this file exists to keep honest. None of those shows up
\ as a failure anywhere - they show up as a number somebody then plans work
\ against.
\
\ THE FIXTURES THAT MATTER MOST ARE THE NEAR-MISSES. For each form there is a
\ word that matches and one that must not: the same load through a base that is
\ not the data-stack pointer, the SCALED load at the same registers and offset,
\ an adjustment that writes the data-stack pointer from another register and one
\ that writes another register from it, and the frame's own adjustment over
\ register 31. A predicate that answered on shape alone passes none of them.
\
\ AND THE NEGATIVE OFFSET IS NOT DECORATION. The unscaled forms exist in this
\ tree precisely because an access BELOW the base has no spelling in the unsigned
\ ones, so the chain really emits them, and a decoder that failed to sign-extend
\ would re-encode a different word and answer false for every one of them. The
\ round trip is asserted at a negative offset for that reason: it is the case
\ that kills the mutation, and nothing else in the suite separates them.
\
\ THE LAST GROUP IS THE REAL THING: routines the native chain really compiled,
\ counted through the same entry the report uses. TAIL-BIG-N is the whole point
\ of the measurement - a tail call that passes its argument by leaving it where
\ the callee already reads it, so it carries no marshalling at all - and
\ NONTAIL-N is the same call NOT in tail position, which carries one. C5-PAIR-N
\ is the arity-two callee whose four accesses are all its OWN entry and exit and
\ none of them a call site, which is what says the two columns are really
\ separate.

require lib/test.f
require lib/prelude.f
require src/arch/arm64/asm.f
require tools/codegen-callsite-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated5.f

package NSITEINV-TEST

using NSITEINV

private

\ The registers the fixtures below name. x18 is what no emitted routine may hold
\ and the classifiers screen it, so no fixture may be built from it.
0 constant R0
1 constant R1
19 constant RBASE                  \ the data-stack pointer
31 constant RSP                    \ the stack pointer: the frame's own base
18 constant RRSVD                  \ x18, which no routine holds - and d18,
                                   \ which is an ordinary floating register

public

: FORM-CASES ( -- )
   s" a load and a store through the data-stack pointer are its accesses" T-LABEL
   R0 RBASE 8 ENC-LDUR DACCESS? TTRUE
   R0 RBASE 8 ENC-STUR DACCESS? TTRUE

   s" and the same forms through another base are not" T-LABEL
   R0 R1 8 ENC-LDUR DACCESS? TFALSE
   R0 R1 8 ENC-STUR DACCESS? TFALSE

   s" the SCALED load and store are different instructions, not these" T-LABEL
   R0 RBASE 8 ENC-LDR DACCESS? TFALSE
   R0 RBASE 8 ENC-STR DACCESS? TFALSE

   s" a negative offset round-trips, which is why the field is sign-extended" T-LABEL
   R0 RBASE -8 ENC-LDUR DACCESS? TTRUE
   R0 RBASE -256 ENC-STUR DACCESS? TTRUE

   s" the D file's two are the same traffic and are counted with them" T-LABEL
   R0 RBASE 8 ENC-LDURD DACCESS? TTRUE
   R0 RBASE 8 ENC-STURD DACCESS? TTRUE
   R0 RBASE -8 ENC-LDURD DACCESS? TTRUE
   R0 RBASE -256 ENC-STURD DACCESS? TTRUE

   s" and the same three near-misses hold for them" T-LABEL
   R0 R1 8 ENC-LDURD DACCESS? TFALSE
   R0 R1 8 ENC-STURD DACCESS? TFALSE
   R0 RBASE 8 ENC-LDRD DACCESS? TFALSE
   R0 RBASE 8 ENC-STRD DACCESS? TFALSE

   s" d18 is an ordinary D register, so an access transferring it is one" T-LABEL
   RRSVD RBASE 8 ENC-LDURD DACCESS? TTRUE
   RRSVD RBASE 8 ENC-STURD DACCESS? TTRUE

   s" an access is not an adjustment and an adjustment is not an access" T-LABEL
   R0 RBASE 8 ENC-LDUR DADJUST? TFALSE
   R0 RBASE 8 ENC-LDURD DADJUST? TFALSE
   RBASE RBASE 16 ENC-ADDI DACCESS? TFALSE ;

: ADJUST-CASES ( -- )
   s" moving the data-stack pointer, either way, is an adjustment" T-LABEL
   RBASE RBASE 16 ENC-ADDI DADJUST? TTRUE
   RBASE RBASE 16 ENC-SUBI DADJUST? TTRUE

   s" THE FRAME'S OWN adjustment is the same form over register 31" T-LABEL
   RSP RSP 16 ENC-SUBI DADJUST? TFALSE
   RSP RSP 16 ENC-ADDI DADJUST? TFALSE

   s" and an adjustment that names the pointer at one end only is not one" T-LABEL
   R0 RBASE 16 ENC-ADDI DADJUST? TFALSE
   RBASE R0 16 ENC-ADDI DADJUST? TFALSE

   s" either kind answers the combined question" T-LABEL
   R0 RBASE 8 ENC-LDUR DSTACK? TTRUE
   RBASE RBASE 16 ENC-ADDI DSTACK? TTRUE
   RSP RSP 16 ENC-SUBI DSTACK? TFALSE
   R0 R1 R1 ENC-ADD DSTACK? TFALSE ;

\ ---- the real routines --------------------------------------------------------

: COUNT-CASE ( ptr u8 n n n -- ) {: a:ptr u:n site:n own:n :}
   a u NSITEINV:ROW!
   NSITEINV:SITE site T=
   NSITEINV:OWN own T= ;

: ROUTINE-CASES ( -- )
   s" a tail call carries NO marshalling: the argument is already where the callee reads it" T-LABEL
   s" CODEGEN-CORPUS5:TAIL-BIG-N" 0 0 COUNT-CASE
   s" CODEGEN-CORPUS5:TAIL-MID-N" 0 0 COUNT-CASE
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" 0 0 COUNT-CASE

   s" the same call NOT in tail position carries one, and one of its own" T-LABEL
   s" CODEGEN-CORPUS5:NONTAIL-N" 1 1 COUNT-CASE

   s" a caller that COMPUTES its argument pays a store the pass-through ones do not" T-LABEL
   s" CODEGEN-CORPUS5:TAIL-WORK-N" 1 1 COUNT-CASE

   s" an arity-two callee's four accesses are all its own, and none a call site" T-LABEL
   s" CODEGEN-CORPUS5:C5-PAIR-N" 0 4 COUNT-CASE

   s" a routine with no call at all has no call site to marshal for" T-LABEL
   s" CODEGEN-CORPUS5:C5-LONG-N" 0 2 COUNT-CASE
   s" CODEGEN-CORPUS:ADD3-N" 0 3 COUNT-CASE

   s" and the two rows that really do marshal, which is where the prize is" T-LABEL
   s" CODEGEN-CORPUS:FACT-N" 3 2 COUNT-CASE
   s" CODEGEN-CORPUS3:T-REL-L2-N" 4 3 COUNT-CASE ;

: RUN ( -- )
   FORM-CASES
   ADJUST-CASES
   ROUTINE-CASES ;

;using

;package

T-RESET
NSITEINV-TEST:RUN
T-REPORT

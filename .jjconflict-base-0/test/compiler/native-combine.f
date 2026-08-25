\ native-combine.f - production combine rewrites and their emitted results.

require lib/test.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/compiler.f
require tools/codegen-combine-inventory.f

package NCT-FIXTURE

public

\ ---- the programs under test ------------------------------------------------
\ Ordinary definitions go through the production native compiler. The cases
\ below inspect their emitted instructions and execute them.

: NCT-SQSUM ( n n -- n )
   dup * swap dup * + ;

: NCT-MAD3 ( n -- n )
   3 * 5 + ;

: NCT-ACC ( n n n -- n ) {: a:n b:n c:n :}
   a b * c + ;

: NCT-ACC2 ( n n n -- n ) {: a:n b:n c:n :}
   c a b * + ;

: NCT-TWICE ( n n -- n ) {: a:n b:n :}
   a b * dup + ;

: NCT-SPLIT ( n n -- n ) {: a:n b:n :}
   a b * dup 7 xor + ;

: NCT-TWO ( n n n -- n ) {: a:n b:n c:n :}
   a b * c +  a c * b +  + ;

: NCT-IADD ( n -- n )
   5 + ;

: NCT-ISUB ( n -- n )
   5 - ;

: NCT-IRSUB ( n -- n )
   5 swap - ;

: NCT-ISHARED ( n n -- n ) {: a:n b:n :}
   a 9 + b 9 + + ;

: NCT-IMAX ( n -- n )
   4095 + ;

: NCT-MAND ( n -- n )
   7 and ;

: NCT-MORR ( n -- n )
   7 or ;

: NCT-MEOR ( n -- n )
   7 xor ;

: NCT-MSWAP ( n -- n )
   7 swap and ;

: NCT-MHOLE ( n -- n )
   5 and ;

: NCT-MZERO ( n -- n )
   0 and ;

: NCT-MALL ( n -- n )
   -1 and ;

: NCT-MSHARED ( n n -- n ) {: a:n b:n :}
   a 7 and b 7 and + ;

: NCT-IOVER ( n -- n )
   4096 + ;

: NCT-CEQ ( n -- bool )
   7 = ;

: NCT-CZERO ( n -- bool )
   0= ;

: NCT-CMAX ( n -- bool )
   4095 = ;

: NCT-COVER ( n -- bool )
   4096 = ;

: NCT-CNEG ( n -- bool )
   -1 = ;

: NCT-CSWAP ( n -- bool )
   7 swap < ;

: NCT-CBR ( n n -- n ) {: a:n b:n :}
   a 7 < if 0 exit then a b / ;

: NCT-CSHARED ( n n -- n ) {: a:n b:n :}
   a 9 < if 9 exit then a b / ;

;package

package NCT-TEST

using NCOMBINV

private

\ The ends of the signed range, where a wrapping product and a wrapping sum are
\ most likely to disagree with an operand order that is wrong.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ How many multiply-adds a published routine's emitted code holds.
: MADDS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:MADD-INSNS ;

\ And how many plain multiplies, so a case can say that the pair really went and
\ not merely that a multiply-add appeared beside it.
: MULS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:MULS ;

\ How many folded constants a published routine's emitted code holds, and how
\ many move-wides survive beside them.
: ADDIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:ADDI-INSNS ;

: SUBIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:SUBI-INSNS ;

\ How many comparisons the row makes against a register and how many against a
\ number the instruction carries. Both are read, never only one: a fold that
\ worked took a comparison OFF the register path and put it on the immediate
\ one, so a row where the immediate count rose and the register count did not
\ fall gained a comparison instead of folding one.
: CMPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:CMP-INSNS ;

: CMPIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:CMPI-INSNS ;

\ The same reading for the three logical immediate forms.
: ANDIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:ANDI-INSNS ;

: ORRIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:ORRI-INSNS ;

: EORIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:EORI-INSNS ;

TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

public

: FIRED-CASES ( -- )
   s" the canonical shape holds one multiply-add and one surviving multiply" T-LABEL
   s" NCT-FIXTURE:NCT-SQSUM" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-SQSUM" MULS-IN 1 T=

   s" a multiply and an addition of constants become one instruction" T-LABEL
   s" NCT-FIXTURE:NCT-MAD3" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-MAD3" MULS-IN 0 T=

   s" and so do a multiply and an addition of three arguments" T-LABEL
   s" NCT-FIXTURE:NCT-ACC" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-ACC" MULS-IN 0 T=

   s" the addend may be either operand of the addition" T-LABEL
   s" NCT-FIXTURE:NCT-ACC2" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-ACC2" MULS-IN 0 T=

   s" two independent pairs in one body become two multiply-adds" T-LABEL
   s" NCT-FIXTURE:NCT-TWO" MADDS-IN 2 T=
   s" NCT-FIXTURE:NCT-TWO" MULS-IN 0 T=

   3 5 NCT-FIXTURE:NCT-SQSUM 34 T=
   4 NCT-FIXTURE:NCT-MAD3 17 T=
   3 5 7 NCT-FIXTURE:NCT-ACC 22 T=
   3 5 7 NCT-FIXTURE:NCT-ACC2 22 T=
   3 5 7 NCT-FIXTURE:NCT-TWO 48 T= ;

: REFUSED-CASES ( -- )
   s" a product the addition reads TWICE is not folded" T-LABEL
   s" NCT-FIXTURE:NCT-TWICE" MADDS-IN 0 T=
   s" NCT-FIXTURE:NCT-TWICE" MULS-IN 1 T=

   s" and neither is one read by an addition and by something else" T-LABEL
   s" NCT-FIXTURE:NCT-SPLIT" MADDS-IN 0 T=
   s" NCT-FIXTURE:NCT-SPLIT" MULS-IN 1 T=
   3 5 NCT-FIXTURE:NCT-TWICE 30 T=
   3 5 NCT-FIXTURE:NCT-SPLIT 23 T= ;

: IMM-FIRED-CASES ( -- )
   s" a small constant added to a value becomes the addition's own immediate"
   T-LABEL
   s" NCT-FIXTURE:NCT-IADD" ADDIS-IN 1 T=

   s" and subtracted, it becomes the subtraction's - not a negated addition"
   T-LABEL
   s" NCT-FIXTURE:NCT-ISUB" SUBIS-IN 1 T=
   s" NCT-FIXTURE:NCT-ISUB" ADDIS-IN 0 T=

   s" the largest value the field holds still folds" T-LABEL
   s" NCT-FIXTURE:NCT-IMAX" ADDIS-IN 1 T=
   7 NCT-FIXTURE:NCT-IADD 12 T=
   7 NCT-FIXTURE:NCT-ISUB 2 T=
   1 NCT-FIXTURE:NCT-IMAX 4096 T= ;

: IMM-REFUSED-CASES ( -- )
   s" a constant the value is subtracted FROM is not folded" T-LABEL
   s" NCT-FIXTURE:NCT-IRSUB" SUBIS-IN 0 T=
   s" NCT-FIXTURE:NCT-IRSUB" ADDIS-IN 0 T=

   s" nor is one a second reader still needs" T-LABEL
   s" NCT-FIXTURE:NCT-ISHARED" ADDIS-IN 0 T=

   s" nor is the first value too large for the field" T-LABEL
   s" NCT-FIXTURE:NCT-IOVER" ADDIS-IN 0 T=
   7 NCT-FIXTURE:NCT-IRSUB -2 T=
   3 5 NCT-FIXTURE:NCT-ISHARED 26 T=
   1 NCT-FIXTURE:NCT-IOVER 4097 T= ;

: CMP-FIRED-CASES ( -- )
   s" a comparison against a small constant carries it in the instruction"
   T-LABEL
   s" NCT-FIXTURE:NCT-CEQ" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CEQ" CMPS-IN 0 T=

   s" and so does `0=`, which is a constant zero and an equality" T-LABEL
   s" NCT-FIXTURE:NCT-CZERO" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CZERO" CMPS-IN 0 T=

   s" the largest value the field holds still folds" T-LABEL
   s" NCT-FIXTURE:NCT-CMAX" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CMAX" CMPS-IN 0 T=

   s" and a comparison FUSED into a branch folds the same way" T-LABEL
   s" NCT-FIXTURE:NCT-CBR" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CBR" CMPS-IN 0 T=
   7 NCT-FIXTURE:NCT-CEQ TTRUE
   6 NCT-FIXTURE:NCT-CEQ TFALSE
   0 NCT-FIXTURE:NCT-CZERO TTRUE
   4095 NCT-FIXTURE:NCT-CMAX TTRUE
   6 3 NCT-FIXTURE:NCT-CBR 0 T= ;

: CMP-REFUSED-CASES ( -- )
   s" a value one past the field is not folded" T-LABEL
   s" NCT-FIXTURE:NCT-COVER" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-COVER" CMPS-IN 1 T=

   s" nor is a negative one, which is a `cmn` this dialect does not carry"
   T-LABEL
   s" NCT-FIXTURE:NCT-CNEG" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-CNEG" CMPS-IN 1 T=

   s" nor is a constant on the LEFT, which is the mirrored relation" T-LABEL
   s" NCT-FIXTURE:NCT-CSWAP" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-CSWAP" CMPS-IN 1 T=

   s" nor is one a second reader still needs" T-LABEL
   s" NCT-FIXTURE:NCT-CSHARED" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-CSHARED" CMPS-IN 1 T=
   4096 NCT-FIXTURE:NCT-COVER TTRUE
   -1 NCT-FIXTURE:NCT-CNEG TTRUE
   8 NCT-FIXTURE:NCT-CSWAP TTRUE
   10 3 NCT-FIXTURE:NCT-CSHARED 3 T= ;

\ The mask fold: which masks reach the instruction, which stay in a register,
\ and - the row that matters most - that a mask with no encoding is DECLINED
\ rather than handed to a packer that would end the process on it.
: MASK-FIRED-CASES ( -- )
   s" a small mask over a value becomes the operation's own immediate" T-LABEL
   s" NCT-FIXTURE:NCT-MAND" ANDIS-IN 1 T=
   s" NCT-FIXTURE:NCT-MORR" ORRIS-IN 1 T=
   s" NCT-FIXTURE:NCT-MEOR" EORIS-IN 1 T=

   s" and it folds from either operand, because all three are commutative" T-LABEL
   s" NCT-FIXTURE:NCT-MSWAP" ANDIS-IN 1 T=
   9 NCT-FIXTURE:NCT-MAND 1 T=
   8 NCT-FIXTURE:NCT-MORR 15 T=
   8 NCT-FIXTURE:NCT-MEOR 15 T=
   9 NCT-FIXTURE:NCT-MSWAP 1 T= ;

: MASK-REFUSED-CASES ( -- )
   s" a mask the field cannot describe is not folded, though it is smaller" T-LABEL
   s" NCT-FIXTURE:NCT-MHOLE" ANDIS-IN 0 T=

   s" nor is a mask of no ones or a mask of nothing but ones" T-LABEL
   s" NCT-FIXTURE:NCT-MZERO" ANDIS-IN 0 T=
   s" NCT-FIXTURE:NCT-MALL" ANDIS-IN 0 T=

   s" nor is one a second reader still needs" T-LABEL
   s" NCT-FIXTURE:NCT-MSHARED" ANDIS-IN 0 T=
   7 NCT-FIXTURE:NCT-MHOLE 5 T=
   7 NCT-FIXTURE:NCT-MZERO 0 T=
   7 NCT-FIXTURE:NCT-MALL 7 T=
   9 6 NCT-FIXTURE:NCT-MSHARED 7 T= ;

: ANSWER-CASES ( -- )
   s" multiply-add rewrites preserve wraparound arithmetic" T-LABEL
   0 0 NCT-FIXTURE:NCT-SQSUM 0 T=
   -1 -1 NCT-FIXTURE:NCT-SQSUM 2 T=
   7 -9 NCT-FIXTURE:NCT-SQSUM 130 T=
   MAX-INT 1 NCT-FIXTURE:NCT-SQSUM 2 T=
   MIN-INT 1 NCT-FIXTURE:NCT-SQSUM 1 T=
   0 NCT-FIXTURE:NCT-MAD3 5 T=
   -1 NCT-FIXTURE:NCT-MAD3 2 T=
   MIN-INT NCT-FIXTURE:NCT-MAD3 MIN-INT 5 + T=
   MAX-INT MAX-INT MAX-INT NCT-FIXTURE:NCT-ACC MIN-INT T=
   MIN-INT MIN-INT MIN-INT NCT-FIXTURE:NCT-ACC2 MIN-INT T=
   MIN-INT MAX-INT MIN-INT NCT-FIXTURE:NCT-TWO MAX-INT T=

   s" declined rewrites preserve the unfused result" T-LABEL
   -1 -1 NCT-FIXTURE:NCT-TWICE 2 T=
   MIN-INT MIN-INT NCT-FIXTURE:NCT-TWICE 0 T=
   -1 -1 NCT-FIXTURE:NCT-SPLIT 7 T=
   MIN-INT MIN-INT NCT-FIXTURE:NCT-SPLIT 7 T= ;

: IMM-ANSWER-CASES ( -- )
   s" immediate and register forms preserve signed-edge results" T-LABEL
   MAX-INT NCT-FIXTURE:NCT-IADD MIN-INT 4 + T=
   MIN-INT NCT-FIXTURE:NCT-IADD MIN-INT 5 + T=
   MAX-INT NCT-FIXTURE:NCT-ISUB MAX-INT 5 - T=
   MIN-INT NCT-FIXTURE:NCT-ISUB MAX-INT 4 - T=
   MAX-INT NCT-FIXTURE:NCT-IRSUB MIN-INT 6 + T=
   MIN-INT NCT-FIXTURE:NCT-IRSUB MIN-INT 5 + T=
   MAX-INT NCT-FIXTURE:NCT-IOVER MIN-INT 4095 + T=
   MIN-INT NCT-FIXTURE:NCT-IOVER MIN-INT 4096 + T=
   MAX-INT MAX-INT NCT-FIXTURE:NCT-ISHARED 16 T=
   MIN-INT MIN-INT NCT-FIXTURE:NCT-ISHARED 18 T= ;

: MASK-ANSWER-CASES ( -- )
   s" logical immediates preserve zero, all-ones, and sign-bit masks" T-LABEL
   -1 NCT-FIXTURE:NCT-MAND 7 T=
   MIN-INT NCT-FIXTURE:NCT-MAND 0 T=
   -1 NCT-FIXTURE:NCT-MORR -1 T=
   MIN-INT NCT-FIXTURE:NCT-MORR MIN-INT 7 + T=
   -1 NCT-FIXTURE:NCT-MEOR -8 T=
   MIN-INT NCT-FIXTURE:NCT-MEOR MIN-INT 7 + T=
   MIN-INT NCT-FIXTURE:NCT-MSWAP 0 T=
   -1 NCT-FIXTURE:NCT-MHOLE 5 T=
   MIN-INT NCT-FIXTURE:NCT-MZERO 0 T=
   MIN-INT NCT-FIXTURE:NCT-MALL MIN-INT T=
   MAX-INT MAX-INT NCT-FIXTURE:NCT-MSHARED 14 T=
   MIN-INT MIN-INT NCT-FIXTURE:NCT-MSHARED 0 T= ;

: CMP-ANSWER-CASES ( -- )
   s" folded and declined comparisons preserve boundary truth values" T-LABEL
   6 NCT-FIXTURE:NCT-CEQ TFALSE
   7 NCT-FIXTURE:NCT-CEQ TTRUE
   8 NCT-FIXTURE:NCT-CEQ TFALSE
   0 NCT-FIXTURE:NCT-CZERO TTRUE
   -1 NCT-FIXTURE:NCT-CZERO TFALSE
   4094 NCT-FIXTURE:NCT-CMAX TFALSE
   4095 NCT-FIXTURE:NCT-CMAX TTRUE
   4096 NCT-FIXTURE:NCT-CMAX TFALSE
   4095 NCT-FIXTURE:NCT-COVER TFALSE
   4096 NCT-FIXTURE:NCT-COVER TTRUE
   -1 NCT-FIXTURE:NCT-CNEG TTRUE
   7 NCT-FIXTURE:NCT-CSWAP TFALSE
   8 NCT-FIXTURE:NCT-CSWAP TTRUE
   6 3 NCT-FIXTURE:NCT-CBR 0 T=
   7 3 NCT-FIXTURE:NCT-CBR 2 T=
   8 3 NCT-FIXTURE:NCT-CSHARED 9 T=
   9 3 NCT-FIXTURE:NCT-CSHARED 3 T= ;

: ADDR-CASES ( -- )
   s" an address chain keeps its relocation kind across combine" T-LABEL
   S\" : NCT-ADDR ( -- ptr u8 n ) s\" combine\" ;" EV
   A64EMIT:ADDR-SITES 1 T=
   0 A64EMIT:ADDR-SITE-KIND@ A64IR:ADDR-DATA T=
   0 A64EMIT:ADDR-SITE@ A64IR:HALVES + A64EMIT:INSNS <= TTRUE ;

: CASES ( -- )
   FIRED-CASES
   REFUSED-CASES
   ANSWER-CASES
   IMM-FIRED-CASES
   IMM-REFUSED-CASES
   IMM-ANSWER-CASES
   MASK-FIRED-CASES
   MASK-REFUSED-CASES
   MASK-ANSWER-CASES
   CMP-FIRED-CASES
   CMP-REFUSED-CASES
   CMP-ANSWER-CASES
   ADDR-CASES ;

;using

;package

T-RESET
NCT-TEST:CASES
T-REPORT

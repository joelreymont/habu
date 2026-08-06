\ native-combine.f - the multiply-add the chain now writes, against the two
\ instructions it replaces. One concern: src/compiler/native/combine.f.
\
\ WHAT A COMBINE HAS TO BE HELD TO. The pass deletes an instruction, so the only
\ question that matters is whether the routine still computes what it computed.
\ A byte count cannot answer that and neither can a disassembly: `madd rd, rn,
\ rm, ra` with its four registers permuted is the same LENGTH and the same
\ opcode, and it computes something else. So every case below is DIFFERENTIAL -
\ the same source text compiled twice, once by the engine's own emitter, which
\ has no multiply-add and never fuses anything, and once by the native chain,
\ which does - and the two are run against each other on the same inputs.
\
\ WHY THE ENGINE IS THE REFERENCE. It is the uncombined compilation of the
\ identical text, produced by a generator this pass cannot reach, and it is
\ already what tools/codegen-compare.f adjudicates every corpus row against. A
\ reference built by switching the pass off would be a second configuration of
\ the thing under test; this one is a different compiler.
\
\ AND WHY THE INPUTS GO TO THE ENDS OF THE RANGE. A multiply-add computes the
\ low sixty-four bits of the product and adds the addend, all of it wrapping, and
\ so does a multiply followed by an addition - so the two agree on every input or
\ the pass is wrong, and the inputs most likely to show a disagreement are the
\ ones where the wrapping happens. MIN-INT and MAX-INT are therefore in every
\ case, beside the small pinned values, and they are what would catch an operand
\ order that happens to be right for small positive numbers.
\
\ THE STRUCTURAL ASSERTION IS NOT DECORATION. A differential test between two
\ compilations neither of which fused anything passes and proves nothing, so each
\ case first asserts that the chain's routine really does hold the multiply-add -
\ read off its emitted code through tools/codegen-combine-inventory.f - and the
\ negative cases assert that it holds none. Without those, deleting the body of
\ the pass would leave this suite green.
\
\ THE TWO NEGATIVE CASES ARE THE GUARD. A product read TWICE may not be folded:
\ the multiply still has to run, so folding one reader would add an instruction
\ rather than remove one, and folding it while something else still reads the
\ product would compute the reader's value from a register nothing wrote.
\ NCT-TWICE is that shape and must come out with no multiply-add in it. NCT-SPLIT
\ is the same product read by an addition and by something that is not an
\ addition, which is the same refusal reached the other way.

require lib/test.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/migrate.f
require tools/codegen-combine-inventory.f

package NCT-FIXTURE

public

\ ---- the engine's compilation: the reference -------------------------------
\ Ordinary definitions. bin/hb compiles these with the emitter it has always
\ used, which has no multiply-add in its vocabulary at all.

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

: NCT-IOVER ( n -- n )
   4096 + ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same text, migrated through the production entry, published beside its
\ reference. The register budget is the straight-line one the comparison corpora
\ state.

package NCT-MIGRATED

private

8 constant REGS

: SQSUM ( -- )
   s" : NCT-SQSUM-N ( n n -- n ) dup * swap dup * + ;" 2 1 REGS NMIGRATE:DEFINE ;

: MAD3 ( -- )
   s" : NCT-MAD3-N ( n -- n ) 3 * 5 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: ACC ( -- )
   s" : NCT-ACC-N ( n n n -- n ) {: a:n b:n c:n :} a b * c + ;"
   3 1 REGS NMIGRATE:DEFINE ;

: ACC2 ( -- )
   s" : NCT-ACC2-N ( n n n -- n ) {: a:n b:n c:n :} c a b * + ;"
   3 1 REGS NMIGRATE:DEFINE ;

: TWICE ( -- )
   s" : NCT-TWICE-N ( n n -- n ) {: a:n b:n :} a b * dup + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: SPLIT ( -- )
   s" : NCT-SPLIT-N ( n n -- n ) {: a:n b:n :} a b * dup 7 xor + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: TWO ( -- )
   s" : NCT-TWO-N ( n n n -- n ) {: a:n b:n c:n :} a b * c +  a c * b +  + ;"
   3 1 REGS NMIGRATE:DEFINE ;

\ ---- the folded constant -----------------------------------------------------
\ One small constant added to a value, which is the whole of the pattern.
: IADD ( -- )
   s" : NCT-IADD-N ( n -- n ) 5 + ;" 1 1 REGS NMIGRATE:DEFINE ;

\ The same subtracted, which is the other opcode and not a negated immediate.
: ISUB ( -- )
   s" : NCT-ISUB-N ( n -- n ) 5 - ;" 1 1 REGS NMIGRATE:DEFINE ;

\ THE CONSTANT ON THE WRONG SIDE OF A SUBTRACTION. `5 - x` subtracts the value
\ FROM the constant, and the immediate form subtracts the immediate from the
\ register, so this one must not fold however small the number is.
: IRSUB ( -- )
   s" : NCT-IRSUB-N ( n -- n ) 5 swap - ;" 1 1 REGS NMIGRATE:DEFINE ;

\ A constant with a SECOND READER. The literal memo gives both additions one
\ value, so folding either would delete a move-wide the other still needs.
: ISHARED ( -- )
   s" : NCT-ISHARED-N ( n n -- n ) {: a:n b:n :} a 9 + b 9 + + ;"
   2 1 REGS NMIGRATE:DEFINE ;

\ THE TWO ENDS OF THE FIELD. 4095 is the largest immediate the form carries and
\ folds; 4096 is the first that does not fit and must stay a move-wide. The pair
\ is what says the bound is the field's and not a number somebody liked.
: IMAX ( -- )
   s" : NCT-IMAX-N ( n -- n ) 4095 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: IOVER ( -- )
   s" : NCT-IOVER-N ( n -- n ) 4096 + ;" 1 1 REGS NMIGRATE:DEFINE ;

public

: RUN ( -- )
   SQSUM
   MAD3
   ACC
   ACC2
   TWICE
   SPLIT
   TWO
   IADD
   ISUB
   IRSUB
   ISHARED
   IMAX
   IOVER ;

;package

package NCT-FIXTURE
public

NCT-MIGRATED:RUN

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

\ ---- the differentials -------------------------------------------------------
\ One input, both compilations, one comparison. The reference is computed first
\ so a failure prints the two numbers in the order the label reads.

: SQSUM= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-SQSUM  a b NCT-FIXTURE:NCT-SQSUM-N  T= ;

: MAD3= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MAD3  a NCT-FIXTURE:NCT-MAD3-N  T= ;

: ACC= ( n n n -- ) {: a:n b:n c:n :}
   a b c NCT-FIXTURE:NCT-ACC  a b c NCT-FIXTURE:NCT-ACC-N  T= ;

: ACC2= ( n n n -- ) {: a:n b:n c:n :}
   a b c NCT-FIXTURE:NCT-ACC2  a b c NCT-FIXTURE:NCT-ACC2-N  T= ;

: TWICE= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-TWICE  a b NCT-FIXTURE:NCT-TWICE-N  T= ;

: SPLIT= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-SPLIT  a b NCT-FIXTURE:NCT-SPLIT-N  T= ;

: TWO= ( n n n -- ) {: a:n b:n c:n :}
   a b c NCT-FIXTURE:NCT-TWO  a b c NCT-FIXTURE:NCT-TWO-N  T= ;

: IADD= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-IADD  a NCT-FIXTURE:NCT-IADD-N  T= ;

: ISUB= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-ISUB  a NCT-FIXTURE:NCT-ISUB-N  T= ;

: IRSUB= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-IRSUB  a NCT-FIXTURE:NCT-IRSUB-N  T= ;

: ISHARED= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-ISHARED  a b NCT-FIXTURE:NCT-ISHARED-N  T= ;

: IMAX= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-IMAX  a NCT-FIXTURE:NCT-IMAX-N  T= ;

: IOVER= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-IOVER  a NCT-FIXTURE:NCT-IOVER-N  T= ;

\ How many folded constants a published routine's emitted code holds, and how
\ many move-wides survive beside them.
: ADDIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:ADDI-INSNS ;

: SUBIS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
   NCOMBINV:SUBI-INSNS ;

public

: FIRED-CASES ( -- )
   s" the canonical shape holds one multiply-add and one surviving multiply" T-LABEL
   s" NCT-FIXTURE:NCT-SQSUM-N" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-SQSUM-N" MULS-IN 1 T=

   s" a multiply and an addition of constants become one instruction" T-LABEL
   s" NCT-FIXTURE:NCT-MAD3-N" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-MAD3-N" MULS-IN 0 T=

   s" and so do a multiply and an addition of three arguments" T-LABEL
   s" NCT-FIXTURE:NCT-ACC-N" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-ACC-N" MULS-IN 0 T=

   s" the addend may be either operand of the addition" T-LABEL
   s" NCT-FIXTURE:NCT-ACC2-N" MADDS-IN 1 T=
   s" NCT-FIXTURE:NCT-ACC2-N" MULS-IN 0 T=

   s" two independent pairs in one body become two multiply-adds" T-LABEL
   s" NCT-FIXTURE:NCT-TWO-N" MADDS-IN 2 T=
   s" NCT-FIXTURE:NCT-TWO-N" MULS-IN 0 T= ;

: REFUSED-CASES ( -- )
   s" a product the addition reads TWICE is not folded" T-LABEL
   s" NCT-FIXTURE:NCT-TWICE-N" MADDS-IN 0 T=
   s" NCT-FIXTURE:NCT-TWICE-N" MULS-IN 1 T=

   s" and neither is one read by an addition and by something else" T-LABEL
   s" NCT-FIXTURE:NCT-SPLIT-N" MADDS-IN 0 T=
   s" NCT-FIXTURE:NCT-SPLIT-N" MULS-IN 1 T= ;

: IMM-FIRED-CASES ( -- )
   s" a small constant added to a value becomes the addition's own immediate"
   T-LABEL
   s" NCT-FIXTURE:NCT-IADD-N" ADDIS-IN 1 T=

   s" and subtracted, it becomes the subtraction's - not a negated addition"
   T-LABEL
   s" NCT-FIXTURE:NCT-ISUB-N" SUBIS-IN 1 T=
   s" NCT-FIXTURE:NCT-ISUB-N" ADDIS-IN 0 T=

   s" the largest value the field holds still folds" T-LABEL
   s" NCT-FIXTURE:NCT-IMAX-N" ADDIS-IN 1 T= ;

: IMM-REFUSED-CASES ( -- )
   s" a constant the value is subtracted FROM is not folded" T-LABEL
   s" NCT-FIXTURE:NCT-IRSUB-N" SUBIS-IN 0 T=
   s" NCT-FIXTURE:NCT-IRSUB-N" ADDIS-IN 0 T=

   s" nor is one a second reader still needs" T-LABEL
   s" NCT-FIXTURE:NCT-ISHARED-N" ADDIS-IN 0 T=

   s" nor is the first value too large for the field" T-LABEL
   s" NCT-FIXTURE:NCT-IOVER-N" ADDIS-IN 0 T= ;

: IMM-ANSWER-CASES ( -- )
   s" the folded forms answer what the engine's own code answers" T-LABEL
   0 IADD= 1 IADD= -1 IADD= MAX-INT IADD= MIN-INT IADD=
   0 ISUB= 1 ISUB= -1 ISUB= MAX-INT ISUB= MIN-INT ISUB=
   0 IMAX= -1 IMAX= MAX-INT IMAX= MIN-INT IMAX=

   s" and so do the three the pass refused, which still have to be right" T-LABEL
   0 IRSUB= 7 IRSUB= -1 IRSUB= MAX-INT IRSUB= MIN-INT IRSUB=
   0 IOVER= -1 IOVER= MAX-INT IOVER= MIN-INT IOVER=
   3 5 ISHARED= 0 0 ISHARED= -1 -1 ISHARED=
   MAX-INT MAX-INT ISHARED= MIN-INT MIN-INT ISHARED= ;

: ANSWER-CASES ( -- )
   s" the canonical shape answers what the engine's own code answers" T-LABEL
   3 5 SQSUM=
   0 0 SQSUM=
   -1 -1 SQSUM=
   7 -9 SQSUM=
   MAX-INT 1 SQSUM=
   MIN-INT 1 SQSUM=
   MAX-INT MAX-INT SQSUM=
   MIN-INT MIN-INT SQSUM=
   MIN-INT MAX-INT SQSUM=

   s" and so does the constant multiply-add" T-LABEL
   0 MAD3=
   1 MAD3=
   -1 MAD3=
   MAX-INT MAD3=
   MIN-INT MAD3=

   s" and the three-argument one, in both addend positions" T-LABEL
   3 5 7 ACC=
   3 5 7 ACC2=
   0 0 0 ACC=
   -1 -1 -1 ACC=
   MAX-INT MAX-INT MAX-INT ACC=
   MIN-INT MIN-INT MIN-INT ACC=
   MAX-INT MIN-INT MAX-INT ACC=
   MAX-INT MAX-INT MAX-INT ACC2=
   MIN-INT MIN-INT MIN-INT ACC2=
   MIN-INT MAX-INT MIN-INT ACC2=

   s" and the body holding two of them" T-LABEL
   3 5 7 TWO=
   0 0 0 TWO=
   -1 2 -3 TWO=
   MAX-INT MAX-INT MAX-INT TWO=
   MIN-INT MIN-INT MIN-INT TWO=
   MIN-INT MAX-INT MIN-INT TWO=

   s" and the two the pass refused, which still have to be right" T-LABEL
   3 5 TWICE=
   -1 -1 TWICE=
   MAX-INT MAX-INT TWICE=
   MIN-INT MIN-INT TWICE=
   3 5 SPLIT=
   -1 -1 SPLIT=
   MAX-INT MAX-INT SPLIT=
   MIN-INT MIN-INT SPLIT= ;

: CASES ( -- )
   FIRED-CASES
   REFUSED-CASES
   ANSWER-CASES
   IMM-FIRED-CASES
   IMM-REFUSED-CASES
   IMM-ANSWER-CASES ;

;using

;package

T-RESET
NCT-TEST:CASES
T-REPORT

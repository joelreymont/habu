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
\ already what bin/hb --load tools/judge.f adjudicates every corpus row against. A
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

\ ---- the chain's compilation: the subject ------------------------------------
\ The same text, migrated through the production entry, published beside its
\ reference. The register budget is the straight-line one the comparison corpora
\ state.

package NCT-MIGRATED

private

: SQSUM ( -- )
   s" : NCT-SQSUM-N ( n n -- n ) dup * swap dup * + ;" NMIGRATE:DEFINE ;

: MAD3 ( -- )
   s" : NCT-MAD3-N ( n -- n ) 3 * 5 + ;" NMIGRATE:DEFINE ;

: ACC ( -- )
   s" : NCT-ACC-N ( n n n -- n ) {: a:n b:n c:n :} a b * c + ;"
   NMIGRATE:DEFINE ;

: ACC2 ( -- )
   s" : NCT-ACC2-N ( n n n -- n ) {: a:n b:n c:n :} c a b * + ;"
   NMIGRATE:DEFINE ;

: TWICE ( -- )
   s" : NCT-TWICE-N ( n n -- n ) {: a:n b:n :} a b * dup + ;"
   NMIGRATE:DEFINE ;

: SPLIT ( -- )
   s" : NCT-SPLIT-N ( n n -- n ) {: a:n b:n :} a b * dup 7 xor + ;"
   NMIGRATE:DEFINE ;

: TWO ( -- )
   s" : NCT-TWO-N ( n n n -- n ) {: a:n b:n c:n :} a b * c +  a c * b +  + ;"
   NMIGRATE:DEFINE ;

\ ---- the folded constant -----------------------------------------------------
\ One small constant added to a value, which is the whole of the pattern.
: IADD ( -- )
   s" : NCT-IADD-N ( n -- n ) 5 + ;" NMIGRATE:DEFINE ;

\ The same subtracted, which is the other opcode and not a negated immediate.
: ISUB ( -- )
   s" : NCT-ISUB-N ( n -- n ) 5 - ;" NMIGRATE:DEFINE ;

\ THE CONSTANT ON THE WRONG SIDE OF A SUBTRACTION. `5 - x` subtracts the value
\ FROM the constant, and the immediate form subtracts the immediate from the
\ register, so this one must not fold however small the number is.
: IRSUB ( -- )
   s" : NCT-IRSUB-N ( n -- n ) 5 swap - ;" NMIGRATE:DEFINE ;

\ A constant with a SECOND READER. The literal memo gives both additions one
\ value, so folding either would delete a move-wide the other still needs.
: ISHARED ( -- )
   s" : NCT-ISHARED-N ( n n -- n ) {: a:n b:n :} a 9 + b 9 + + ;"
   NMIGRATE:DEFINE ;

\ THE TWO ENDS OF THE FIELD. 4095 is the largest immediate the form carries and
\ folds; 4096 is the first that does not fit and must stay a move-wide. The pair
\ is what says the bound is the field's and not a number somebody liked.
: IMAX ( -- )
   s" : NCT-IMAX-N ( n -- n ) 4095 + ;" NMIGRATE:DEFINE ;

: IOVER ( -- )
   s" : NCT-IOVER-N ( n -- n ) 4096 + ;" NMIGRATE:DEFINE ;

\ ---- the folded comparison ---------------------------------------------------
\ A comparison against a small constant, whose flag the routine ANSWERS - so the
\ machine form is the flag-materialising one and what folds is its immediate.
: CEQ ( -- )
   s" : NCT-CEQ-N ( n -- bool ) 7 = ;" NMIGRATE:DEFINE ;

\ THE ROW THE SURVEY IS ABOUT. `0=` is a constant zero and an equality, so it is
\ this fold's largest single consumer, and zero is a value the field holds.
: CZERO ( -- )
   s" : NCT-CZERO-N ( n -- bool ) 0= ;" NMIGRATE:DEFINE ;

\ THE THREE ENDS OF THE FIELD. 4095 is the largest value the compare immediate
\ carries and folds; 4096 is the first that does not fit; -1 is the other side
\ of the bound entirely - the field is unsigned, and `cmp rn, #-1` is a `cmn`
\ this dialect does not carry. All three are comparisons against small-LOOKING
\ numbers, which is what makes the trio the check that the bound is the
\ encoder's field and not somebody's idea of small.
: CMAX ( -- )
   s" : NCT-CMAX-N ( n -- bool ) 4095 = ;" NMIGRATE:DEFINE ;

: COVER ( -- )
   s" : NCT-COVER-N ( n -- bool ) 4096 = ;" NMIGRATE:DEFINE ;

: CNEG ( -- )
   s" : NCT-CNEG-N ( n -- bool ) -1 = ;" NMIGRATE:DEFINE ;

\ THE CONSTANT ON THE LEFT. `7 x <` compares seven against the value, and the
\ instruction subtracts the immediate FROM the register - so folding this one
\ would compute the mirrored relation. It must keep the register form however
\ small the number is, and it is the case a fold written as "either operand"
\ would get wrong while every corpus row still passed.
: CSWAP ( -- )
   s" : NCT-CSWAP-N ( n -- bool ) 7 swap < ;" NMIGRATE:DEFINE ;

\ The same fold where the comparison is FUSED INTO A BRANCH rather than
\ answered, which is the second of the two machine forms and the one that
\ carries successors across.
\
\ THE DIVISION IN THE SECOND ARM IS WHAT KEEPS THE BRANCH THERE, and it is a
\ schema property rather than a size. A small two-armed body is if-converted
\ into a conditional select, which is a third machine form and not one this
\ fold claims, so a fixture written as `dup 7 < if drop 0 exit then 1+` measures
\ nothing about the fused branch at all - it never becomes one. A division may
\ trap, and src/compiler/native/select.f refuses to run anything that may trap
\ on a path the program would not have taken, so this body keeps its branch for
\ the same stated reason test/compiler/native-select.f's own branch fixture
\ does. The divisor is never zero on any path that reaches the division.
: CBR ( -- )
   s" : NCT-CBR-N ( n n -- n ) {: a:n b:n :} a 7 < if 0 exit then a b / ;"
   NMIGRATE:DEFINE ;

\ A constant the comparison shares with a SECOND reader - here the value the
\ word answers on the arm the comparison chose. Folding it would delete a
\ move-wide that arm still needs. It is CBR's body with one character changed,
\ so the ONLY difference between the row that folds and the row that does not
\ is the second reader: a fixture whose comparison could not have folded anyway
\ would say nothing about the use count.
: CSHARED ( -- )
   s" : NCT-CSHARED-N ( n n -- n ) {: a:n b:n :} a 9 < if 9 exit then a b / ;"
   NMIGRATE:DEFINE ;

\ ---- the folded mask ---------------------------------------------------------
\ One small mask over a value, in each of the three bitwise forms. 7 is a run of
\ three ones, which the logical field describes, so all three fold.
: MAND ( -- )
   s" : NCT-MAND-N ( n -- n ) 7 and ;" NMIGRATE:DEFINE ;

: MORR ( -- )
   s" : NCT-MORR-N ( n -- n ) 7 or ;" NMIGRATE:DEFINE ;

: MEOR ( -- )
   s" : NCT-MEOR-N ( n -- n ) 7 xor ;" NMIGRATE:DEFINE ;

\ THE MASK ON THE OTHER SIDE. All three forms are commutative, so unlike the
\ subtraction this one folds too - and it is the case that says the fold reads
\ both operands rather than only the second.
: MSWAP ( -- )
   s" : NCT-MSWAP-N ( n -- n ) 7 swap and ;" NMIGRATE:DEFINE ;

\ THE MASK THE FIELD CANNOT DESCRIBE. 5 is 0b101 - three bits wide with a hole
\ in the middle - so it is not a rotated contiguous run and the thirteen-bit
\ description cannot be built for it. It must stay a move-wide and a register
\ `and`, and it is the row that catches a fold written as a range check: 5 is
\ SMALLER than 7, so any bound on magnitude would have admitted it.
: MHOLE ( -- )
   s" : NCT-MHOLE-N ( n -- n ) 5 and ;" NMIGRATE:DEFINE ;

\ And the two masks with no encoding at all, at the ends the packer refuses:
\ a mask of no ones and a mask of nothing but ones.
: MZERO ( -- )
   s" : NCT-MZERO-N ( n -- n ) 0 and ;" NMIGRATE:DEFINE ;

: MALL ( -- )
   s" : NCT-MALL-N ( n -- n ) -1 and ;" NMIGRATE:DEFINE ;

\ A mask with a SECOND READER, which must not fold for the reason a shared
\ constant must not: the move-wide the fold would delete is still needed.
: MSHARED ( -- )
   s" : NCT-MSHARED-N ( n n -- n ) {: a:n b:n :} a 7 and b 7 and + ;"
   NMIGRATE:DEFINE ;

public

\ Migrated ON DEMAND rather than from RUN below, because the case that asks about
\ it reads the SEALED EMISSION, which holds the last routine this process
\ compiled. A migration run at load time would have been overwritten by every
\ migration after it.
: ADDR-SURVIVES ( -- )
   S\" : NCT-ADDR-N ( -- ptr u8 n ) s\" combine\" ;" NMIGRATE:DEFINE ;

: RUN ( -- )
   MAND MORR MEOR MSWAP MHOLE MZERO MALL MSHARED
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
   IOVER
   CEQ
   CZERO
   CMAX
   COVER
   CNEG
   CSWAP
   CBR
   CSHARED ;

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

: MAND= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MAND  a NCT-FIXTURE:NCT-MAND-N  T= ;

: MORR= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MORR  a NCT-FIXTURE:NCT-MORR-N  T= ;

: MEOR= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MEOR  a NCT-FIXTURE:NCT-MEOR-N  T= ;

: MSWAP= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MSWAP  a NCT-FIXTURE:NCT-MSWAP-N  T= ;

: MHOLE= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MHOLE  a NCT-FIXTURE:NCT-MHOLE-N  T= ;

: MZERO= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MZERO  a NCT-FIXTURE:NCT-MZERO-N  T= ;

: MALL= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-MALL  a NCT-FIXTURE:NCT-MALL-N  T= ;

: MSHARED= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-MSHARED  a b NCT-FIXTURE:NCT-MSHARED-N  T= ;

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

: CMP-FIRED-CASES ( -- )
   s" a comparison against a small constant carries it in the instruction"
   T-LABEL
   s" NCT-FIXTURE:NCT-CEQ-N" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CEQ-N" CMPS-IN 0 T=

   s" and so does `0=`, which is a constant zero and an equality" T-LABEL
   s" NCT-FIXTURE:NCT-CZERO-N" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CZERO-N" CMPS-IN 0 T=

   s" the largest value the field holds still folds" T-LABEL
   s" NCT-FIXTURE:NCT-CMAX-N" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CMAX-N" CMPS-IN 0 T=

   s" and a comparison FUSED into a branch folds the same way" T-LABEL
   s" NCT-FIXTURE:NCT-CBR-N" CMPIS-IN 1 T=
   s" NCT-FIXTURE:NCT-CBR-N" CMPS-IN 0 T= ;

: CMP-REFUSED-CASES ( -- )
   s" a value one past the field is not folded" T-LABEL
   s" NCT-FIXTURE:NCT-COVER-N" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-COVER-N" CMPS-IN 1 T=

   s" nor is a negative one, which is a `cmn` this dialect does not carry"
   T-LABEL
   s" NCT-FIXTURE:NCT-CNEG-N" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-CNEG-N" CMPS-IN 1 T=

   s" nor is a constant on the LEFT, which is the mirrored relation" T-LABEL
   s" NCT-FIXTURE:NCT-CSWAP-N" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-CSWAP-N" CMPS-IN 1 T=

   s" nor is one a second reader still needs" T-LABEL
   s" NCT-FIXTURE:NCT-CSHARED-N" CMPIS-IN 0 T=
   s" NCT-FIXTURE:NCT-CSHARED-N" CMPS-IN 1 T= ;

\ A Habu flag as the number a measured row records for one, so that two flags
\ can be compared with the same assertion every other row uses. It is
\ tools/judge/cost.f's FLAG-BITS written here rather than reached for, because
\ this suite loads none of the judge.
: FLAG>N ( bool -- n )
   if 1 else 0 then ;

: CEQ= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-CEQ FLAG>N  a NCT-FIXTURE:NCT-CEQ-N FLAG>N  T= ;

: CZERO= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-CZERO FLAG>N  a NCT-FIXTURE:NCT-CZERO-N FLAG>N  T= ;

: CMAX= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-CMAX FLAG>N  a NCT-FIXTURE:NCT-CMAX-N FLAG>N  T= ;

: COVER= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-COVER FLAG>N  a NCT-FIXTURE:NCT-COVER-N FLAG>N  T= ;

: CNEG= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-CNEG FLAG>N  a NCT-FIXTURE:NCT-CNEG-N FLAG>N  T= ;

: CSWAP= ( n -- ) {: a:n :}
   a NCT-FIXTURE:NCT-CSWAP FLAG>N  a NCT-FIXTURE:NCT-CSWAP-N FLAG>N  T= ;

: CBR= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-CBR  a b NCT-FIXTURE:NCT-CBR-N  T= ;

: CSHARED= ( n n -- ) {: a:n b:n :}
   a b NCT-FIXTURE:NCT-CSHARED  a b NCT-FIXTURE:NCT-CSHARED-N  T= ;

\ The inputs straddle every boundary the fold reads: the compared constant
\ itself, the values either side of it, the ends of the range where a signed
\ comparison and an unsigned field disagree if anything confused them, and the
\ two values that make the refused rows answer differently from the folded ones.
: CMP-ANSWER-CASES ( -- )
   s" the folded comparisons answer what the engine's own code answers" T-LABEL
   0 CEQ= 6 CEQ= 7 CEQ= 8 CEQ= -7 CEQ= MAX-INT CEQ= MIN-INT CEQ=
   0 CZERO= 1 CZERO= -1 CZERO= MAX-INT CZERO= MIN-INT CZERO=
   4094 CMAX= 4095 CMAX= 4096 CMAX= -4095 CMAX= MAX-INT CMAX= MIN-INT CMAX=
   6 0 CBR= 7 3 CBR= 8 3 CBR= 0 0 CBR= -1 0 CBR=
   MAX-INT 3 CBR= MAX-INT 1 CBR= MIN-INT 0 CBR=

   s" and so do the four the pass refused, which still have to be right" T-LABEL
   4095 COVER= 4096 COVER= 4097 COVER= 0 COVER= -4096 COVER=
   MAX-INT COVER= MIN-INT COVER=
   0 CNEG= -1 CNEG= 1 CNEG= 4095 CNEG= MAX-INT CNEG= MIN-INT CNEG=
   6 CSWAP= 7 CSWAP= 8 CSWAP= 0 CSWAP= -1 CSWAP= MAX-INT CSWAP= MIN-INT CSWAP=
   8 0 CSHARED= 9 3 CSHARED= 10 3 CSHARED= 0 0 CSHARED= -1 0 CSHARED=
   MAX-INT 3 CSHARED= MAX-INT 1 CSHARED= MIN-INT 0 CSHARED= ;

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

\ The mask fold: which masks reach the instruction, which stay in a register,
\ and - the row that matters most - that a mask with no encoding is DECLINED
\ rather than handed to a packer that would end the process on it.
: MASK-FIRED-CASES ( -- )
   s" a small mask over a value becomes the operation's own immediate" T-LABEL
   s" NCT-FIXTURE:NCT-MAND-N" ANDIS-IN 1 T=
   s" NCT-FIXTURE:NCT-MORR-N" ORRIS-IN 1 T=
   s" NCT-FIXTURE:NCT-MEOR-N" EORIS-IN 1 T=

   s" and it folds from either operand, because all three are commutative" T-LABEL
   s" NCT-FIXTURE:NCT-MSWAP-N" ANDIS-IN 1 T= ;

: MASK-REFUSED-CASES ( -- )
   s" a mask the field cannot describe is not folded, though it is smaller" T-LABEL
   s" NCT-FIXTURE:NCT-MHOLE-N" ANDIS-IN 0 T=

   s" nor is a mask of no ones or a mask of nothing but ones" T-LABEL
   s" NCT-FIXTURE:NCT-MZERO-N" ANDIS-IN 0 T=
   s" NCT-FIXTURE:NCT-MALL-N" ANDIS-IN 0 T=

   s" nor is one a second reader still needs" T-LABEL
   s" NCT-FIXTURE:NCT-MSHARED-N" ANDIS-IN 0 T= ;

\ Every one of them has to answer what the engine's own code answers, the
\ refused ones included - a declined fold still has to compile to right code.
: MASK-ANSWER-CASES ( -- )
   s" the folded masks answer what the engine's own code answers" T-LABEL
   0 MAND= 1 MAND= -1 MAND= MAX-INT MAND= MIN-INT MAND=
   0 MORR= 1 MORR= -1 MORR= MAX-INT MORR= MIN-INT MORR=
   0 MEOR= 1 MEOR= -1 MEOR= MAX-INT MEOR= MIN-INT MEOR=
   0 MSWAP= 9 MSWAP= -1 MSWAP= MAX-INT MSWAP= MIN-INT MSWAP=

   s" and so do the four the pass refused, which still have to be right" T-LABEL
   0 MHOLE= 9 MHOLE= -1 MHOLE= MAX-INT MHOLE= MIN-INT MHOLE=
   0 MZERO= -1 MZERO= MAX-INT MZERO= MIN-INT MZERO=
   0 MALL= -1 MALL= MAX-INT MALL= MIN-INT MALL=
   3 5 MSHARED= 0 0 MSHARED= -1 -1 MSHARED=
   MAX-INT MAX-INT MSHARED= MIN-INT MIN-INT MSHARED= ;


\ ---- an address chain SURVIVES the rewrite, kind and all ----------------------
\ WHY THIS SUITE OWNS THE QUESTION. combine.f rewrites a module between selection
\ and emission: it copies every operation it does not fold into a fresh module,
\ attribute by attribute, through an explicit list of keys it knows. A key that
\ is not on that list is DROPPED - and the relocation kind an address chain
\ carries is exactly the sort of field that would go missing in silence, leaving
\ a chain that still computes the right address and no longer tells anyone it is
\ one. The emission would publish, the word would run, and the AOT capture would
\ not know the site existed.
\
\ WHAT MAKES THE ANSWER TRUSTWORTHY IS THAT LOSING IT IS LOUD, and this case is
\ the standing proof of that. `a64.addr` is a REQUIRED attribute of the move-wide
\ forms, so there are two independent refusals under a rewrite that lost it:
\ combine.f's own KEY-SLOT-OF throws E-A64COMB-OPCODE on an attribute key it was
\ never taught, and IR-OP's freeze verifier throws E-IR-VERIFY-ATTRKEY on a
\ required key an operation omits. Deleting the K-ADDR arm from COPY-ATTRS reds
\ this case; so does deleting the key's row from the bound table.
\
\ AND IT ASKS THE QUESTION THROUGH THE WHOLE CHAIN. A string literal is the
\ shortest source form that makes an address chain: the body pushes the address
\ of the interned bytes and their length. The word is migrated through the
\ production entry, so combine.f runs over the real module, and the answer is
\ read off the SEALED EMISSION - four consecutive move-wide words into one
\ register, recorded as one site - rather than off the module combine.f produced.
\ A site that the emitter would not record is a site the publication cannot
\ write, whatever the module says.
: ADDR-CASES ( -- )
   s" an address chain keeps its kind across the rewrite, and is recorded" T-LABEL
   NCT-MIGRATED:ADDR-SURVIVES
   A64EMIT:ADDR-SITES 1 T=
   0 A64EMIT:ADDR-SITE-KIND@ A64IR:ADDR-DATA T=

   s" and it is the four-lane carrier, not the shortest chain" T-LABEL
   0 A64EMIT:ADDR-SITE@ {: k:n :}
   k A64IR:HALVES + A64EMIT:INSNS <= TTRUE ;

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

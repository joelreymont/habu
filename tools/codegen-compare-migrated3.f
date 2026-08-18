\ codegen-compare-migrated3.f - the third corpus's straight-line float bodies,
\ compiled by the new chain and published as ordinary words. One concern: putting
\ the third corpus's new-chain words into the dictionary before anything measures
\ them.
\
\ This is tools/codegen-compare-migrated.f's discipline over the third corpus,
\ and everything that file says applies here: the source is handed to the engine,
\ the engine compiles it through the path it compiles every definition through,
\ the tape the chain then recompiles is the one the checker's own reader filled
\ while it certified that body, and the publication seam points the `-N` word's
\ dictionary record at the chain's emission. A body that is not well typed Habu
\ does not compile at all, and nothing here catches: a body the chain refuses is
\ a claim this file made and did not keep.
\
\ ALL TEN, AND THE THREE CALLEES THE KERNELS REACH MEMORY THROUGH. The scalar
\ float leaf compiled straight-line float arithmetic over a locals frame, the two
\ conversions and float literals; the comparison leaf added the five float
\ comparisons and the branch a comparison feeds; and this leaf added a double
\ placed where a straight line does not reach - across a block edge, across a
\ call, and round a loop's back edge. That is every shape the corpus has, so
\ tools/codegen-compare-new3.f declares no gap at all. Nothing is respelled to
\ buy a row.
\
\ THE SUBSTITUTIONS, AND THERE ARE ONLY THE TWO KINDS. Every body carries `-N` on
\ the name it defines, which is the migration's own convention; and the five
\ kernel rows call `T-GET-N` and `T-SET-N` where the corpus writes `T-GET` and
\ `T-SET`, and T-REL-L2-N calls `T-DIST2-N` and `T-NORM2-N` where the corpus
\ writes `T-DIST2` and `T-NORM2`. That is the discipline
\ tools/codegen-compare-migrated2.f established when it migrated CELL-FIELD for
\ VEC-COPY-CELLS to call: the new column's word is the new chain's code all the
\ way down, so a row measures a program of the chain's making rather than a
\ chain-compiled shell around the engine's loops. No constant is respelled, no
\ operation is changed, no local is renamed and no annotation is added or removed.
\
\ WHY THE FLOAT LITERALS NEED NO SUBSTITUTION, WHICH IS THE POINT OF THE LEAF.
\ Both bodies carry a float literal - SGD's arguments are doubles the caller
\ pushes but SEG-1/SQRT writes `1.0` - and the tape records a real literal's own
\ cell, as the engine's own number reader read it (the `num-parse` primitive).
\ So a literal here is the same double the interpreted word pushes, to the bit,
\ and the head-to-head check below is what says so rather than this sentence.
\
\ THE REGISTER BUDGET. Both routines hold their locals and their intermediate
\ doubles at once, and the doubles are in the OTHER register file - the routine
\ contract hands out the whole of it, for the reason src/compiler/native/abi.f
\ gives - so what the budget below constrains is the general registers the frame
\ pointers and the crossings need. It is a budget, exactly as the other two
\ corpora's are: dot habu-choose-the-register-a95390ac carries taking the number
\ off the routine.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus3.f

package CODEGEN-MIGRATED3

private

\ maki/optim.f:12 through tools/codegen-compare-corpus3.f, verbatim: w' = w - lr*g.
: SGD ( -- )
   s" : SGD-N ( r r r -- r ) {: w g lr :} w  lr g f* f- ;"
   3 1 NMIGRATE:DEFINE ;

\ maki/segment.f:61 through the same corpus, verbatim: an integer length becomes
\ a double, and 1/sqrt(d) comes back.
: SEG ( -- )
   s" : SEG-1/SQRT-N ( n -- r ) {: d:n :}  1.0 d s>f fsqrt f/ ;"
   1 1 NMIGRATE:DEFINE ;

\ maki/autograd.f:48 through the same corpus, verbatim: the two-armed branch on
\ a two-operand float compare. It is the first row of this corpus with control
\ flow in it, and what it measures is the fused float compare-and-branch: the
\ comparison's answer is read by the `if` above it and by nothing else, so the
\ chain emits one Fcmp and one conditional branch where the engine emits an
\ Fcmp, a Cset, a negation and a test-and-branch.
\
\ ITS PINNED INPUTS ARE WHY IT IS WORTH A ROW RATHER THAN A TEST. Both argument
\ orders, both zeros in both orders, and a NaN in each position: the two orders
\ catch a branch taken the wrong way, the two zeros catch a comparison of bits
\ rather than of numbers, and the two NaN positions catch a condition that is
\ TRUE on unordered - which is what a float less-than lowered under `lt` instead
\ of `mi` would be. The recorded output is the whole cell, so a negative zero
\ that came back positive is a reported disagreement rather than an equal number.
: MAXF ( -- )
   s" : MAX-F-N ( r r -- r ) {: x:r y:r :}  x y f< if y else x then ;"
   2 1 NMIGRATE:DEFINE ;

\ ---- the three words a tensor body reaches an element through -----------------
\ maki/array.f:9-11 through the corpus, verbatim but for the `-N`. They are not
\ rows and they are not measured: they are the callees the five kernel rows below
\ reach memory through, migrated so that the new column's kernels are the new
\ chain's code all the way down - the discipline tools/codegen-compare-migrated2.f
\ established when it migrated CELL-FIELD for VEC-COPY-CELLS to call.
\
\ AND THIS IS WHERE THE FLOAT CORPUS REALLY TOUCHES MEMORY. `T-GET` reads a
\ double with `@` and `T-SET` writes one with `!` - the ORDINARY cell load and
\ the ordinary cell store, not float forms, because a double on a Habu stack IS
\ one cell of its own bit pattern. So the eight bytes move through a general
\ register by design, and the kernels that call these words hand a double over as
\ a cell and read one back as a cell. Nothing in this corpus stores a double from
\ a floating register, which is why the crossing at `hir.store` is still dot
\ habu-store-a-double-a31b313e's and no row here waits for it.
: T-AT ( -- )
   s" : T-AT-N ( ptr a n -- ptr a ) cells + ;" 2 1 NMIGRATE:DEFINE ;

: T-GET ( -- )
   s" : T-GET-N ( ptr a n -- r ) T-AT-N @ ;" 2 1 NMIGRATE:DEFINE ;

: T-SET ( -- )
   s" : T-SET-N ( r ptr a n -- ) T-AT-N ! ;" 3 0 NMIGRATE:DEFINE ;

\ ---- the five kernel rows ----------------------------------------------------
\ WHAT THEY NEEDED THAT THE THREE ABOVE DID NOT. Every one of them carries a
\ double round a loop's back edge or across a call, which is where a position's
\ type has to be stated before the value that reaches it is known. The
\ accumulator enters the header as a double and comes back to it as one, so the
\ header's argument is a floating register; and it is live across the load call
\ every turn, so it goes out through a data-stack slot as the cell it is and
\ comes back a cell the next Fadd crosses again. Both are the same crossing, and
\ neither computes anything.
\
\ THE BUDGETS. These loops hold more at once than any row of the first two
\ corpora: the accumulator, the loop's index and limit, two or three locals, and
\ the call site's own scratch, all live from the header to the latch. The
\ floating side costs none of it - the routine contract hands out the whole
\ floating file - so what the number below constrains is the general registers,
\ exactly as the other two corpora's budgets do. Dot
\ habu-choose-the-register-a95390ac carries taking it off the routine.

\ maki/array.f:16 through the corpus: the plain accumulation.
: T-SUM ( -- )
   s" : T-SUM-N ( ptr a n -- r ) {: base len :} 0.0  len 0 ?do  base i T-GET-N f+  loop ;"
   2 1 NMIGRATE:DEFINE ;

\ maki/array.f:20 through the corpus: the in-place step, whose point is the
\ stores. Both columns step the SAME weight buffer, so the head-to-head check is
\ about the loads and the stores and not only about the arithmetic between them.
: T-SGD ( -- )
   s" : T-SGD!-N ( r ptr a ptr a n -- ) {: lr wbase gbase len :} len 0 ?do wbase i T-GET-N lr gbase i T-GET-N f* f- wbase i T-SET-N loop ;"
   4 0 NMIGRATE:DEFINE ;

\ maki/array.f:32 through the corpus: the two-pointer accumulation.
: T-DIST2 ( -- )
   s" : T-DIST2-N ( ptr a ptr a n -- r ) {: abase:ptr bbase:ptr len:n :} 0.0 len 0 ?do abase i T-GET-N bbase i T-GET-N f- dup f* f+ loop ;"
   3 1 NMIGRATE:DEFINE ;

\ maki/array.f:39 through the corpus: the one-pointer square accumulation.
: T-NORM2 ( -- )
   s" : T-NORM2-N ( ptr a n -- r ) {: bbase:ptr len:n :} 0.0 len 0 ?do bbase i T-GET-N dup f* f+ loop ;"
   2 1 NMIGRATE:DEFINE ;

\ maki/array.f:46 through the corpus: two accumulations, two square roots and a
\ division, reached through two calls to two DIFFERENT words. It is the row that
\ needed the migration to carry a list of callees rather than one.
: T-REL-L2 ( -- )
   s" : T-REL-L2-N ( ptr a ptr a n -- r ) {: abase:ptr bbase:ptr len:n :} abase bbase len T-DIST2-N fsqrt bbase len T-NORM2-N fsqrt f/ ;"
   3 1 NMIGRATE:DEFINE ;

\ ---- the two branch rows whose arms disagree about the class -----------------
\ maki/autograd.f:23 through the corpus. Its two arms hand the join a DOUBLE and
\ a CELL - `0.0` is a float literal and `x` is the cell the argument arrived in -
\ which is exactly the case the join-type rule exists for: the first arm states
\ the position's type and the second crosses to it. Its pinned inputs include a
\ NaN, because `f0<` is false on one and the ELSE arm is therefore the one a NaN
\ takes.
: RELU-F ( -- )
   s" : RELU-F-N ( r -- r ) {: x :}   x f0< if 0.0 else x  then ;"
   1 1 NMIGRATE:DEFINE ;

\ lib/fmath.f:36 through the corpus: round to nearest, written the way a
\ truncating `f>s` makes necessary. Both of its arms leave a COMPUTED double, so
\ what crosses its join is two doubles rather than a double and a cell - the
\ other half of the same rule.
: FROUND ( -- )
   s" : FROUND-N ( r -- n )  dup f0< if 0.5 f- else 0.5 f+ then f>s ;"
   1 1 NMIGRATE:DEFINE ;

public

\ Publish all ten, and the three callees the kernels reach memory through first,
\ because a call site is given the address its callee's record already carries.
\ It is one word rather than thirteen top-level lines because a migration claims
\ code space at the engine's free slot, and the interpreter uses that slot for
\ the line it is running.
: RUN ( -- )
   SGD
   SEG
   MAXF
   RELU-F
   FROUND
   T-AT
   T-GET
   T-SET
   T-SUM
   T-SGD
   T-DIST2
   T-NORM2
   T-REL-L2 ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS3 publics, beside the words they are compared against.
package CODEGEN-CORPUS3
public

CODEGEN-MIGRATED3:RUN

;package

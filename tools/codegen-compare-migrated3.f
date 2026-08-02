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
\ THREE OF THE TEN, AND THE OTHER SEVEN SAY WHY THEY ARE NOT HERE. The scalar
\ float leaf compiles straight-line float arithmetic over a locals frame, the two
\ conversions, and float literals; the comparison leaf added the five float
\ comparisons and the branch a comparison feeds, which is what MAX-F is. What the
\ chain still cannot do is place a double anywhere but a straight line - across a
\ block edge, across a call, or into a memory cell - so the three accumulations,
\ the step, the two-call row, RELU-F and FROUND stay gap rows in
\ tools/codegen-compare-new3.f and name exactly what each of them is still
\ waiting for. Nothing is respelled to buy a row.
\
\ WHY MAX-F IS HERE AND RELU-F IS NOT, when both are one float compare feeding
\ one branch. MAX-F's two arms hand over the values `x` and `y`, which arrive in
\ data-stack cells and are still cells when they cross; RELU-F's arms hand over
\ `0.0` and `x`, and `0.0` is a DOUBLE. A double may not cross a block edge yet
\ (src/compiler/native/elaborate.f refuses it by name with E-NELAB-TYPE, measured
\ at the head of tools/codegen-compare-new3.f), so RELU-F is refused one stage
\ after its comparison compiled. The two rows differ in what crosses the join and
\ in nothing else, which is why the gap row says so.
\
\ THE THREE BODIES ARE THE CORPUS'S OWN, TO THE BYTE. SGD is
\ tools/codegen-compare-corpus3.f's SGD with `SGD-N` in place of `SGD`,
\ SEG-1/SQRT is that file's SEG-1/SQRT with `SEG-1/SQRT-N` in place of
\ `SEG-1/SQRT`, and MAX-F is its MAX-F with `MAX-F-N` in place of `MAX-F`. No
\ constant is respelled, no operation is changed, no local is renamed and no
\ annotation is added or removed - the second corpus needed two substitutions and
\ its own header lists them; this one needs none, which is the shortest thing
\ that can be said about a migration and the best.
\
\ WHY THE FLOAT LITERALS NEED NO SUBSTITUTION, WHICH IS THE POINT OF THE LEAF.
\ Both bodies carry a float literal - SGD's arguments are doubles the caller
\ pushes but SEG-1/SQRT writes `1.0` - and the tape records a real literal's own
\ cell, read back along the engine's own route (src/compiler/native/real-lit.f).
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
require tools/codegen-compare-corpus3.f

package CODEGEN-MIGRATED3

private

8 constant REGS                   \ general registers a straight-line float routine may use

\ maki/optim.f:12 through tools/codegen-compare-corpus3.f, verbatim: w' = w - lr*g.
: SGD ( -- )
   s" : SGD-N ( r r r -- r ) {: w g lr :} w  lr g f* f- ;"
   3 1 REGS NMIGRATE:DEFINE ;

\ maki/segment.f:61 through the same corpus, verbatim: an integer length becomes
\ a double, and 1/sqrt(d) comes back.
: SEG ( -- )
   s" : SEG-1/SQRT-N ( n -- r ) {: d:n :}  1.0 d s>f fsqrt f/ ;"
   1 1 REGS NMIGRATE:DEFINE ;

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
   2 1 REGS NMIGRATE:DEFINE ;

public

\ Publish all three. It is one word rather than three top-level lines because a
\ migration claims code space at the engine's free slot, and the interpreter uses
\ that slot for the line it is running.
: RUN ( -- )
   SGD
   SEG
   MAXF ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS3 publics, beside the words they are compared against.
package CODEGEN-CORPUS3
public

CODEGEN-MIGRATED3:RUN

;package

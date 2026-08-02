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
\ TWO OF THE TEN, AND THE OTHER EIGHT SAY WHY THEY ARE NOT HERE. The scalar float
\ leaf compiles straight-line float arithmetic over a locals frame, the two
\ conversions, and float literals. It does not compile a loop, a branch, a call,
\ or a memory access, so the three accumulations, the step, the two branch rows,
\ the two-call row and FROUND stay gap rows in tools/codegen-compare-new3.f and
\ name exactly what each of them is still waiting for. Nothing is respelled to
\ buy a row.
\
\ THE TWO BODIES ARE THE CORPUS'S OWN, TO THE BYTE. SGD is
\ tools/codegen-compare-corpus3.f's SGD with `SGD-N` in place of `SGD`, and
\ SEG-1/SQRT is that file's SEG-1/SQRT with `SEG-1/SQRT-N` in place of
\ `SEG-1/SQRT`. No constant is respelled, no operation is changed, no local is
\ renamed and no annotation is added or removed - the second corpus needed two
\ substitutions and its own header lists them; this one needs none, which is the
\ shortest thing that can be said about a migration and the best.
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

public

\ Publish both. It is one word rather than two top-level lines because a
\ migration claims code space at the engine's free slot, and the interpreter uses
\ that slot for the line it is running.
: RUN ( -- )
   SGD
   SEG ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS3 publics, beside the words they are compared against.
package CODEGEN-CORPUS3
public

CODEGEN-MIGRATED3:RUN

;package

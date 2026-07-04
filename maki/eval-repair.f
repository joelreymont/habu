\ maki/eval-repair.f - the eval matrix's repair-rounds and tokens-to-green columns
\ for the Habu-PTX kernel-authoring loop.
\
\ The authoring loop is author -> checker-feedback -> repair -> green, with the
\ checker (maki/eval.f CHECK-PASSES?) as the in-environment judge. Each trajectory
\ below is a real authoring path: a buggy first draft, then one repair per checker
\ rejection until the kernel certifies. ER-STEP feeds each candidate, counting a
\ repair round per rejection and summing tokens until green. repair-rounds = the
\ number of checker-guided fixes; tokens-to-green = total kernel-source tokens
\ authored until certification. (This is the Habu-PTX side; the Triton comparison
\ is the external arm.)

require lib/ptx/test-prelude.f
require maki/eval.f

package MAKI

variable ER-NTOK  variable ER-ST
\ count whitespace-separated tokens in a kernel source string
: COUNT-TOKS ( ptr u8 n -- n ) {: a u :}
   0 ER-NTOK !  0 ER-ST !
   begin  a u $20 ER-ST @ SPLIT-NEXT  while      ( tokptr toklen nextstart )
      ER-ST !                                    ( tokptr toklen )
      dup 0 > if  ER-NTOK @ 1+ ER-NTOK !  then  2drop
   repeat
   2drop drop  ER-NTOK @ ;

variable ER-ROUND  variable ER-TOKENS  variable ER-GREEN
: ER-RESET ( -- )  0 ER-ROUND !  0 ER-TOKENS !  0 ER-GREEN ! ;
\ one authoring step: count its tokens, check it; reject -> +1 repair round,
\ certify -> green. Steps after green are ignored (the loop has converged).
: ER-STEP ( ptr u8 n -- )
   ER-GREEN @ if 2drop exit then
   2dup COUNT-TOKS  ER-TOKENS @ +  ER-TOKENS !
   CHECK-PASSES? if  -1 ER-GREEN !  else  ER-ROUND @ 1+ ER-ROUND !  then ;
: ER-ROUNDS@ ( -- n )  ER-ROUND @ ;
: ER-TOKENS@ ( -- n )  ER-TOKENS @ ;
: ER-GREEN? ( -- bool )  ER-GREEN @ ;          \ -1 (green) / 0 (not), already canonical

T-RESET

\ --- trajectory A: SAXPY, draft has a type error AND a missing store ---
ER-RESET
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +."           ER-STEP  \ draft: y SCALE (bad type) + no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."           ER-STEP  \ repair 1: a SCALE; still no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  ER-STEP  \ repair 2: add STORE -> GREEN
ER-GREEN?  TTRUE
ER-ROUNDS@  2 T=                                  \ two checker-guided repairs
s" SAXPY authoring: repair-rounds=" type ER-ROUNDS@ . s"  tokens-to-green=" type ER-TOKENS@ . cr

\ --- trajectory B: a one-fix path (single missing-store error) ---
ER-RESET
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."           ER-STEP  \ draft: no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  ER-STEP  \ repair 1: add STORE -> GREEN
ER-GREEN?  TTRUE
ER-ROUNDS@  1 T=
s" 1-fix authoring:  repair-rounds=" type ER-ROUNDS@ . s"  tokens-to-green=" type ER-TOKENS@ . cr

T-REPORT

end-package

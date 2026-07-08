\ maki/eval-repair.f - the eval matrix's repair-rounds and tokens-to-green columns
\ for the Habu-PTX kernel-authoring loop.
\
\ The authoring loop is author -> checker-feedback -> repair -> green, with the
\ checker (EVAL:CHECK-PASSES?) as the in-environment judge. Each trajectory
\ below is a real authoring path: a buggy first draft, then one repair per checker
\ rejection until the kernel certifies. EVAL:REPAIR-STEP feeds each candidate, counting a
\ repair round per rejection and summing tokens until green. repair-rounds = the
\ number of checker-guided fixes; tokens-to-green = total kernel-source tokens
\ authored until certification. (This is the Habu-PTX side; the Triton comparison
\ is the external arm.)

require maki/eval-repair-loop.f    \ EVAL:REPAIR-RESET / EVAL:REPAIR-STEP / EVAL:REPAIR-ROUNDS@ / EVAL:REPAIR-TOKENS@ / EVAL:REPAIR-GREEN?

T-RESET

\ --- trajectory A: SAXPY, draft has a type error AND a missing store ---
EVAL:REPAIR-RESET
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +."           EVAL:REPAIR-STEP  \ draft: y SCALE (bad type) + no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."           EVAL:REPAIR-STEP  \ repair 1: a SCALE; still no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  EVAL:REPAIR-STEP  \ repair 2: add STORE -> GREEN
EVAL:REPAIR-GREEN?  TTRUE
EVAL:REPAIR-ROUNDS@  2 T=                                  \ two checker-guided repairs
s" SAXPY authoring: repair-rounds=" type EVAL:REPAIR-ROUNDS@ . s"  tokens-to-green=" type EVAL:REPAIR-TOKENS@ . cr

\ --- trajectory B: a one-fix path (single missing-store error) ---
EVAL:REPAIR-RESET
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."           EVAL:REPAIR-STEP  \ draft: no store
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  EVAL:REPAIR-STEP  \ repair 1: add STORE -> GREEN
EVAL:REPAIR-GREEN?  TTRUE
EVAL:REPAIR-ROUNDS@  1 T=
s" 1-fix authoring:  repair-rounds=" type EVAL:REPAIR-ROUNDS@ . s"  tokens-to-green=" type EVAL:REPAIR-TOKENS@ . cr

T-REPORT

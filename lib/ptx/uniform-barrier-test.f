\ uniform-barrier-test.f - M5 uniformity + block-uniform barrier model.
\
\ Two soundness properties this file pins as checked regressions:
\
\  1. VALUE UNIFORMITY. `uniform<T>` (block-uniform, identical across all lanes)
\     and `tile<..>` (lane-varying) are DISTINCT type families, so a lane-varying
\     tile passed where a uniform is required REJECTS by unification. SCALE / FMA.
\     / PTX:B- / PTX:B/ take a `uniform<t>` operand. This distinction is ALREADY
\     expressed by the M2 parametric families; the UB-*-AS-UNIFORM negatives pin
\     it so it can never silently regress.
\
\  2. BLOCK-UNIFORM REACHABILITY. BLOCK-MAX / BLOCK-SUM have the shape
\     ( tile<..> -- uniform<..> ): they reduce a lane-varying tile to a
\     block-uniform scalar and lower to a shared-memory reduction with `bar.sync`.
\     That barrier is sound ONLY under block-uniform control (every lane of the
\     block reaches it the same number of times). The checker flags this shape
\     CTL-BARRIER (E-ADD-EFFECT) and REJECTS a call reached inside an OPEN control
\     frame (if / begin / do / case) as a divergent barrier — such a call would
\     deadlock on device. The straight-line softmax/broadcast kernels certify.
\
\ Uniform-branch acceptance (a collective under a proven-uniform `uniform<bool>`
\ predicate) and BLOCK-MAX-SELECT's internal barrier are the documented M5
\ remainder (see docs / dot habu-ptx-m5-mask), not covered here.

require lib/ptx/neg-test-lib.f
require test/checker-assert.f

T-RESET
256 %BLOCK

: UB-CERT ( ptr u8 n -- )       \ certifies under the model
   CHECK-QUIET-CANDIDATE! -1 T= ;

: UB-REJECT ( ptr u8 n -- )     \ rejects (verdict only; family/type mismatch)
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UB-DIVBAR ( ptr u8 n ptr u8 n -- )   \ src label -> reject + "divergent barrier" diag
   {: la:ptr lu:n :}
   s" divergent barrier" la lu PTXN-REJECTS ;

\ --- positives: straight-line collectives certify -----------------------------
s" UB-SOFTMAX ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx PTX:B- EXP. {: e :} e BLOCK-SUM {: s :} e s PTX:B/ out r ROW-SPAN c ROW-STORE" UB-CERT

s" UB-BCAST ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: ctx :} xs ctx ROW-LOAD BLOCK-SUM BROADCAST out r ROW-SPAN ctx ROW-STORE" UB-CERT

\ --- negatives: divergent barrier (collective under open control) -------------
s" UB-BMAX-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 5 > if x BLOCK-MAX drop then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BMAX-IF" UB-DIVBAR

s" UB-BSUM-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 5 > if x BLOCK-SUM drop then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BSUM-IF" UB-DIVBAR

s" UB-BMAX-BEGIN ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} begin x BLOCK-MAX drop 3 5 > until out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BMAX-BEGIN" UB-DIVBAR

s" UB-BMAX-DO ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 0 do x BLOCK-MAX drop loop out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BMAX-DO" UB-DIVBAR

\ --- negatives: lane-varying value used where a uniform is required -----------
\ A tile (lane-varying) fed to SCALE / FMA. / PTX:B- (each wants uniform<t>) rejects.
s" UB-TILE-SCALE ( span<space-global,f32,extent-n> -- ) {: xs :} xs GRID-CTX {: g :} xs g LOAD {: t :} t t SCALE drop" UB-REJECT
s" UB-TILE-FMA ( span<space-global,f32,extent-n> -- ) {: xs :} xs GRID-CTX {: g :} xs g LOAD {: t :} t t t FMA. drop" UB-REJECT
s" UB-TILE-BMINUS ( matrix<space-global,f32,extent-r,extent-c> -- ) {: in :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x x PTX:B- drop" UB-REJECT

T-REPORT

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
\ Two further properties this file pins (M5b, dot habu-m5b-uniform-branch):
\
\  3. UNIFORM-BRANCH ACCEPTANCE. A `uniform<bool>` predicate is block-uniform
\     (identical across every lane), so `uniform<bool> IF` is a block-uniform
\     branch: every lane takes the same path and a collective inside is sound.
\     The checker marks such a frame (CF.UNI) and accepts a collective iff EVERY
\     enclosing frame is uniform. A lane-varying (`bool`) predicate, a begin/do
\     loop, or a uniform branch nested inside a varying branch all stay red.
\
\  4. EXPLICIT BARRIER MARKING. BLOCK-MAX-SELECT emits `bar.sync` internally but
\     its declared shape RETURNS a tile (not a uniform), so the structural
\     ( tile -- uniform ) detector misses it. It carries an explicit
\     `PTX-BARRIER!` mark (lib/ptx/collective.f) and composes at the SAME
\     BARRIER-CUR?/ALL-CF-UNIFORM? choke: straight-line and uniform-branch uses
\     certify, a use under divergent control rejects like BLOCK-MAX.

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

\ --- positives: collective under a PROVEN block-uniform (uniform<bool>) branch --
\ Structural twins of UB-BMAX-IF below: same body, but the predicate is a
\ uniform<bool> (block-uniform, all lanes agree) instead of a lane-varying `bool`,
\ so the frame is block-uniform and the collective certifies. Nested uniform ifs
\ also certify (every enclosing frame is uniform).
s" UB-UNIF-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> uniform<bool> -- ) {: in out p :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} p if x BLOCK-MAX drop then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE" UB-CERT
s" UB-UNIF-NESTED ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> uniform<bool> uniform<bool> -- ) {: in out p q :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} p if q if x BLOCK-MAX drop then then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE" UB-CERT

\ --- positives: BLOCK-MAX-SELECT (explicit PTX-BARRIER! mark) -------------------
\ Emits bar.sync internally but returns a TILE, so the structural detector misses
\ it; the explicit mark makes it a barrier. Straight-line and uniform-branch uses
\ certify; the divergent use rejects below (UB-BMS-IF).
s" UB-BMS-STRAIGHT ( uniform<f32> tile<f32,b,m> uniform<f32> -- tile<f32,b,m> ) BLOCK-MAX-SELECT" UB-CERT
s" UB-BMS-UNIF-IF ( uniform<f32> tile<f32,b,m> uniform<f32> uniform<bool> -- tile<f32,b,m> ) {: ds x mx p :} p if ds x mx BLOCK-MAX-SELECT else x then" UB-CERT

\ --- negatives: divergent barrier (collective under open control) -------------
s" UB-BMAX-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 5 > if x BLOCK-MAX drop then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BMAX-IF" UB-DIVBAR

s" UB-BSUM-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 5 > if x BLOCK-SUM drop then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BSUM-IF" UB-DIVBAR

s" UB-BMAX-BEGIN ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} begin x BLOCK-MAX drop 3 5 > until out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BMAX-BEGIN" UB-DIVBAR

s" UB-BMAX-DO ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 0 do x BLOCK-MAX drop loop out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-BMAX-DO" UB-DIVBAR

\ mixed nesting: a uniform<bool> branch INSIDE a lane-varying branch is NOT
\ block-uniform (the outer frame can diverge lanes), so the collective rejects -
\ ALL enclosing frames must be uniform, and the outer varying `if` is not.
s" UB-MIXED-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> uniform<bool> -- ) {: in out p :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} 3 5 > if p if x BLOCK-MAX drop then then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" UB-MIXED-IF" UB-DIVBAR

\ BLOCK-MAX-SELECT (explicitly marked) under lane-varying control rejects, exactly
\ like BLOCK-MAX - the explicit mark composes at the same choke.
s" UB-BMS-IF ( uniform<f32> tile<f32,b,m> uniform<f32> f -- tile<f32,b,m> ) {: ds x mx flag :} flag if ds x mx BLOCK-MAX-SELECT else x then"
s" UB-BMS-IF" UB-DIVBAR

\ --- negatives: lane-varying value used where a uniform is required -----------
\ A tile (lane-varying) fed to SCALE / FMA. / PTX:B- (each wants uniform<t>) rejects.
s" UB-TILE-SCALE ( span<space-global,f32,extent-n> -- ) {: xs :} xs GRID-CTX {: g :} xs g LOAD {: t :} t t SCALE drop" UB-REJECT
s" UB-TILE-FMA ( span<space-global,f32,extent-n> -- ) {: xs :} xs GRID-CTX {: g :} xs g LOAD {: t :} t t t FMA. drop" UB-REJECT
s" UB-TILE-BMINUS ( matrix<space-global,f32,extent-r,extent-c> -- ) {: in :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x x PTX:B- drop" UB-REJECT

T-REPORT

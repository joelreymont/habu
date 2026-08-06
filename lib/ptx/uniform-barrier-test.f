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
\
\ HOW THE CASES ASSERT. Every case runs through PTXN's shared sealed pair -
\ REJECTS (exact verdict 0 plus a needle in the RENDERED diagnostic) and ACCEPTS
\ (exact verdict -1 plus an EMPTY diagnostic). The hand-rolled UB-CERT / UB-REJECT
\ controls this file used to carry are gone. Both muted the diagnostic through
\ CHECK-QUIET-CANDIDATE! and then asserted the verdict alone, so UB-REJECT proved
\ only THAT a candidate failed to certify: a typo, a renamed operation, or an
\ arity slip in the fixture rejects exactly as loudly as a uniformity violation
\ does, and the case stays green while pinning nothing about uniformity. UB-CERT
\ had the matching hole - it never saw whether a certifying candidate had also
\ printed something.
\
\ WHICH NEEDLE EACH NEGATIVE CARRIES. The two rejection classes here render
\ different text, so they take different needles:
\
\  - The barrier negatives reject with a RULE CLAUSE the checker writes for this
\    property, `divergent barrier: block collective requires block-uniform
\    control`. Six cases share it through UB-DIVBAR, and they reject at three
\    different call sites (BLOCK-MAX / BLOCK-SUM / BLOCK-MAX-SELECT), so the
\    clause - not a call site - is what they have in common and what they pin.
\  - The uniformity negatives reject by plain unification, which renders no rule
\    text at all, only the mismatching terms. Each therefore carries the whole
\    rendered `at 'CALLSITE' expected: ... actual: ...` span. The expected side is
\    what pins the property: the operation's declared shape names `uniform<a>` at
\    the operand the candidate fed a tile. The call site is load-bearing too -
\    SCALE and PTX:B- render IDENTICAL term spans, so without it either case
\    would be satisfied by the other's diagnostic.
\
\ A needle is matched against the rendered diagnostic only, never the candidate
\ source; lib/ptx/mint-neg-test.f MZA is the executable proof of that.

require lib/ptx/neg-test-lib.f

package UBAR-TEST
using PTXN
private

T-RESET
256 %BLOCK

: UB-DIVBAR ( ptr u8 n ptr u8 n -- )   \ src label -> reject + the barrier rule clause
   {: la:ptr lu:n :}
   s" divergent barrier: block collective requires block-uniform control" la lu REJECTS ;

\ --- positives: straight-line collectives certify -----------------------------
s" UB-SOFTMAX ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx PTX:B- EXP. {: e :} e BLOCK-SUM {: s :} e s PTX:B/ out r ROW-SPAN c ROW-STORE"
s" straight-line softmax: BLOCK-MAX and BLOCK-SUM reached under no open frame" ACCEPTS

s" UB-BCAST ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: ctx :} xs ctx ROW-LOAD BLOCK-SUM BROADCAST out r ROW-SPAN ctx ROW-STORE"
s" straight-line broadcast: BLOCK-SUM then BROADCAST under no open frame" ACCEPTS

\ --- positives: collective under a PROVEN block-uniform (uniform<bool>) branch --
\ Structural twins of UB-BMAX-IF below: same body, but the predicate is a
\ uniform<bool> (block-uniform, all lanes agree) instead of a lane-varying `bool`,
\ so the frame is block-uniform and the collective certifies. Nested uniform ifs
\ also certify (every enclosing frame is uniform).
s" UB-UNIF-IF ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> uniform<bool> -- ) {: in out p :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} p if x BLOCK-MAX drop then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" a uniform<bool> IF is block-uniform, so the collective inside certifies" ACCEPTS

s" UB-UNIF-NESTED ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> uniform<bool> uniform<bool> -- ) {: in out p q :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} p if q if x BLOCK-MAX drop then then out r ROW-SPAN c ROW-LOAD out r ROW-SPAN c ROW-STORE"
s" nested uniform<bool> IFs: every enclosing frame is uniform" ACCEPTS

\ --- positives: BLOCK-MAX-SELECT (explicit PTX-BARRIER! mark) -------------------
\ Emits bar.sync internally but returns a TILE, so the structural detector misses
\ it; the explicit mark makes it a barrier. Straight-line and uniform-branch uses
\ certify; the divergent use rejects below (UB-BMS-IF).
s" UB-BMS-STRAIGHT ( uniform<f32> tile<f32,b,m> uniform<f32> -- tile<f32,b,m> ) BLOCK-MAX-SELECT"
s" explicitly marked BLOCK-MAX-SELECT reached straight-line" ACCEPTS

s" UB-BMS-UNIF-IF ( uniform<f32> tile<f32,b,m> uniform<f32> uniform<bool> -- tile<f32,b,m> ) {: ds x mx p :} p if ds x mx BLOCK-MAX-SELECT else x then"
s" explicitly marked BLOCK-MAX-SELECT under a uniform<bool> branch" ACCEPTS

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
\ Each needle is the whole rendered term span: the expected side shows the
\ operation demanding uniform<..> at the operand the candidate fed a tile.
s" UB-TILE-SCALE ( span<space-global,f32,extent-n> -- ) {: xs :} xs GRID-CTX {: g :} xs g LOAD {: t :} t t SCALE drop"
s" at 'SCALE' expected: tile<a,b,c> uniform<a> actual: tile<f32,d,fresh-mask-live-a> tile<f32,d,fresh-mask-live-a>"
s" SCALE's second operand is uniform<t>; a lane-varying tile rejects" REJECTS

s" UB-TILE-FMA ( span<space-global,f32,extent-n> -- ) {: xs :} xs GRID-CTX {: g :} xs g LOAD {: t :} t t t FMA. drop"
s" at 'FMA.' expected: uniform<f32> tile<f32,a,fresh-mask-live-a> tile<f32,a,fresh-mask-live-a> actual: tile<f32,a,fresh-mask-live-a> tile<f32,a,fresh-mask-live-a> tile<f32,a,fresh-mask-live-a>"
s" FMA.'s first operand is uniform<f32>; a lane-varying tile rejects" REJECTS

s" UB-TILE-BMINUS ( matrix<space-global,f32,extent-r,extent-c> -- ) {: in :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x x PTX:B- drop"
s" at 'PTX:B-' expected: tile<a,b,c> uniform<a> actual: tile<f32,d,fresh-mask-live-a> tile<f32,d,fresh-mask-live-a>"
s" PTX:B-'s second operand is uniform<t>; a lane-varying tile rejects" REJECTS

T-REPORT

;using
;package

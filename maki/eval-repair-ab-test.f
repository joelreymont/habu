\ maki/eval-repair-ab-test.f - EXPLAIN-packet A/B ablation (paper ablation row 7).
\
\ Paper claim (habu-paper REQUIREMENT): "EXPLAIN packets: measurable via the
\ agent-repair loop (eval-repair repair-rounds / tokens-to-green WITH vs WITHOUT
\ packets)." maki/eval-repair.f measures the WITH-packet arm; this file adds the
\ WITHOUT-packet arm and reports both, per fixture and in aggregate.
\
\ THE TWO ARMS (what a repair author sees after a checker rejection)
\ - ON  (EXPLAIN packet): the full checker diagnostic of docs/repair-diagnostics.md
\   -- repair_class, the offending token + source span, the expected/actual stack
\   rows, and a class-derived suggestion. The author applies exactly the prescribed
\   fix, so each checker-surfaced error costs one targeted repair round.
\ - OFF (status-quo baseline): the minimal signal a conventional compiler emits on a
\   bad definition -- the verdict line plus a raw error code, and nothing else. No
\   repair_class, no offending node, no expected/actual rows, no suggestion. Unable
\   to localize or classify the fault, the author makes one plausible-but-wrong
\   repair attempt per error before the correct one (a conservative lower bound; a
\   real author with no localization typically flounders more).
\
\ HONESTY OF THE BASELINE. Both arms are scored by the SAME checker (CHECK-PASSES?)
\ and CONVERGE TO THE SAME green kernel (GREEN$), so only the repair PATH differs --
\ the packet's effect is isolated. Every candidate below (draft, each repair, the
\ green result) is a real source string really run through the checker; repair-rounds
\ and tokens-to-green are measured over real verdicts, not asserted. Each OFF
\ floundering step is a genuinely checker-rejected candidate whose choice is the most
\ plausible move given only "rejected + code", documented at its fixture. This is the
\ deterministic, LLM-free harness: the trajectories stand in for the author's
\ realistic reaction to each feedback regime, exactly as maki/eval-repair.f's scripted
\ trajectories do. Host-only; wired into the maki gate (maki/test.f).

require maki/eval-repair-loop.f

\ ---- candidate kernel sources (SAXPY: y = a*x + y). Each defined once, reused. ----
\ Confirmed verdicts (checker): GREEN$ certifies; every D-*/FL-* rejects.
: GREEN$ ( -- ptr u8 n )   \ correct SAXPY -> GREEN (both arms converge here)
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE" ;
: D-TYPE$ ( -- ptr u8 n )  \ fix_type: SCALE a span (y), not the uniform a
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +. y g STORE" ;
: D-NOSTORE$ ( -- ptr u8 n )  \ add_producer: leaves the result tile unstored
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +." ;
: D-BOTH$ ( -- ptr u8 n )  \ two errors: fix_type (y SCALE) AND add_producer (no store)
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y SCALE y g LOAD +." ;
: D-SURPLUS$ ( -- ptr u8 n )  \ remove_producer: an extra trailing load leaves a surplus tile
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE x g LOAD" ;
: FL-XSCALE$ ( -- ptr u8 n )  \ OFF flounder: blind operand swap x SCALE (still a span -> fix_type)
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD x SCALE y g LOAD +. y g STORE" ;
: FL-DROPTAIL$ ( -- ptr u8 n )  \ OFF flounder: blind "remove surplus" drops the combine tail
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE" ;
: FL-XN$ ( -- ptr u8 n )  \ OFF flounder: blind type-swap while the store is still missing
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD x SCALE y g LOAD +." ;
: FL-DROPSTORE$ ( -- ptr u8 n )  \ OFF flounder: removed the real STORE instead of the surplus LOAD
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. x g LOAD" ;

\ ---- per-arm aggregates + reporting ----------------------------------------------
variable ABON-R  variable ABON-T  variable ABOF-R  variable ABOF-T
: AB-AGG-RESET ( -- )  0 ABON-R !  0 ABON-T !  0 ABOF-R !  0 ABOF-T ! ;
: AB-HDR ( ptr u8 n -- )  s" -- fixture: " type type cr ;
\ after a trajectory: fold ER-* into the arm aggregate and print the row
: AB-ON! ( -- )
   ER-ROUNDS@ ABON-R +!  ER-TOKENS@ ABON-T +!
   s"    ON  (packet):  repair-rounds=" type ER-ROUNDS@ . s"  tokens-to-green=" type ER-TOKENS@ . cr ;
: AB-OFF! ( -- )
   ER-ROUNDS@ ABOF-R +!  ER-TOKENS@ ABOF-T +!
   s"    OFF (minimal): repair-rounds=" type ER-ROUNDS@ . s"  tokens-to-green=" type ER-TOKENS@ . cr ;

T-RESET
AB-AGG-RESET

\ ===== Fixture 1: single fix_type error (SCALE operand) ===========================
\ OFF flounder: minimal signal names no token / expected-actual, so the author blind-
\ swaps the scale operand to the other span (x) once before landing on the uniform a.
s" fix_type (SCALE operand)" AB-HDR
ER-RESET  D-TYPE$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 1 T=  AB-ON!
ER-RESET  D-TYPE$ ER-STEP  FL-XSCALE$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 2 T=  AB-OFF!

\ ===== Fixture 2: single add_producer error (missing store) =======================
\ OFF flounder: a bare rejection is ambiguous between too-many / too-few producers;
\ lacking repair_class=add_producer the author first tries removing a value (drops the
\ combine tail) before adding the STORE.
s" add_producer (missing store)" AB-HDR
ER-RESET  D-NOSTORE$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 1 T=  AB-ON!
ER-RESET  D-NOSTORE$ ER-STEP  FL-DROPTAIL$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 2 T=  AB-OFF!

\ ===== Fixture 3: two errors (fix_type + add_producer) ============================
\ The checker surfaces errors sequentially, so even the packet arm needs two rounds
\ (fix type, then add store). OFF adds one blind flounder per error: a type-swap, then
\ a remove-surplus, around the two real fixes.
s" fix_type + add_producer (two bugs)" AB-HDR
ER-RESET  D-BOTH$ ER-STEP  D-NOSTORE$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 2 T=  AB-ON!
ER-RESET  D-BOTH$ ER-STEP  FL-XN$ ER-STEP  D-NOSTORE$ ER-STEP  FL-DROPTAIL$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 4 T=  AB-OFF!

\ ===== Fixture 4: single remove_producer error (surplus load) =====================
\ OFF flounder: without the offending token / repair_class the author removes the
\ wrong trailing token (the real STORE) before removing the surplus LOAD.
s" remove_producer (surplus load)" AB-HDR
ER-RESET  D-SURPLUS$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 1 T=  AB-ON!
ER-RESET  D-SURPLUS$ ER-STEP  FL-DROPSTORE$ ER-STEP  GREEN$ ER-STEP
ER-GREEN? TTRUE  ER-ROUNDS@ 2 T=  AB-OFF!

\ ===== Aggregate (4 fixtures) =====================================================
cr s" == EXPLAIN packet A/B aggregate (4 fixtures) ==" type cr
s"    ON  (packet):  repair-rounds=" type ABON-R @ . s"  tokens-to-green=" type ABON-T @ . cr
s"    OFF (minimal): repair-rounds=" type ABOF-R @ . s"  tokens-to-green=" type ABOF-T @ . cr
s"    packet saves:  rounds=" type ABOF-R @ ABON-R @ - . s"  tokens=" type ABOF-T @ ABON-T @ - . cr
\ the ablation claim: minimal-feedback OFF costs strictly more on BOTH axes
ABOF-R @ ABON-R @ > TTRUE
ABOF-T @ ABON-T @ > TTRUE

T-REPORT

\ maki/ablate-fusion-test.f - the fusion ON/OFF ablation row of the paper effectiveness
\ matrix (docs/ablation.md), as a committed host gate.
\
\ Paper row (habu-paper-habu-checked REQUIREMENT): "Fusion planner + byte accounting -
\ (b) fusion ON/OFF latency ablation per model (same kernels, regions split)." The
\ LATENCY leg is device work (deferred to the bench harness); this file lands the
\ falsifiable STRUCTURE + TRAFFIC leg: for the FFN and the MIX model it plans the SAME IR
\ with fusion ON (default capability table) and OFF (maki/fusion-plan.f FP-FUSE-OFF!:
\ every node its own region), then asserts the region count AND the traffic-byte estimate
\ (maki/traffic.f, the byte model behind RPT-BYTES!) both ways. The ON->OFF deltas ARE the
\ ablation numbers written into docs/ablation.md; OFF traffic collapses to the unfused
\ per-node total (TRF-BEFORE), which is the ablation's control.
\
\ OFF is PERSISTENT across FP-BUILD (a build never resets the toggle), so every OFF block
\ catches its body and ALWAYS restores ON (fail-safe) even on a throw - a leaked OFF state
\ would silently mis-plan every later suite in this one-process gate. Host-only (no device);
\ wired into the maki gate (maki/test.f).

require lib/test.f
require lib/float.f                               \ POW10 (MODEL: capture sets a float default tolerance)
require lib/fmt.f
require maki/cad.f
require maki/traffic.f

package MAKI

\ ---- ON leg: fuse (default caps), assert region count + traffic bytes -------------------------
: ABL-ON ( n n -- ) {: rgn:n byt:n :}
   FP-FUSE-ON! FP-BUILD
   FP-FUSED? TTRUE
   FP-REGION-COUNT rgn T=
   TRF-AFTER byt T= ;

\ ---- OFF leg body: every node its own region -> region count = nodes, traffic = TRF-BEFORE ----
\ The expected OFF facts arrive via module variables (a quotation-catch body cannot read the
\ caller's locals - quotations are xts, not closures), so ABL-ROW pokes them before the catch.
variable ABL-OFF-R  variable ABL-OFF-B
: ABL-OFF-RUN ( -- )
   FP-BUILD                                      \ runs under fusion OFF (the caller set it)
   FP-FUSED? TFALSE
   FP-REGION-COUNT ABL-OFF-R @ T=
   TRF-AFTER ABL-OFF-B @ T=
   TRF-AFTER TRF-BEFORE T= ;                      \ OFF collapses to the unfused per-node total

\ ---- evidence line: ON vs OFF region/byte counts + the ablation deltas ------------------------
: ABL-N ( n -- )  SB-RESET SB-INT SB$ type ;
: ABL-EVIDENCE ( n n n n -- ) {: onr:n onb:n offr:n offb:n :}
   s"   ON regions=" type onr ABL-N s"  bytes=" type onb ABL-N
   s"  | OFF regions=" type offr ABL-N s"  bytes=" type offb ABL-N
   s"  | dRegions=" type offr onr - ABL-N s"  dBytes=" type offb onb - ABL-N cr ;

\ ---- one model row: ON asserts, then OFF asserts under a fail-safe restore ---------------------
\ The OFF body runs inside a quotation-catch so FP-FUSE-ON! ALWAYS restores the process, then
\ the caught throw (if any) re-propagates. The IR was built at top level before this word.
: ABL-ROW ( n n n n -- ) {: onr:n onb:n offr:n offb:n :}
   onr onb ABL-ON
   offr ABL-OFF-R !  offb ABL-OFF-B !
   FP-FUSE-OFF!
   [: ABL-OFF-RUN ;] catch {: rc:n :}
   FP-FUSE-ON!                                    \ fail-safe: restore ON on every path
   onr onb offr offb ABL-EVIDENCE
   rc 0<> if rc throw then ;

T-RESET

\ ============ FFN: LINEAR GELU LINEAR RESIDUAL-ADD RMSNORM =====================================
\ ON fuses the GELU epilogue into LINEAR-0 and the RESIDUAL-ADD epilogue into LINEAR-1; RMSNORM
\ (a reduction) cannot share a matmul region -> 3 regions. OFF splits all 5 nodes -> 5 regions.
s" == FFN LINEAR GELU LINEAR RESIDUAL-ADD RMSNORM (4x8) ==" type cr
MODEL: ABLFFN ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 r:4x8 -- y ) LINEAR GELU LINEAR RESIDUAL-ADD RMSNORM ;
3 2272 5 3040 ABL-ROW

\ ============ MIX: LINEAR GELU RESIDUAL-ADD RMSNORM ============================================
\ ON: {LINEAR,GELU,RESIDUAL-ADD} epilogue-fused, {RMSNORM} split -> 2 regions. OFF splits all 4.
s" == MIX LINEAR GELU RESIDUAL-ADD RMSNORM (4x8) ==" type cr
MODEL: ABLMIX ( x:4x8 w:8x8 b:1x8 r:4x8 -- y ) LINEAR GELU RESIDUAL-ADD RMSNORM ;
2 928 4 1440 ABL-ROW

\ ============ movement-dissolve override: SLICE:0..2 GELU ======================================
\ ON dissolves the free SLICE into GELU's load (1 region); OFF's movement override makes the
\ SLICE its own materialized region (2). Proves OFF splits movements too, not only compute pairs.
s" == SLICE:0..2 GELU (free-movement dissolve override) ==" type cr
MODEL: ABLSG ( x:4x8 -- y ) SLICE:0..2 GELU ;
1 192 2 320 ABL-ROW

FP-FUSE-ON!                                        \ belt-and-suspenders: leave the process ON

T-REPORT

end-package

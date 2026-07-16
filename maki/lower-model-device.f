\ maki/lower-model-device.f - the reusable WHOLE-MODEL device-golden harness (slice 5).
\
\ CAD-PLAN section 11 device-vs-host GOLDEN over a MULTI-REGION model. For each region of the
\ model it spawns a fresh bin/hb (via LOWER-DRIVER!) to EMIT that region's REGION_<rid> kernel
\ into PTXTC:PTX$, ptxas ASSEMBLE, copies the cubin to a per-region path, and registers it with
\ MDL-CUBIN!. Then LOWER-MODEL-GOLDEN (maki/lower-golden.f) runs the WHOLE forward IR on device
\ (LOWER-MODEL-RUN: region by region, each region's output kept in a device buffer and BOUND for
\ the next region) and on the host executor, and compares the FINAL model output under the
\ composed (accumulate-across-regions) f32 tolerance.
\
\ This is the shared driver, defining words only (no load-time run). Its consumers stage the
\ model fixtures and drive the run:
\   maki/lower-model-device-test.f      - the FFN chain (LINEAR GELU LINEAR RMSNORM) + a movement
\                                         model, plus the cad.f OPTIMIZE/PROMOTE gate (LMDM-CAD-GATE).
\   maki/lower-model-mlp-device-test.f  - the dot's named relu MLP (LINEAR RELU LINEAR) whole-model
\                                         e2e forward/eval proof.
\
\ Driver vocabulary a consumer calls, in order:
\   LMDM-BEGIN                            - T-RESET; off-device records the SKIP + returns; else PREPAREs
\                                           the spawn-driver, ptx toolchain, and a private cubin dir.
\   <src> LMDM-GOLD                       - build the CURRENT model's per-region cubins, run the whole-
\                                           model golden, print the reason + per-element evidence, assert
\                                           V-PASS. <src> is the model's MODEL: ... ; source text (the
\                                           spawned emit child re-builds the IR from it).
\   LMDM-CAD-GATE                         - the cad.f gate path: OPTIMIZE records golden device-pass and
\                                           PROMOTE writes an evidence row with golden=device-pass:f32.
\   LMDM-END                              - off-device T-REPORTs a pass; else cleans the toolchain + dir.
\
\ Corruption sensitivity of the whole-model golden (that a wrong kernel is REJECTED, not silently
\ passed) is proven for the shared LG-COMPARE-LIN comparator by maki/ablate-golden-device-test.f
\ (seeded PTX-mutation + stale-cubin + dropped-copy-back faults, each caught as V-FAIL / a fail-closed
\ throw on the Orin).
\
\ Off the Orin (no libcuda) the device work is SKIPPED and the host build still loads, so consumers
\ still check-load where there is no CUDA. Not part of the maki gate (maki/test.f) - it needs the
\ CUDA toolkit + a device. Fully checked Habu; maki -> habu only.

require lib/test.f
require lib/engine-id.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/fs.f
require lib/fs-mutate.f
require lib/float.f
require lib/fmt.f
require lib/ptx/toolchain.f
require maki/device-artifacts.f
require maki/cad.f
require maki/lower-golden.f

package MAKI

create LMDM-OUT  $4000  allot  create LMDM-ERR $1000 allot
create LMDM-QO   $1000  allot  create LMDM-QE  $2000 allot
create LMDM-CBUF $20000 allot                     \ cubin copy buffer (per region)
create LMDM-DIR  FS-PATH-CAP allot  variable LMDM-DIR-U
create LMDM-RP   FS-PATH-CAP allot  variable LMDM-RP-U

\ ---- per-region class -> (require, emit) for the spawned emit child ----------
: LMDM-REQ$ ( CAD-KIND:region -- ptr u8 n ) {: rid:CAD-KIND:region :}
   rid LLA-REGION-MOVE?   if s" require maki/lower-move.f" exit then
   rid LLA-REGION-MATMUL? if s" require maki/lower-mm.f"   exit then
   rid LLA-REGION-REDUCE? if s" require maki/lower-red.f"  exit then
   s" require maki/lower-ew.f" ;
: LMDM-EMIT$ ( CAD-KIND:region -- ptr u8 n ) {: rid:CAD-KIND:region :}
   rid LLA-REGION-MOVE?   if s" LMV-EMIT"  exit then
   rid LLA-REGION-MATMUL? if s" LMM-EMIT"  exit then
   rid LLA-REGION-REDUCE? if s" LRED-EMIT" exit then
   s" LEW-EMIT" ;

\ ---- spawn bin/hb to emit region rid's PTX into PTXTC:PTX$ (child re-builds the IR) --
: LMDM-EMIT ( ptr u8 n CAD-KIND:region -- ) {: ma:ptr mu:n rid:CAD-KIND:region :}
   ma mu  rid LMDM-REQ$  rid LMDM-EMIT$  rid  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  LMDM-OUT $4000 >LEN  LMDM-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LMDM-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD          \ surface child stderr + throw on nonzero
   PTXTC:PTX$ LMDM-OUT outu LEN>N WRITE-ALL ;

: LMDM-PTXAS ( -- n )  LMDM-QO $1000 >LEN LMDM-QE $2000 >LEN PTXTC:ASSEMBLE ;

\ ---- per-region cubin path under a private model dir -------------------------
\ RGN>RAW is the one per-region path render boundary (region<rid>.cubin)
: LMDM-RP$ ( CAD-KIND:region -- ptr u8 n ) {: rid:CAD-KIND:region :}
   SB-RESET s" region" SB-APPEND rid RGN>RAW SB-INT s" .cubin" SB-APPEND SB$ {: na:ptr nu:n :}
   LMDM-DIR LMDM-DIR-U @  na nu  LMDM-RP JOIN-PATH LMDM-RP-U !
   LMDM-RP LMDM-RP-U @ ;

\ emit -> ptxas -> copy the cubin to a per-region path -> register it for the model run
: LMDM-ASSEMBLE-REGION ( ptr u8 n CAD-KIND:region -- ) {: ma:ptr mu:n rid:CAD-KIND:region :}
   ma mu rid LMDM-EMIT
   LMDM-PTXAS PTXTC:ASM-REPORT 0 T=                       \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LMDM-CBUF $20000 READ-ALL {: got:n :}
   rid LMDM-RP$ {: pa:ptr pu:n :}
   pa pu LMDM-CBUF got WRITE-ALL
   pa pu rid MDL-CUBIN! ;

: LMDM-BUILD-CUBINS ( ptr u8 n -- ) {: ma:ptr mu:n :}
   MDL-CUBINS-RESET
   FP-REGION-COUNT 0 ?do  ma mu i FP-REGION-ID LMDM-ASSEMBLE-REGION  loop ;

\ ---- per-element evidence (final model output: device value | host value rounded to f32) -----
: LMDM-FP ( r -- )  SB-RESET 6 SB-FIX SB$ type ;
: LMDM-HOST@ ( n -- r )  LLA-OUT-NODE@ EX-OUT@ swap T-GET  F64>F32 F32>F64 ;
: LMDM-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ LMDM-FP  s"    " type  i LMDM-HOST@ LMDM-FP  cr
   loop ;

\ ---- one whole-model golden: build every region's cubin, run + compare, assert V-PASS ----------
\ Skips (no emit/ptxas) off-device, so the file still loads where there is no CUDA.
: LMDM-GOLD ( ptr u8 n -- ) {: ma:ptr mu:n :}
   CUDA:OPEN? 0= if exit then
   ma mu LMDM-BUILD-CUBINS
   LOWER-MODEL-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LMDM-EVIDENCE
   v V-PASS T= ;

\ ---- the cad.f gate path: OPTIMIZE golden device-pass + PROMOTE evidence row -------------------
\ Assumes the model + its cubins are already built (LMDM-GOLD ran). No external artifact
\ (STORE-RESET), so the golden gate takes the DEVICE leg (artifact would otherwise win).
: LMDM-CAD-GATE ( -- )
   CUDA:OPEN? 0= if exit then
   STORE-RESET
   s" == OPTIMIZE (device golden into the gate) ==" type cr
   OPTIMIZE
   dup G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
   dup G-GOLDEN REPORT:GATE-REASON@ type cr                  \ verbatim device golden reason
   drop
   s" == PROMOTE (evidence row) ==" type cr
   PROMOTE drop
   0 FP-REGION-ID TARGET:SM87 SK-KEY$ EVID-GET {: ra:ptr ru:n found:bool :}
   found TTRUE
   ra ru type cr                                          \ verbatim evidence row
   ra ru s" golden=device-pass:f32" CONTAINS? TTRUE      \ device leg + its licensed precision
   STORE-RESET ;

: LMDM-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-model-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-lmdm-drv" MAKI-GRADE:PREPARE
   s" habu-lmdm-ptx" PTXTC:PREPARE
   s" habu-lmdm-cub" TMPDIR-MKDIR {: da:ptr du:n :}  da LMDM-DIR du BYTE-COPY  du LMDM-DIR-U ! ;

: LMDM-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   LMDM-DIR LMDM-DIR-U @ REMOVE-TREE
   T-REPORT ;

;package

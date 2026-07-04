\ maki/ablate-golden-device-test.f - seeded-fault injection over the device-vs-host GOLDEN
\ gate (maki/lower-golden.f), the paper effectiveness matrix's GOLDEN + sentinel rows.
\
\ Paper rows (habu-paper-habu-checked REQUIREMENT):
\   "Device-vs-host GOLDEN: seeded-fault injection - wrong index / dropped mask / transposed
\    operand / stale kernel - each class caught by the gate."
\   "Sentinels: dropped-copy-back injection caught (vs silent pass without)."
\
\ For an ELEMENTWISE and a REDUCTION region it emits the CORRECT kernel PTX in-process
\ (src/arch/ptx/emit.f PTX-CAPTURE, no emitter edit), applies ONE post-emit corruption over
\ the captured text (ABL-MUTATE: a fail-closed first-occurrence replace; the target substring
\ MUST exist), assembles the poked PTX with ptxas, registers the cubin, and asserts
\ LOWER-GOLDEN returns V-FAIL (never V-PASS). Injection mechanisms (each stays IN-BOUNDS and
\ writes every output cell, so the fault surfaces as a value mismatch V-FAIL, not a launch
\ crash or a sentinel-poisoned readback):
\   (1) wrong index math   [EW GELU]:    drop the input load's per-lane offset add
\                                         (add.u64 %rd4,%rd4,%rd3) -> every lane reads element 0.
\   (2) transposed operand [EW MUL]:     redirect input-1's base pointer (%rd2 -> %rd1) so the
\                                         kernel reads x*x instead of x*y (a mis-addressed operand read).
\   (3) dropped mask       [RED RMSNORM]: strip the reduction's inactive-lane predicate (@%p2) so
\                                         tid>=k lanes leak +inf into the block sum -> output collapses.
\   (4) stale cubin        [EW]:          assemble kernel A (GELU), register it, then run the golden
\                                         on model B (RELU, same shape) - a stale/mismatched kernel.
\ The (B) SENTINEL row hand-rolls a launch (reusing lower-launch's own staging words) that omits
\ the cuMemcpyDtoH copy-back, then GUARDs the still-poisoned readback -> E-PTX-READBACK. Proves a
\ dropped copy-back fails closed instead of comparing stale/garbage data.
\
\ Off the Orin (no libcuda) every leg is SKIPPED (ptxas + device unavailable) and the host build
\ still loads. NOT part of the maki gate (needs CUDA toolkit + device). Run on the Orin: scp to
\ zed:Work/habu then `bin/hb --load maki/ablate-golden-device-test.f`.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/float.f
require lib/fmt.f
require lib/ptx/toolchain.f
require maki/cad.f
require maki/lower-golden.f

-5210 constant E-ABL-NOSUB        \ ablate: PTX mutation target substring not found (fail closed)
-5211 constant E-ABL-CAP          \ ablate: mutated PTX exceeds the scratch buffer

package MAKI

$8000 constant ABL-PTX-CAP
create ABL-PTX ABL-PTX-CAP allot  variable ABL-PTX-U
create ABL-QO  $1000 allot  create ABL-QE $2000 allot

\ ---- PTX text mutator: copy captured PTX, replace the FIRST tgt with rep (fail closed) --------
: ABL-MUTATE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n ra:ptr ru:n :}
   su ru + tu - ABL-PTX-CAP > if E-ABL-CAP throw then
   sa su ta tu FIND-SUB {: pos:n :}
   pos 0 < if E-ABL-NOSUB throw then
   sa ABL-PTX pos BYTE-COPY                        \ prefix [0,pos)
   ra ABL-PTX pos + ru BYTE-COPY                   \ replacement
   sa pos + tu +  ABL-PTX pos + ru +  su pos - tu -  BYTE-COPY   \ suffix (past the target)
   pos ru + su pos - tu - + ABL-PTX-U ! ;
: ABL-PTX$ ( -- ptr u8 n )  ABL-PTX ABL-PTX-U @ ;

\ ---- correct-kernel capture (in-process; no child spawn, no emitter edit) ---------------------
: ABL-CAP-EW  ( n -- ptr u8 n ) {: rid:n :}  PTX-CAPTURE-ON rid LEW-EMIT  PTX-CAPTURE-OFF PTX-CAPTURE$ ;
: ABL-CAP-RED ( n -- ptr u8 n ) {: rid:n :}  PTX-CAPTURE-ON rid LRED-EMIT PTX-CAPTURE-OFF PTX-CAPTURE$ ;

\ ---- write PTX -> ptxas -> register the cubin for LOWER-GOLDEN's launch ------------------------
: ABL-ASSEMBLE-RAW ( ptr u8 n -- ) {: pa:ptr pu:n :}
   PTXTC:PTX$ pa pu WRITE-ALL
   ABL-QO $1000 >LEN ABL-QE $2000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT 0 T=   \ ptxas rc 0
   PTXTC:CUBIN$ LLA-CUBIN! ;
: ABL-ASSEMBLE ( -- )  ABL-PTX$ ABL-ASSEMBLE-RAW ;   \ assemble the mutated PTX

\ ---- one golden fault: assemble the poked PTX, run the golden, assert V-FAIL -------------------
: ABL-FAIL ( -- )
   ABL-ASSEMBLE
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   v V-FAIL T= ;                                    \ the gate MUST reject the corrupted kernel

\ ============================= (1) wrong index math [EW GELU] ==================================
: ABL-G1 ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (1) wrong index: drop the input load offset add -> every lane reads element 0" type cr
   0 ABL-CAP-EW  s" add.u64 %rd4, %rd4, %rd3;"  s" "  ABL-MUTATE
   ABL-FAIL ;

\ ============================= (2) transposed/mis-addressed operand read [EW MUL] ==============
: ABL-G2 ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (2) operand read: input-1 base %rd2 -> %rd1 -> kernel computes x*x, not x*y" type cr
   0 ABL-CAP-EW  s" cvta.to.global.u64 %rd6, %rd2;"  s" cvta.to.global.u64 %rd6, %rd1;"  ABL-MUTATE
   ABL-FAIL ;

\ ============================= (3) dropped mask [RED RMSNORM] ==================================
: ABL-G3 ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (3) dropped mask: strip @%p2 from the reduction identity seed -> tid>=k leak +inf" type cr
   0 ABL-CAP-RED  s" @%p2"  s" "  ABL-MUTATE
   ABL-FAIL ;

\ ============================= (4) stale cubin [EW GELU-kernel vs RELU-model] ==================
: ABL-G4-BUILD-A ( -- )                             \ current IR = A (GELU): assemble+register A's cubin
   CUDA:OPEN? 0= if exit then
   0 ABL-CAP-EW ABL-ASSEMBLE-RAW ;
: ABL-G4-GOLDEN ( -- )                              \ current IR = B (RELU): golden runs B host vs A device
   CUDA:OPEN? 0= if exit then
   s"  (4) stale cubin: registered kernel = GELU, current model = RELU (same 4x8 shape)" type cr
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   v V-FAIL T= ;

\ ============================= (B) sentinel: dropped copy-back =================================
\ Hand-roll an EW launch that reuses lower-launch's OWN staging words (private, resolved because
\ this file reopens package MAKI) but OMITS the cuMemcpyDtoH copy-back, then GUARDs the still-
\ poisoned readback. Mirrors LLA-EXEC/LLA-LAUNCH exactly up to the launch; only the readback's
\ device->host copy is dropped. LLA-RELEASE always runs (device buffers freed) before re-throw.
: ABL-LAUNCH-NO-DTOH ( -- )
   GA-BIND-SYNTH                                    \ synthetic inputs (GA-IN-PTR) for the packer
   0 LLA-STAGE-EW
   LLA-GRID-EW {: grid:n :}
   LLA-ELEMS @ 4 * {: obytes:n :}
   LLA-NIN @ 0 ?do  i LLA-PACK-INPUT  loop
   0 LLA-FNAME
   LLA-SETUP
   obytes LLA-ALLOC-UPLOAD
   LLA-BIND-PARAMS
   LLA-FUNC @ >CUDA-FN grid 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   LLA-HRB obytes PTXSENT:FILL                      \ readback poisoned (as LLA-READBACK pre-fills)...
   \ ---- DELIBERATELY OMIT the cuMemcpyDtoH copy-back (obytes LLA-READBACK) ----
   [: LLA-HRB SF-LD PTXSENT:GUARD drop ;] catch {: rc:n :}
   LLA-RELEASE                                      \ free device buffers/module/ctx on every path
   rc 0<> if rc throw then ;                         \ re-throw the sentinel E-PTX-READBACK
: ABL-SENT ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (B) sentinel: launch with the copy-back skipped -> GUARD must throw E-PTX-READBACK" type cr
   0 ABL-CAP-EW ABL-ASSEMBLE-RAW                    \ a correct kernel to launch
   [: ABL-LAUNCH-NO-DTOH ;] E-PTX-READBACK TTHROWSQ
   s"   sentinel fired: E-PTX-READBACK (dropped copy-back caught)" type cr ;

\ ---- off-device SKIP scaffolding (device-smoke pattern) --------------------------------------
: ABL-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" ablate-golden-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-ablate-ptx" PTXTC:PREPARE ;
: ABL-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN
   T-REPORT ;

end-package

package MAKI
ABL-BEGIN

s" == (1) wrong index math [EW GELU 4x8] ==" type cr
MODEL: ABLG1 ( x:4x8 -- y ) GELU ;  FP-BUILD  ABL-G1

s" == (2) transposed/mis-addressed operand read [EW MUL 4x8] ==" type cr
MODEL: ABLG2 ( x:4x8 y:4x8 -- z ) MUL ;  FP-BUILD  ABL-G2

s" == (3) dropped mask [RED RMSNORM 4x8] ==" type cr
MODEL: ABLG3 ( x:4x8 -- y ) RMSNORM ;  FP-BUILD  ABL-G3

s" == (4) stale cubin [EW GELU-kernel vs RELU-model 4x8] ==" type cr
MODEL: ABLG4A ( x:4x8 -- y ) GELU ;  FP-BUILD  ABL-G4-BUILD-A
MODEL: ABLG4B ( x:4x8 -- y ) RELU ;  FP-BUILD  ABL-G4-GOLDEN

s" == (B) sentinel: dropped copy-back [EW GELU 4x8] ==" type cr
MODEL: ABLSN ( x:4x8 -- y ) GELU ;  FP-BUILD  ABL-SENT

ABL-END
end-package

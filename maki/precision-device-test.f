\ maki/precision-device-test.f - the Orin device leg for GATE-LICENSED precision
\ (CAD-PLAN 8.1 lever 5 / step 3a - the tensor-core MMA prerequisite).
\
\ Precision is a per-region-class licensed fact, never a global flag: requesting
\ PREC-TF32 for the matmul class (maki/precision.f PREC!) makes the golden judge
\ under the tf32 tolerance row (atol 1e-6, rtol 2e-3) and the LICENSE is the passing
\ verdict itself. The REAL TF32 tensor-core kernel now exists (lib/ptx/cg-mma.f
\ mma.sync m16n8k8, dispatched by maki/lower-mm.f LMM-MMA? when the matmul class is
\ licensed at TF32), so this test licenses THAT kernel, not the f32 one under the band:
\   (1) request tf32 for CLASS-MATMUL -> LMM-EMIT emits the mma.sync kernel ->
\       LOWER-GOLDEN V-PASS under the tf32 row AND the reason + LG-PREC-USED@ name tf32
\       (the passing verdict is the running license for the tensor-core path);
\   (2) the cad.f gate path on the same model: PROMOTE's evidence row reads
\       golden=device-pass:tf32 (the recorded license);
\   (3) the INVERSE guard: a seeded PTX fault (maki/ablate-ptx.f ABL-MUTATE scales
\       the first output store by 1.005 = 0.5% relative error - in-bounds,
\       every output cell still written) must FAIL even under the tf32 band
\       (0.5% > rtol 0.2%), proving tf32 licensing is a tolerance, not a bypass;
\   (4) PREC-RESET restores f32: LMM-EMIT re-emits the f32 blocked kernel and it
\       passes again under the f32 row and the reason names f32.
\ The kernel is emitted IN-PROCESS per precision request (PTX-CAPTURE + LMM-EMIT), so
\ the dispatch sees the parent's PREC! - no child re-build that would drop the request.
\
\ Off the Orin (no libcuda) every leg is SKIPPED and the host build still loads.
\ NOT part of the maki gate (needs CUDA toolkit + device). Run on the Orin: scp to
\ zed:Work/habu then `bin/hb --load maki/precision-device-test.f`.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/fs.f
require lib/fs-mutate.f
require lib/float.f
require lib/fmt.f
require lib/ptx/toolchain.f
require src/arch/ptx/emit.f
require maki/device-artifacts.f
require maki/cad.f
require maki/lower-mm.f
require maki/lower-golden.f
require maki/precision.f
require maki/ablate-ptx.f

package MAKI

create PDT-OUT $10000 allot  create PDT-ERR $1000 allot   \ blocked-tile PTX is ~28 KB
create PDT-QO  $1000  allot  create PDT-QE  $2000 allot
variable PDT-PTX-U                                         \ correct-module text length (in PDT-OUT)

\ the seeded fault: scale the FIRST micro-tile store's accumulator by 1.005 (0f3F80A3D7),
\ a 0.5% relative error on those output cells - beyond tf32 rtol 2e-3, in-bounds, all cells
\ still written, so it must surface as a value-mismatch V-FAIL.
: PDT-TGT$ ( -- ptr u8 n )  s" st.global.f32 [%rd11], %f10;" ;
: PDT-REP$ ( -- ptr u8 n )  s" mul.f32 %f10, %f10, 0f3F80A3D7; st.global.f32 [%rd11], %f10;" ;

\ ---- emit region 0's kernel IN-PROCESS under the CURRENTLY-requested precision ----------
\ In-process (not a spawned child) so LMM-EMIT's LMM-MMA? dispatch sees the parent's PREC!
\ request: tf32 -> the mma.sync kernel, f32 -> the blocked fma kernel. Captured to PDT-OUT.
: PDT-EMIT-INPROC ( -- )
   PTX-CAPTURE-ON  0 LMM-EMIT  PTX-CAPTURE-OFF
   PTX-CAPTURE$ {: ca:ptr cu:n :}
   ca PDT-OUT cu BYTE-COPY  cu PDT-PTX-U ! ;
: PDT-PTX$ ( -- ptr u8 n )  PDT-OUT PDT-PTX-U @ ;

\ ---- write a PTX module -> ptxas -> register the cubin for the golden launch ----------
: PDT-ASSEMBLE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   PTXTC:PTX$ pa pu WRITE-ALL
   PDT-QO $1000 >LEN PDT-QE $2000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT 0 T=
   PTXTC:CUBIN$ LLA-CUBIN! ;

\ ---- one golden run: reason printed verbatim, verdict + judged precision asserted -----
: PDT-GOLDEN ( n n -- ) {: want:n prec:n :}
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   v want T=
   LG-PREC-USED@ prec T= ;

\ ============ (1) request tf32 -> LMM emits the mma.sync kernel; golden passes as tf32 ====
: PDT-TF32-PASS ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (1) tf32 requested for CLASS-MATMUL: mma.sync tensor-core kernel judged tf32" type cr
   PREC-TF32 CLASS-MATMUL PREC!
   PDT-EMIT-INPROC                                 \ LMM-MMA? true -> the mma.sync kernel
   PDT-PTX$ PDT-ASSEMBLE
   V-PASS PREC-TF32 PDT-GOLDEN
   LOWER-GOLDEN-REASON$ s" within tf32 tol" CONTAINS? TTRUE ;

\ ============ (2) PROMOTE records the license: golden=device-pass:tf32 =================
: PDT-EVIDENCE ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (2) PROMOTE under the tf32 request: evidence row records the license" type cr
   MDL-CUBINS-RESET  PTXTC:CUBIN$ 0 MDL-CUBIN!    \ region 0 = the CORRECT cubin
   STORE-RESET
   PROMOTE drop
   0 SK-KEY$ EVID-GET {: ra:ptr ru:n found:bool :}
   found TTRUE
   ra ru type cr                                  \ verbatim evidence row
   ra ru s" golden=device-pass:tf32" CONTAINS? TTRUE
   STORE-RESET ;

\ ============ (3) inverse guard: a 0.5% seeded fault fails EVEN under tf32 =============
: PDT-INVERSE ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (3) inverse: first micro-tile store scaled by 1.005 (0.5% > tf32 rtol 0.2%)" type cr
   PDT-PTX$ PDT-TGT$ PDT-REP$ ABL-MUTATE
   ABL-PTX$ PDT-ASSEMBLE
   V-FAIL PREC-TF32 PDT-GOLDEN
   LOWER-GOLDEN-REASON$ s" beyond tf32 tol" CONTAINS? TTRUE ;

\ ============ (4) PREC-RESET restores the f32 row + naming =============================
: PDT-RESET ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (4) PREC-RESET: the f32 blocked kernel re-emits and passes under the f32 row" type cr
   PREC-RESET
   CLASS-MATMUL PREC@ PREC-F32 T=
   PDT-EMIT-INPROC                                 \ LMM-MMA? false -> the f32 blocked kernel
   PDT-PTX$ PDT-ASSEMBLE
   V-PASS PREC-F32 PDT-GOLDEN
   LOWER-GOLDEN-REASON$ s" within f32 tol" CONTAINS? TTRUE ;

\ ---- off-device SKIP scaffolding (device-smoke pattern) --------------------------------
: PDT-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" precision-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-pdt-drv" MAKI-GRADE:PREPARE
   s" habu-pdt-ptx" PTXTC:PREPARE ;
: PDT-END ( -- )
   PREC-RESET                                     \ never leak a requested precision
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

;package

package MAKI
PDT-BEGIN

s" == GATE-LICENSED PRECISION: blocked MATMUL 64x64 (tf32 licenses the mma.sync kernel) ==" type cr
MODEL: PMB ( x:64x64 w:64x64 -- y ) MATMUL ;  FP-BUILD

PDT-TF32-PASS
PDT-EVIDENCE
PDT-INVERSE
PDT-RESET

PDT-END
;package

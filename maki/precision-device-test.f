\ maki/precision-device-test.f - the Orin device leg for GATE-LICENSED precision
\ (CAD-PLAN 8.1 lever 5 / step 3a - the tensor-core MMA prerequisite).
\
\ Precision is a per-region-class licensed fact, never a global flag: requesting
\ PREC-TF32 for the matmul class (maki/precision.f PREC!) makes the golden judge
\ under the tf32 tolerance row (atol 1e-6, rtol 2e-3) and the LICENSE is the passing
\ verdict itself. No TF32 kernel exists yet (the MMA lane emits it), so this test
\ proves the MECHANISM with the existing f32 blocked GEMM:
\   (1) request tf32 for CLASS-MATMUL -> LOWER-GOLDEN V-PASS (an f32 kernel is
\       trivially inside the tf32 band) AND the reason + LG-PREC-USED@ name tf32;
\   (2) the cad.f gate path on the same model: PROMOTE's evidence row reads
\       golden=device-pass:tf32 (the recorded license);
\   (3) the INVERSE guard: a seeded PTX fault (maki/ablate-ptx.f ABL-MUTATE scales
\       the first micro-tile store by 1.005 = 0.5% relative error - in-bounds,
\       every output cell still written) must FAIL even under the tf32 band
\       (0.5% > rtol 0.2%), proving tf32 licensing is a tolerance, not a bypass;
\   (4) PREC-RESET restores f32: the correct kernel passes again under the f32 row
\       and the reason names f32.
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
require maki/device-artifacts.f
require maki/cad.f
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

\ ---- spawn bin/hb to emit region 0's blocked-GEMM PTX (child re-builds the IR) --------
: PDT-EMIT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su  s" require maki/lower-mm.f"  s" LMM-EMIT"  0  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  PDT-OUT $10000 >LEN  PDT-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   PDT-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ surface stderr + throw on a nonzero child
   outu LEN>N PDT-PTX-U ! ;
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

: PDT-BUILD ( ptr u8 n -- ) {: ma:ptr mu:n :}    \ emit the CORRECT module once (kept in PDT-OUT)
   CUDA:OPEN? 0= if exit then
   ma mu PDT-EMIT ;

\ ============ (1) request tf32 -> the f32 kernel passes AND the verdict names tf32 =====
: PDT-TF32-PASS ( -- )
   CUDA:OPEN? 0= if exit then
   s"  (1) tf32 requested for CLASS-MATMUL: f32 blocked GEMM inside the tf32 band" type cr
   PREC-TF32 CLASS-MATMUL PREC!
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
   s"  (4) PREC-RESET: the correct kernel passes again under the f32 row" type cr
   PREC-RESET
   CLASS-MATMUL PREC@ PREC-F32 T=
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

end-package

package MAKI
PDT-BEGIN

s" == GATE-LICENSED PRECISION: blocked MATMUL 64x64 (tf32 request over the f32 kernel) ==" type cr
MODEL: PMB ( x:64x64 w:64x64 -- y ) MATMUL ;  FP-BUILD
s" MODEL: PMB ( x:64x64 w:64x64 -- y ) MATMUL ;" PDT-BUILD

PDT-TF32-PASS
PDT-EVIDENCE
PDT-INVERSE
PDT-RESET

PDT-END
end-package

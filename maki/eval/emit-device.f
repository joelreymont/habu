\ maki/eval/emit-device.f - the DEVICE NUMERIC golden for the sumnorm / gemm /
\ attention authoring tasks: GRADE = run the EMITTED kernel on the Orin and
\ compare its output to a CPU reference on deterministic exact-binary-fraction
\ inputs. This closes the wrong-but-green class that maki/eval/emit.f's STRUCTURAL
\ gates cannot see (dot habu-eval-device-numeric-c2e98ec4).
\
\ The five pinned wrong-but-green candidates (maki/eval/emit-test.f) all certify
\ AND emit the required PTX, so the required/forbidden structural gates grade them
\ GREEN(2). Only running the emitted kernel distinguishes them. What the device
\ golden actually finds, per shape (ground truth, measured on-device):
\
\  - sumnorm in/out swap  : the emitted PTX reads p_out and writes p_in (the
\    collective scaffold THREADS the matrix register identity through ROW-SPAN),
\    so p_out is untouched -> NUMERIC DIVERGENCE from the reference. CAUGHT.
\  - sumnorm div-by-sum^2 : emits an extra div.rn.f32 -> out = in/s^2, not in/s
\    -> NUMERIC DIVERGENCE. CAUGHT.
\  - gemm double-accumulate: MM-K-LOOP twice emits the pipeline (and its $KLOOP/
\    $KEND/$NOPF/$PFDONE labels) TWICE -> ptxas rejects the duplicate labels.
\    CAUGHT at assembly (rc != 0); it never produces a runnable cubin.
\  - attention Q/K swap    : NUMERIC DIVERGENCE. The attention scaffold now THREADS
\    each operand's pointer register through the phase pipeline (ATTN-PACK /
\    ATTN-QREG..ATTN-OREG in lib/ptx/cg-attention.f, dot
\    habu-attention-scaffold-erases-e03f933b), so permuting Q/K/V/O before ATTN:START
\    emits genuinely swapped loads: STAGE-Q stages from K's pointer and SCORE reads
\    Q's, the scores become Q-dependent, softmax is no longer uniform, and the device
\    output diverges from the reference. CAUGHT.
\  - attention output-into-V: the store lands in V's pointer and the O-role load reads
\    the zero-initialized O buffer, so the O output buffer keeps its sentinel -> NUMERIC
\    DIVERGENCE. CAUGHT.
\
\ So the device golden closes the class wherever wrongness reaches codegen: all four
\ numeric shapes (the two sumnorm, the two attention) diverge, and the gemm shape is
\ rejected at ptxas. The former attention operand-role erasure is fixed at the
\ scaffold, so the role swaps are now real wrong kernels, not codegen no-ops.
\
\ MECHANISM (per shape, like maki/eval/device.f): write a driver that defines the
\ candidate kernel K + the task scaffold, spawn a bin/hb CHILD to emit its PTX
\ (untrusted generated source never mutates this dictionary), ptxas-assemble to a
\ private per-run cubin, load + launch on the Orin, copy the output back, and take
\ max|err| vs a host reference. The whole device leg is keyed on the device-FFI
\ probe (ON-DEVICE?, the maki/device-smoke.f pattern), so off-device the suite is
\ a recorded SKIP and this file still check-loads. This file OWNS the canonical
\ wrong-but-green fixture strings; maki/eval/emit-test.f references them so the
\ structural pins and the device golden grade the EXACT same candidates.
\ Fully checked Habu; no 0 set-check. Load after maki/cuda-driver.f.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/ffi-abi.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require maki/cuda-driver.f
require maki/cuda-run.f
require maki/device-artifacts.f
require maki/eval/active-target.f

package EVND

private

\ ---- device-golden problem sizes (exact-binary-fraction inputs) -------------
2 constant SN-ROWS   4 constant SN-COLS
SN-ROWS SN-COLS * constant SN-ELEMS
SN-ELEMS 4 * constant SN-BYTES
20 constant SN-PARAM-BYTES                     \ p_in(8) p_out(8) p_k(4)

64 constant GEMM-M   64 constant GEMM-N   32 constant GEMM-K
GEMM-M GEMM-K * constant GM-A-ELEMS
GEMM-K GEMM-N * constant GM-B-ELEMS
GEMM-M GEMM-N * constant GM-C-ELEMS
GM-A-ELEMS 4 * constant GM-A-BYTES
GM-B-ELEMS 4 * constant GM-B-BYTES
GM-C-ELEMS 4 * constant GM-C-BYTES
GEMM-N 64 / constant GM-GRID-N                 \ gridDim.x = N/64 (col tiles)
GEMM-M 64 / constant GM-GRID-M                 \ gridDim.y = M/64 (row tiles)
36 constant GM-PARAM-BYTES                     \ pA,pB,pC(8) pM,pN,pK(4)

4 constant ATTN-N    2 constant ATTN-D
ATTN-N ATTN-D * constant AT-ELEMS
AT-ELEMS 4 * constant AT-BYTES
40 constant AT-PARAM-BYTES                     \ pQ,pK,pV,pO(8) pN,pD(4)

$10000 constant CH-OUT-CAP                      \ emit child stdout (gemm double PTX ~53 KB)
$1000  constant CH-ERR-CAP
$1000  constant PA-OUT-CAP
$2000  constant PA-ERR-CAP
20000  constant EMIT-TIMEOUT-MS
10000  constant PTXAS-TIMEOUT-MS

\ ---- host staging buffers ---------------------------------------------------
create SN-IN  SN-BYTES allot   create SN-OUT SN-BYTES allot
create GEMM-A GM-A-BYTES allot  create GEMM-B GM-B-BYTES allot  create GEMM-C GM-C-BYTES allot
create ATTN-Q AT-BYTES allot  create ATTN-K AT-BYTES allot
create ATTN-V AT-BYTES allot  create ATTN-O AT-BYTES allot

create CH-OUT CH-OUT-CAP allot  create CH-ERR CH-ERR-CAP allot
create PA-OUT PA-OUT-CAP allot  create PA-ERR PA-ERR-CAP allot

\ ---- little-endian f32 host pack/unpack -------------------------------------
: F32! ( n ptr a n -- ) {: v:n buf:ptr idx:n :}
   idx 4 * {: o:n :}
   v           $FF and  buf o     + c!
   v 8 rshift  $FF and  buf o 1 + + c!
   v 16 rshift $FF and  buf o 2 + + c!
   v 24 rshift $FF and  buf o 3 + + c! ;

: F32@ ( ptr a n -- n ) {: buf:ptr idx:n :}
   idx 4 * {: o:n :}
   buf o     + c@
   buf o 1 + + c@  8  lshift or
   buf o 2 + + c@  16 lshift or
   buf o 3 + + c@  24 lshift or ;

: F! ( r ptr a n -- ) {: v:r buf:ptr idx:n :}    \ store a Habu float as f32 at element idx
   v F64>F32 buf idx F32! ;

: MAXF ( r r -- r ) {: a:r b:r :}  a b f> if a else b then ;

\ ---- driver + child emit + ptxas (shared across the three tasks) ------------
: WRITE-DRIVER ( ptr u8 n ptr u8 n ptr u8 n -- ) {: ba:ptr bu:n ca:ptr cu:n sa:ptr su:n :}
   SB-RESET
   s" PTX-TARGET! " SB-APPEND  ATGT:LABEL$ SB-APPEND  32 SB-APPEND-C  ATGT:VER$ SB-APPEND  10 SB-APPEND-C
   ba bu SB-APPEND  10 SB-APPEND-C
   s" : " SB-APPEND  ca cu SB-APPEND  s"  ;" SB-APPEND  10 SB-APPEND-C
   sa su SB-APPEND  10 SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

: ARGV-COMMON ( -- )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+ ;

: SN-ARGV ( -- )
   ARGV-COMMON
   s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/collective.f"    >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$          >LEN PROC-ARGV+ ;

: GEMM-ARGV ( -- )
   ARGV-COMMON
   s" lib/ptx/cg-matmul.f" >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$      >LEN PROC-ARGV+ ;

: ATTN-ARGV ( -- )
   ARGV-COMMON
   s" lib/ptx/cg-attention.f" >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$         >LEN PROC-ARGV+ ;

\ spawn the resolved engine over the built argv; capture the emitted PTX to MAKI-GRADE:PTX$
: EMIT-CHILD ( -- )
   ENGINE-CANDIDATE:PATH$ >LEN  CH-OUT CH-OUT-CAP >LEN  CH-ERR CH-ERR-CAP >LEN  EMIT-TIMEOUT-MS >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF          \ clean exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                  \ nonzero child: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   rc RC>N 0 <> outu LEN>N 0= or if E-EVAL-EMIT throw then
   MAKI-GRADE:PTX$ CH-OUT outu LEN>N WRITE-ALL ;

\ ptxas-assemble MAKI-GRADE:PTX$ -> MAKI-GRADE:CUBIN$; return ptxas rc (NOT
\ fail-closed: a nonzero rc is the wrong-shape signal for gemm double-accumulate)
: PTXAS ( -- n )
   PROC-ARGV-RESET
   SB-RESET s" -arch=" SB-APPEND ATGT:LABEL$ SB-APPEND SB$  >LEN PROC-ARGV+
   MAKI-GRADE:PTX$    >LEN PROC-ARGV+
   s" -o"             >LEN PROC-ARGV+
   MAKI-GRADE:CUBIN$  >LEN PROC-ARGV+
   MAKI-GRADE:PTXAS$  >LEN  PA-OUT PA-OUT-CAP >LEN  PA-ERR PA-ERR-CAP >LEN  PTXAS-TIMEOUT-MS >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 0 ENDOF                    \ ptxas clean -> rc 0
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} c RC>N ENDOF
   ;MATCH ;

: SN-BUILD ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-evnd-sumnorm" MAKI-GRADE:PREPARE
   s" 256 %BLOCK"  a u
   s" CG-SM-RESET CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS 1 MATRIX-REG 2 MATRIX-REG K CG-SM-RET CG-SM-CLOSE"
   WRITE-DRIVER  SN-ARGV EMIT-CHILD  PTXAS ;

: GEMM-BUILD ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-evnd-gemm" MAKI-GRADE:PREPARE
   s" 256 %BLOCK"  a u  s" MM-AUTHOR-OPEN K MM-AUTHOR-CLOSE"
   WRITE-DRIVER  GEMM-ARGV EMIT-CHILD  PTXAS ;

: ATTN-BUILD ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-evnd-attn" MAKI-GRADE:PREPARE
   s" 128 %BLOCK"  a u  s" ATTN:AUTHOR-OPEN K ATTN:AUTHOR-CLOSE"
   WRITE-DRIVER  ATTN-ARGV EMIT-CHILD  PTXAS ;

\ ---- CUDA context + module load/unload (shared) -----------------------------
create EVND-PATH FS-PATH-CAP allot  create EVND-KN 32 allot
variable EVND-DEV variable EVND-CTX variable EVND-MOD variable EVND-FUNC

\ CU-BEGIN/CU-LOAD acquire the primary context, module, and (below) the device
\ buffers into the enclosing CUDA-SCOPE frame; each RUN-* runs its body under a
\ single SCOPE (RUN-*-CORE), so the module, primary context, and every allocation
\ unwind in reverse on both return and throw - no open-coded CU-END release list.
: CU-BEGIN ( -- )
   MKD:OPEN
   0 MKD:CUINIT CUDA:RC0
   EVND-DEV 0 >IDX MKD:CUDEVICEGET CUDA:RC0
   EVND-CTX EVND-DEV @ >CUDA-DEV MKD:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   EVND-DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   EVND-CTX @ >CUDA-CTX MKD:CUCTXSETCURRENT CUDA:RC0 ;

: CU-LOAD ( ptr u8 n -- ) {: na:ptr nu:n :}    \ load CUBIN$ (owned), resolve function <name> into EVND-FUNC
   MAKI-GRADE:CUBIN$ EVND-PATH FFI:CSTR
   EVND-MOD EVND-PATH MKD:CUMODULELOAD CUDA:RC0
   EVND-MOD @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   na nu EVND-KN FFI:CSTR
   EVND-FUNC EVND-MOD @ >CUDA-MOD EVND-KN MKD:CUMODULEGETFUNCTION CUDA:RC0 ;

\ ---- sumnorm device run: SOFTMAX_ROWS, grid = R blocks, block = 256 ----------
variable SN-DIN variable SN-DOUT variable SN-KV

: RUN-SUMNORM-CORE ( -- )                       \ acquire+own ctx/module/buffers, launch, readback -> SN-OUT
   CU-BEGIN
   s" SOFTMAX_ROWS" CU-LOAD
   SN-DIN SN-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0   SN-DIN @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   SN-DOUT SN-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  SN-DOUT @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   SN-DIN @ >CUDA-DEVPTR SN-IN SN-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   SN-DOUT @ >CUDA-DEVPTR 0 SN-ELEMS >COUNT MKD:CUMEMSETD32 CUDA:RC0   \ sentinel 0: an unwritten output stays 0
   SN-COLS SN-KV !
   EVND-FUNC @ >CUDA-FN 256 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   EVND-FUNC @ >CUDA-FN SN-PARAM-BYTES >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   EVND-FUNC @ >CUDA-FN 0 >IDX  SN-DIN 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 8 >IDX  SN-DOUT 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 16 >IDX SN-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN SN-ROWS 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   SN-OUT SN-DOUT @ >CUDA-DEVPTR SN-BYTES >LEN MKD:CUMEMCPYDTOH CUDA:RC0 ;
: RUN-SUMNORM ( -- )  [: RUN-SUMNORM-CORE ;] CUDA-SCOPE:SCOPE ;   \ buffers+module+ctx released on return/throw

\ ---- gemm device run: MM, block 16x16, grid (N/64, M/64) --------------------
variable GM-DA variable GM-DB variable GM-DC
variable GM-MV variable GM-NV variable GM-KV

: RUN-GEMM-CORE ( -- )                          \ acquire+own ctx/module/buffers, launch, readback -> GEMM-C
   CU-BEGIN
   s" MM" CU-LOAD
   GM-DA GM-A-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  GM-DA @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   GM-DB GM-B-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  GM-DB @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   GM-DC GM-C-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  GM-DC @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   GM-DA @ >CUDA-DEVPTR GEMM-A GM-A-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   GM-DB @ >CUDA-DEVPTR GEMM-B GM-B-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   GM-DC @ >CUDA-DEVPTR 0 GM-C-ELEMS >COUNT MKD:CUMEMSETD32 CUDA:RC0
   GEMM-M GM-MV !  GEMM-N GM-NV !  GEMM-K GM-KV !
   EVND-FUNC @ >CUDA-FN 16 16 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   EVND-FUNC @ >CUDA-FN GM-PARAM-BYTES >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   EVND-FUNC @ >CUDA-FN 0 >IDX  GM-DA 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 8 >IDX  GM-DB 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 16 >IDX GM-DC 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 24 >IDX GM-MV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 28 >IDX GM-NV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 32 >IDX GM-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN GM-GRID-N GM-GRID-M CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   GEMM-C GM-DC @ >CUDA-DEVPTR GM-C-BYTES >LEN MKD:CUMEMCPYDTOH CUDA:RC0 ;
: RUN-GEMM ( -- )  [: RUN-GEMM-CORE ;] CUDA-SCOPE:SCOPE ;

\ ---- attention device run: ATTN, block = N threads, grid = N blocks ---------
variable AT-DQ variable AT-DK variable AT-DV variable AT-DO
variable AT-NV variable AT-DPARAM

: RUN-ATTN-CORE ( -- )                          \ acquire+own ctx/module/buffers, launch, readback -> ATTN-O
   CU-BEGIN
   s" ATTN" CU-LOAD
   AT-DQ AT-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  AT-DQ @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   AT-DK AT-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  AT-DK @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   AT-DV AT-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  AT-DV @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   AT-DO AT-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0  AT-DO @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   AT-DQ @ >CUDA-DEVPTR ATTN-Q AT-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   AT-DK @ >CUDA-DEVPTR ATTN-K AT-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   AT-DV @ >CUDA-DEVPTR ATTN-V AT-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   AT-DO @ >CUDA-DEVPTR 0 AT-ELEMS >COUNT MKD:CUMEMSETD32 CUDA:RC0
   ATTN-N AT-NV !  ATTN-D AT-DPARAM !
   EVND-FUNC @ >CUDA-FN ATTN-N 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   EVND-FUNC @ >CUDA-FN AT-PARAM-BYTES >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   EVND-FUNC @ >CUDA-FN 0 >IDX  AT-DQ 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 8 >IDX  AT-DK 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 16 >IDX AT-DV 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 24 >IDX AT-DO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 32 >IDX AT-NV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN 36 >IDX AT-DPARAM 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   EVND-FUNC @ >CUDA-FN ATTN-N 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   ATTN-O AT-DO @ >CUDA-DEVPTR AT-BYTES >LEN MKD:CUMEMCPYDTOH CUDA:RC0 ;
: RUN-ATTN ( -- )  [: RUN-ATTN-CORE ;] CUDA-SCOPE:SCOPE ;

\ ---- reference inputs + CPU references (exact binary fractions) --------------
\ sumnorm: out[r][c] = in[r][c] / sum(in[r]).  row0 sum=2, row1 sum=4.
: SN-INVAL ( n n -- r ) {: r:n c:n :}
   r 0 = if
      c 0 = if 0.5  exit then  c 1 = if 0.25 exit then  c 2 = if 1.0 exit then  0.25
   else
      c 0 = if 1.0  exit then  c 1 = if 1.0  exit then  c 2 = if 0.5 exit then  1.5
   then ;
: SN-ROWSUM ( n -- r )  0 = if 2.0 else 4.0 then ;
: SN-REFVAL ( n n -- r ) {: r:n c:n :}  r c SN-INVAL  r SN-ROWSUM f/ ;
: FILL-SN-IN ( -- )
   SN-ELEMS 0 ?do  i SN-COLS /mod swap SN-INVAL  SN-IN i F!  loop ;
: SN-MAXERR ( -- r )
   0.0  SN-ELEMS 0 ?do
      SN-OUT i F32@ F32>F64   i SN-COLS /mod swap SN-REFVAL  f- fabs  MAXF
   loop ;

\ gemm: A[i][k]=1, B[k][j]=((j&3)+1)*0.25, so C[i][j] = 32*B[0][j] = 8*((j&3)+1).
: GEMM-BVAL ( n -- r )
   3 and dup 0 = if drop 0.25 exit then
   dup 1 = if drop 0.5  exit then
   dup 2 = if drop 0.75 exit then  drop 1.0 ;
: GEMM-REFVAL ( n -- r )  GEMM-BVAL 32.0 f* ;
: FILL-GEMM-A ( -- )  GM-A-ELEMS 0 ?do  1.0 GEMM-A i F!  loop ;
: FILL-GEMM-B ( -- )  GM-B-ELEMS 0 ?do  i GEMM-N mod GEMM-BVAL  GEMM-B i F!  loop ;
: GEMM-MAXERR ( -- r )
   0.0  GM-C-ELEMS 0 ?do
      GEMM-C i F32@ F32>F64   i GEMM-N mod GEMM-REFVAL  f- fabs  MAXF
   loop ;

\ attention: K rows all equal -> scores equal per query -> softmax uniform, so
\ O[q] = colmean(V) = [1.0, 1.5] for every q (exp precision drops out: exp(0)=1).
: ATTN-QVAL ( n n -- r ) {: q:n d:n :}
   d 0 = if
      q 0 = if 0.25 exit then  q 1 = if 0.5 exit then  q 2 = if 0.75 exit then  1.0
   else 0.5 then ;
: ATTN-KVAL ( n n -- r ) {: j:n d:n :}  d 0 = if 1.0 else 0.5 then ;
: ATTN-VVAL ( n n -- r ) {: j:n d:n :}
   d 0 = if
      j 0 = if 1.0 exit then  j 1 = if 0.5 exit then  j 2 = if 2.0 exit then  0.5
   else
      j 0 = if 3.0 exit then  j 1 = if 1.0 exit then  j 2 = if 0.5 exit then  1.5
   then ;
: ATTN-REFVAL ( n n -- r ) {: q:n d:n :}  d 0 = if 1.0 else 1.5 then ;
: FILL-ATTN-Q ( -- )  AT-ELEMS 0 ?do  i ATTN-D /mod swap ATTN-QVAL  ATTN-Q i F!  loop ;
: FILL-ATTN-K ( -- )  AT-ELEMS 0 ?do  i ATTN-D /mod swap ATTN-KVAL  ATTN-K i F!  loop ;
: FILL-ATTN-V ( -- )  AT-ELEMS 0 ?do  i ATTN-D /mod swap ATTN-VVAL  ATTN-V i F!  loop ;
: ATTN-MAXERR ( -- r )
   0.0  AT-ELEMS 0 ?do
      ATTN-O i F32@ F32>F64   i ATTN-D /mod swap ATTN-REFVAL  f- fabs  MAXF
   loop ;

\ ---- build + run + compare per task -----------------------------------------
: SN-RUN ( ptr u8 n -- r ) {: a:ptr u:n :}
   a u SN-BUILD {: rc:n :}
   rc 0 <> if MAKI-GRADE:CLEAN E-EVAL-EMIT throw then
   FILL-SN-IN  RUN-SUMNORM  MAKI-GRADE:CLEAN  SN-MAXERR ;

: GEMM-RUN ( ptr u8 n -- r ) {: a:ptr u:n :}
   a u GEMM-BUILD {: rc:n :}
   rc 0 <> if MAKI-GRADE:CLEAN E-EVAL-EMIT throw then
   FILL-GEMM-A  FILL-GEMM-B  RUN-GEMM  MAKI-GRADE:CLEAN  GEMM-MAXERR ;

: GEMM-ASM-RC ( ptr u8 n -- n ) {: a:ptr u:n :}    \ build only; the ptxas rc is the verdict
   a u GEMM-BUILD {: rc:n :}
   MAKI-GRADE:CLEAN  rc ;

: ATTN-RUN ( ptr u8 n -- r ) {: a:ptr u:n :}
   a u ATTN-BUILD {: rc:n :}
   rc 0 <> if MAKI-GRADE:CLEAN E-EVAL-EMIT throw then
   FILL-ATTN-Q  FILL-ATTN-K  FILL-ATTN-V  RUN-ATTN  MAKI-GRADE:CLEAN  ATTN-MAXERR ;

\ ---- evidence printing ------------------------------------------------------
: REPORT-ERR ( ptr u8 n r -- ) {: a:ptr u:n e:r :}
   s" eval-device " type  a u type  s" : max|err| vs CPU ref = " type  e 6 FMT:F.N cr ;
: REPORT-RC ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   s" eval-device " type  a u type  s" : ptxas rc = " type  rc .  s" (nonzero -> assembly rejected)" type cr ;

public

\ tolerance: divergences are >= ~0.09, matches are 0 (exact inputs); 1/1024 splits them wide
: TOL ( -- r )  0.0009765625 ;

\ device-FFI probe (maki/device-smoke.f pattern): libcuda present AND cuInit ok
: ON-DEVICE? ( -- bool )
   CUDA:OPEN? 0= if false exit then
   0 CUDA:CUINIT RC>N 0= ;

\ ---- canonical wrong-but-green fixtures (also referenced by eval-emit-test.f) ----
: SN-GREEN$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-SUM {: s :} x s PTX:B/ out r ROW-SPAN c ROW-STORE" ;
: SN-SWAP$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: out in :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-SUM {: s :} x s PTX:B/ out r ROW-SPAN c ROW-STORE" ;
: SN-SQSUM$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-SUM {: s :} x s PTX:B/ s PTX:B/ out r ROW-SPAN c ROW-STORE" ;
: GEMM-GREEN$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-K-LOOP MM-STORE" ;
: GEMM-DOUBLE$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-K-LOOP MM-K-LOOP MM-STORE" ;
: ATTN-GREEN$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH" ;
: ATTN-QK$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) {: q k v o :} k q v o ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH" ;
: ATTN-OV$ ( -- ptr u8 n )
   s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) {: q k v o :} q k o v ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH" ;

\ ---- per-shape device verdicts (print measured evidence; return the assertion bool) ----
: SN-GREEN-OK? ( -- bool )
   SN-GREEN$ SN-RUN {: e:r :}  s" sumnorm correct" e REPORT-ERR  e TOL f< ;
: SN-SWAP-CAUGHT? ( -- bool )
   SN-SWAP$ SN-RUN {: e:r :}  s" sumnorm in/out-swap" e REPORT-ERR  e TOL f> ;
: SN-SQSUM-CAUGHT? ( -- bool )
   SN-SQSUM$ SN-RUN {: e:r :}  s" sumnorm div-by-sum^2" e REPORT-ERR  e TOL f> ;
: GEMM-GREEN-OK? ( -- bool )
   GEMM-GREEN$ GEMM-RUN {: e:r :}  s" gemm correct" e REPORT-ERR  e TOL f< ;
: GEMM-DOUBLE-REJECTED? ( -- bool )
   GEMM-DOUBLE$ GEMM-ASM-RC {: rc:n :}  s" gemm double-accumulate" rc REPORT-RC  rc 0 <> ;
: ATTN-GREEN-OK? ( -- bool )
   ATTN-GREEN$ ATTN-RUN {: e:r :}  s" attn correct" e REPORT-ERR  e TOL f< ;
: ATTN-QK-CAUGHT? ( -- bool )
   ATTN-QK$ ATTN-RUN {: e:r :}  s" attn Q/K-swap [emits swapped loads]" e REPORT-ERR  e TOL f> ;
: ATTN-OV-CAUGHT? ( -- bool )
   ATTN-OV$ ATTN-RUN {: e:r :}  s" attn output-into-V [stores into V ptr]" e REPORT-ERR  e TOL f> ;

;package

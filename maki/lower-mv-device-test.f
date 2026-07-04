\ maki/lower-mv-device-test.f - the Orin device leg for the movement lowering (slice 4).
\
\ CAD-PLAN 6.3 device-vs-host GOLDEN. For each movement model it builds the model, fuses it,
\ and lowers region 0: spawn a fresh bin/hb (via LOWER-DRIVER!) to EMIT the kernel to
\ PTXTC:PTX$, EMIT-GUARD, ptxas ASSEMBLE + ASM-REPORT, then LOWER-GOLDEN (maki/lower-golden.f)
\ runs the host executor and the device kernel on the same synthetic inputs and compares. Two
\ legs: (B) MATERIALIZED movement copy kernels (TRANSPOSE via a multi-use fan-out that
\ materializes it, unaligned SLICE, GATHER) route to LMV-RUN under the exact copy tolerance;
\ (A) DISSOLVED free movement folded into a compute kernel (SLICE:0..2 GELU into the flat EW
\ kernel, SLICE RMSNORM into the row kernel, SLICE MATMUL into the K-loop) route to the EW/RED/MM
\ launch with the movement offset baked into the base pointer. Off the Orin (no libcuda) the
\ device leg is reported SKIPPED and the host build still runs.
\
\ Each MODEL: line runs at load time; LMD-GOLD1 lowers + launches whatever IR is current, so the
\ same model text feeds the host reference, the launch staging, and the spawned child that emits
\ the kernel PTX. Not part of the maki gate (maki/test.f) - it needs the CUDA toolkit + a device.
\ Run on the Orin: scp to zed:Work/habu then `bin/hb --load maki/lower-mv-device-test.f`.

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

package MAKI

create LMD-OUT $4000 allot  create LMD-ERR $1000 allot
create LMD-QO  $1000 allot  create LMD-QE  $2000 allot

\ ---- spawn bin/hb to emit region 0's PTX into PTXTC:PTX$ (child re-builds the IR) --
\ require/emit vary per lowering leg (copy kernel vs dissolved EW/RED/MM fold).
: LMD-EMIT ( ptr u8 n ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ra:ptr ru:n ea:ptr eu:n :}
   sa su  ra ru  ea eu  0  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  LMD-OUT $4000 >LEN  LMD-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LMD-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ surface stderr + throw on a nonzero child
   PTXTC:PTX$ LMD-OUT outu LEN>N WRITE-ALL ;

: LMD-PTXAS ( -- n )  LMD-QO $1000 >LEN LMD-QE $2000 >LEN PTXTC:ASSEMBLE ;

\ ---- per-element evidence (device value | host value rounded to f32) --------
: LMD-FP ( r -- )  SB-RESET 6 SB-FIX SB$ type ;
: LMD-HOST@ ( n -- r )  LLA-OUT-NODE@ EX-OUT@ swap T-GET  F64>F32 F32>F64 ;   \ narrowed host elem
: LMD-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ LMD-FP  s"    " type  i LMD-HOST@ LMD-FP  cr
   loop ;

\ ---- one model: emit -> assemble -> golden -> evidence -> assert V-PASS ------
\ Skips (no emit/ptxas) off-device, so the file still loads where there is no CUDA.
: LMD-GOLD1 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ra:ptr ru:n ea:ptr eu:n :}
   CUDA:OPEN? 0= if exit then
   sa su ra ru ea eu LMD-EMIT
   LMD-PTXAS PTXTC:ASM-REPORT 0 T=                        \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LLA-CUBIN!
   0 LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LMD-EVIDENCE
   v V-PASS T= ;

\ leg-specific convenience: copy kernel vs dissolved EW/RED/MM fold
: LMD-COPY ( ptr u8 n -- )  s" require maki/lower-move.f" s" LMV-EMIT"  LMD-GOLD1 ;
: LMD-EW   ( ptr u8 n -- )  s" require maki/lower-ew.f"   s" LEW-EMIT"  LMD-GOLD1 ;
: LMD-RED  ( ptr u8 n -- )  s" require maki/lower-red.f"  s" LRED-EMIT" LMD-GOLD1 ;
: LMD-MM   ( ptr u8 n -- )  s" require maki/lower-mm.f"   s" LMM-EMIT"  LMD-GOLD1 ;

: LMD-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-mv-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-lmv-drv" MAKI-GRADE:PREPARE
   s" habu-lmv-ptx" PTXTC:PREPARE ;

: LMD-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

end-package

package MAKI
LMD-BEGIN

\ ============ (B) materialized movement copy kernels =========================

\ TRANSPOSE 4x8: a multi-use fan-out (H used twice) materializes the transpose as region 0,
\ so the device copy kernel runs and is compared against the host MOVE-TRANSPOSE reference.
s" == TRANSPOSE 4x8 (materialized copy) ==" type cr
MODEL: TP ( x:4x8 -- y ) TRANSPOSE >V H H RESIDUAL-ADD ;  FP-BUILD
s" MODEL: TP ( x:4x8 -- y ) TRANSPOSE >V H H RESIDUAL-ADD ;" LMD-COPY

\ SLICE 1..3 of 4x6: r0*cols=6 is lane-unaligned (verdict materialize), a standalone offset copy.
s" == SLICE:1..3 of 4x6 (materialized offset copy) ==" type cr
MODEL: SL ( x:4x6 -- y ) SLICE:1..3 ;  FP-BUILD
s" MODEL: SL ( x:4x6 -- y ) SLICE:1..3 ;" LMD-COPY

\ GATHER 8x8 by 4 rows: the f32 index input (synthetic 0s) selects rows; device rounds like the host.
s" == GATHER 8x8 by 4x1 (indexed rows) ==" type cr
MODEL: GA ( x:8x8 i:4x1 -- y ) GATHER ;  FP-BUILD
s" MODEL: GA ( x:8x8 i:4x1 -- y ) GATHER ;" LMD-COPY

\ ============ (A) dissolved free movement folded into index math ==============

\ SLICE:0..2 GELU: free slice folds into the flat EW kernel's load base (offset 0 here).
s" == SLICE:0..2 GELU (dissolved into EW) ==" type cr
MODEL: SG ( x:4x8 -- y ) SLICE:0..2 GELU ;  FP-BUILD
s" MODEL: SG ( x:4x8 -- y ) SLICE:0..2 GELU ;" LMD-EW

\ SLICE:1..3 RMSNORM: free slice (offset 8 elems) folds into the row kernel's span base.
s" == SLICE:1..3 RMSNORM (dissolved into RED) ==" type cr
MODEL: SR ( x:4x8 -- y ) SLICE:1..3 RMSNORM ;  FP-BUILD
s" MODEL: SR ( x:4x8 -- y ) SLICE:1..3 RMSNORM ;" LMD-RED

\ SLICE:2..6 MATMUL: free slice on A (offset 16 elems) folds into the K-loop's A base.
s" == SLICE:2..6 MATMUL 8x8@8x16 (dissolved into MM) ==" type cr
MODEL: SM ( x:8x8 w:8x16 -- y ) SLICE:2..6 MATMUL ;  FP-BUILD
s" MODEL: SM ( x:8x8 w:8x16 -- y ) SLICE:2..6 MATMUL ;" LMD-MM

LMD-END
end-package

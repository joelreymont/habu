\ fusion-compare.f - benchmark unfused v4 SAXPY+RELU against fused v4 RELU.
\
\ Self-contained: each kernel's cubin is SELF-EMITTED (tools/ptx/fusion-emit.f) to a
\ PRIVATE per-run toolchain root and ptxas-assembled fail-closed - no shared
\ /tmp/*.cubin that could be stale/missing/wrong. A missing producer or a nonzero
\ emit/ptxas rc fails CLOSED with the named E-PTX-EMIT throw, never an uncaught
\ E-CUDA from a stale path. The device launch/compare half is gated on CUDA:OPEN?
\ (recorded SKIP off-device); the emit half is proven host-side by
\ tools/ptx/fusion-emit-test.f.

require maki/eval/active-target.f
require tools/ptx/fusion-emit.f
require tools/ptx/bandwidth-lib.f

package PTXBW

: CONFIG-V4 ( -- )
   4 ELEMS-PER-THREAD! ;

: RUN-SAXPY-V4 ( -- n )
   s" habu-ptx-fusion-saxpy" s" tools/ptx/saxpy-v4-cg.f" PTXFE:BUILD-KERNEL
   DEFAULTS
   PTXTC:CUBIN$ CUBIN!                       \ load the private per-run cubin, not a shared /tmp path
   s" SAXPY-V4" LABEL!
   CONFIG-V4
   12 BYTES-PER-ELEM!
   2 FLOPS-PER-ELEM!
   MEASURE dup REPORT-NS ;

: RUN-RELU-V4 ( -- n )
   s" habu-ptx-fusion-relu" s" tools/ptx/relu-v4-cg.f" PTXFE:BUILD-KERNEL
   DEFAULTS
   PTXTC:CUBIN$ CUBIN!
   s" RELU-V4" LABEL!
   CONFIG-V4
   8 BYTES-PER-ELEM!
   1 FLOPS-PER-ELEM!
   MEASURE dup REPORT-NS ;

: RUN-FUSED-RELU-V4 ( -- n )
   s" habu-ptx-fusion-fused" s" tools/ptx/fused-relu-cg.f" PTXFE:BUILD-KERNEL
   DEFAULTS
   PTXTC:CUBIN$ CUBIN!
   s" FUSED-RELU-V4" LABEL!
   CONFIG-V4
   12 BYTES-PER-ELEM!
   3 FLOPS-PER-ELEM!
   MEASURE dup REPORT-NS ;

: REPORT-UNFUSED ( n n -- )
   {: sax:n relu:n :}
   BW-N @ BW-ITERS * 20 * {: by:n :}
   BW-N @ BW-ITERS * 3 * {: fl:n :}
   sax relu + {: ns:n :}
   s" kernel=UNFUSED-SAXPY-V4+RELU-V4" type
   s"  work_items=" type BW-N @ FMT:.U
   s"  kernels=2" type
   s"  iters=" type BW-ITERS FMT:.U cr
   s" gpu_elapsed_ns_sum=" type ns FMT:.U cr
   by fl ns PTXPROF:REPORT-METRICS ;

: REPORT-SPEEDUP ( n n -- )
   {: unfused:n fused:n :}
   s" fusion_elapsed_ratio_x1000=" type
   unfused 1000 * fused / FMT:.U cr ;

: MAIN ( -- )
   ATGT:LABEL$ PTXTC:TC-ARCH!                \ assembler arch from the probed active target; every BUILD-KERNEL below assembles, and PTXTC:PREPARE does not clear it
   RUN-SAXPY-V4 {: sax:n :}
   RUN-RELU-V4 {: relu:n :}
   sax relu REPORT-UNFUSED
   RUN-FUSED-RELU-V4 {: fused:n :}
   sax relu + fused REPORT-SPEEDUP
   PTXTC:CLEAN ;                             \ remove the last private per-run root

: RUN ( -- )   \ off-device the compare is a recorded SKIP; the emit half is proven by fusion-emit-test.f
   CUDA:OPEN? 0= if
      s" fusion-compare: libcuda.so.1 unavailable -> device compare SKIPPED (off-device)" type cr
      exit
   then
   MAIN ;

RUN

;package

\ fusion-compare.f - benchmark unfused v4 SAXPY+RELU against fused v4 RELU.
\
\ Prereq cubins:
\   /tmp/saxpy-v4.cubin       from tools/ptx/saxpy-v4-cg.f
\   /tmp/relu-v4.cubin        from tools/ptx/relu-v4-cg.f
\   /tmp/fused-relu-v4.cubin  from tools/ptx/fused-relu-cg.f

require tools/ptx/bandwidth-lib.f

package PTXBW

: CONFIG-V4 ( -- )
   4 ELEMS-PER-THREAD! ;

: RUN-SAXPY-V4 ( -- n )
   DEFAULTS
   s" /tmp/saxpy-v4.cubin" CUBIN!
   s" SAXPY-V4" LABEL!
   CONFIG-V4
   12 BYTES-PER-ELEM!
   2 FLOPS-PER-ELEM!
   MEASURE dup REPORT-NS ;

: RUN-RELU-V4 ( -- n )
   DEFAULTS
   s" /tmp/relu-v4.cubin" CUBIN!
   s" RELU-V4" LABEL!
   CONFIG-V4
   8 BYTES-PER-ELEM!
   1 FLOPS-PER-ELEM!
   MEASURE dup REPORT-NS ;

: RUN-FUSED-RELU-V4 ( -- n )
   DEFAULTS
   s" /tmp/fused-relu-v4.cubin" CUBIN!
   s" FUSED-RELU-V4" LABEL!
   CONFIG-V4
   12 BYTES-PER-ELEM!
   3 FLOPS-PER-ELEM!
   MEASURE dup REPORT-NS ;

: REPORT-UNFUSED ( n n -- )
   {: sax:n relu:n :}
   BW-N BW-ITERS * 20 * {: by:n :}
   BW-N BW-ITERS * 3 * {: fl:n :}
   sax relu + {: ns:n :}
   s" kernel=UNFUSED-SAXPY-V4+RELU-V4" type
   s"  work_items=" type BW-N .U
   s"  kernels=2" type
   s"  iters=" type BW-ITERS .U cr
   s" gpu_elapsed_ns_sum=" type ns .U cr
   by fl ns PTXPROF:REPORT-METRICS ;

: REPORT-SPEEDUP ( n n -- )
   {: unfused:n fused:n :}
   s" fusion_elapsed_ratio_x1000=" type
   unfused 1000 * fused / .U cr ;

: MAIN ( -- )
   RUN-SAXPY-V4 {: sax:n :}
   RUN-RELU-V4 {: relu:n :}
   sax relu REPORT-UNFUSED
   RUN-FUSED-RELU-V4 {: fused:n :}
   sax relu + fused REPORT-SPEEDUP ;

MAIN

end-package

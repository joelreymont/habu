\ profile.f - reusable PTX kernel benchmark/profile metrics.

package PTXPROF
public

63000 constant MEM-ROOF-GBS-X1000
940000 constant FP32-ROOF-GFLOPS-X1000

: TRIAD-BYTES ( n n -- n ) * 12 * ;
: SAXPY-FLOPS ( n n -- n ) * 2 * ;
: GBS-X1000 ( n n -- n ) swap 1000 * swap / ;
: GFLOPS-X1000 ( n n -- n ) GBS-X1000 ;
: UTIL-X1000 ( n n -- n ) swap 1000 * swap / ;

: REPORT-METRICS ( n n n -- )
   {: by:n fl:n ns:n :} \ ( by fl ns -- )
   by ns GBS-X1000 {: gbs:n :} \ ( -- )
   fl ns GFLOPS-X1000 {: gflops:n :} \ ( -- )
   gbs MEM-ROOF-GBS-X1000 UTIL-X1000 {: memu:n :} \ ( -- )
   gflops FP32-ROOF-GFLOPS-X1000 UTIL-X1000 {: fpu:n :} \ ( -- )
   s" bytes=" type by . s"  flops=" type fl . cr \ ( -- )
   s" GB/s_x1000=" type gbs . s"  mem_roof_util_x1000=" type memu . cr \ ( -- )
   s" GFLOP/s_x1000=" type gflops . s"  fp32_roof_util_x1000=" type fpu . cr ; \ ( -- )

end-package

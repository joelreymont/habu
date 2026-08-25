\ bandwidth-v4.f - measure v4 checked SAXPY effective bandwidth on the Orin.
\
\ Self-contained: the v4 SAXPY cubin is SELF-EMITTED (tools/ptx/fusion-emit.f
\ PTXFE:BUILD-KERNEL) from the checked producer tools/ptx/saxpy-v4-cg.f to a
\ PRIVATE per-run PTXTC root and ptxas-assembled fail-closed - no shared
\ /tmp/saxpy.cubin that could be stale/missing/wrong. A missing producer or a
\ nonzero emit/ptxas rc fails CLOSED with the named E-PTX-EMIT throw (child stderr
\ surfaced first). Off the Orin (no libcuda) it SKIPS explicitly.

require tools/ptx/fusion-emit.f
require tools/ptx/bandwidth-lib.f

package PTXBW

: RUN ( -- )
   CUDA:OPEN? 0= if
      s" bandwidth-v4: libcuda.so.1 unavailable -> SKIPPED (off-device)" type cr exit
   then
   s" habu-ptx-bw-saxpy-v4" s" tools/ptx/saxpy-v4-cg.f" PTXFE:BUILD-KERNEL
   DEFAULTS
   PTXTC:CUBIN$ CUBIN!                       \ load the private per-run cubin, not a shared /tmp path
   s" SAXPY-V4" LABEL!
   4 ELEMS-PER-THREAD!
   REPORT
   PTXTC:CLEAN ;

RUN

;package

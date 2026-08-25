\ bandwidth.f - measure scalar checked SAXPY effective bandwidth on the Orin.
\
\ Self-contained: the scalar SAXPY cubin is SELF-EMITTED (tools/ptx/fusion-emit.f
\ PTXFE:BUILD-KERNEL) from the checked producer tools/ptx/saxpy-cg.f to a PRIVATE
\ per-run PTXTC root and ptxas-assembled fail-closed - no shared /tmp/saxpy.cubin
\ that could be stale/missing/wrong. A missing producer or a nonzero emit/ptxas rc
\ fails CLOSED with the named E-PTX-EMIT throw (child stderr surfaced first), never
\ a silent load of an absent artifact. Off the Orin (no libcuda) it SKIPS explicitly.

require tools/ptx/fusion-emit.f
require tools/ptx/bandwidth-lib.f

package PTXBW

: RUN ( -- )
   CUDA:OPEN? 0= if
      s" bandwidth: libcuda.so.1 unavailable -> SKIPPED (off-device)" type cr exit
   then
   s" habu-ptx-bw-saxpy" s" tools/ptx/saxpy-cg.f" PTXFE:BUILD-KERNEL
   DEFAULTS
   PTXTC:CUBIN$ CUBIN!                       \ load the private per-run cubin, not a shared /tmp path
   REPORT
   PTXTC:CLEAN ;

RUN

;package

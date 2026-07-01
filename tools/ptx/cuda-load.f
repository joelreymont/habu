\ cuda-load.f - checked on-device proof that a SAXPY cubin loads as a GPU module.
\
\ Prereq: a cubin at /tmp/saxpy.cubin (see tools/ptx/saxpy-cg.f + ptxas).

require tools/ptx/bench.f

package PTX

: CUDA-LOAD-SAXPY ( -- bool )
   BENCH-RESET
   s" /tmp/saxpy.cubin" BENCH-CUBIN!
   s" SAXPY" BENCH-KERNEL!
   DEVICE-OPEN
   MODULE-LOAD
   MODULE-UNLOAD
   DEVICE-CLOSE
   0 0= ;

: CUDA-LOAD-REPORT ( -- )
   CUDA-LOAD-SAXPY if s" SAXPY loaded on GPU: yes" else s" SAXPY loaded on GPU: NO" then type cr ;

CUDA-LOAD-REPORT

end-package

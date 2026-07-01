\ bandwidth-v4.f - measure v4 checked SAXPY effective bandwidth on the Orin.
\
\ Prereq: assemble the v4 SAXPY kernel to /tmp/saxpy.cubin.

require tools/ptx/bandwidth-lib.f

package PTXBW

DEFAULTS
s" SAXPY-V4" LABEL!
4 ELEMS-PER-THREAD!
REPORT

end-package

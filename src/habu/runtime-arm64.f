\ runtime-arm64.f - the arm64 engine's installs. native-runtime.f loads it only
\ for arm64 targets, after T0: CHECKER-REG:SEAL runs SEAL-TARGET, filled here by
\ INSTALL-T0, which makes T0 tier 0's compiler (NCOMP-DISPATCH:COMP-CELL)
\ before the capture seals the cell.

require src/habu/t0-arm64.f
require src/habu/t0.f

package RUNTIME-ARM64

private

: INSTALL-T0 ( -- )
   CTARGET-ARCH:AARCH64 T0-EMIT:SELECT T0:INSTALL ;

: BIND ( -- ) [: INSTALL-T0 ;] is CHECKER-REG:SEAL-TARGET ;
BIND

;package

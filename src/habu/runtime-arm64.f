\ runtime-arm64.f - the arm64 engine's installs; native-runtime.f loads it for
\ arm64 targets only. CHECKER-REG:SEAL runs SEAL-TARGET, bound here to
\ COMP:INSTALL, which makes COMP tier 0's compiler (NCOMP-DISPATCH:COMP-CELL)
\ before the capture seals the cell. It carries the x86-64 emitter too, for T0's
\ shadow walk while an x86-64 image is built (src/habu/t0.f THE WALKS): COMP is
\ an engine package, and a sealed package cannot be reopened to load it later.

require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0.f

package RUNTIME-ARM64

private

: BIND ( -- ) ['] COMP:INSTALL is CHECKER-REG:SEAL-TARGET ;
BIND

;package

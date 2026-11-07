\ runtime-x64.f - the x86-64 engine's installs; native-runtime.f loads it only
\ for linux-x86-64. CHECKER-REG:SEAL runs SEAL-TARGET, bound here to
\ RUNTIME:INSTALL, before the capture seals the cells it fills.

require src/habu/t0-x64.f
require src/habu/runtime.f

package RUNTIME-X64

private

: BIND ( -- ) ['] RUNTIME:INSTALL is CHECKER-REG:SEAL-TARGET ;
BIND

;package

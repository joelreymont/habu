\ Image emission, loaded only after the target capture is complete. Load only
\ the emitter for this engine's native target: tools/native-emit-arm.f for
\ AArch64 Linux and macOS, tools/native-emit-x64.f for x86-64 Linux. The x86
\ writer must bind X64CODE before its Linux syscall seam; the ARM writer binds
\ A64 instead.
require src/lib/errors.f

package NATIVE-EMIT-LOAD
private

: LOAD ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" tools/native-emit-x64.f" required exit
   then
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if
      s" tools/native-emit-arm.f" required exit
   then
   s" native-emit: unknown target" 76 die ;

' LOAD
;package
execute

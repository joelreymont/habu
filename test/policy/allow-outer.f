\ allow-outer.f - the sealed design must use the loaded-bytes binding. The
\ count proves the binding ran; evaluate-closed uses the captured Habu loop.
require lib/policy.f
require lib/ffi-abi.f
require lib/adt/option.f
require test/policy/dep.f
require test/policy/foreign.f

package POLICY-OUTER-HARNESS
private

variable LOADS

: CLOSED ( ptr u8 n -- )
   1 LOADS +! evaluate-closed ;

public

: RUN ( -- )
   [: CLOSED ;] is SOURCE-ROOT:INCLUDE-INTERPRET
   s" PDEP" POLICY:ALLOW POLICY:SEAL
   0 SCRIPT-ARGV$ included LOADS @ . ;

;package

POLICY-OUTER-HARNESS:RUN

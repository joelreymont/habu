\ insn-manifest.f - Run native instruction encoding and child-process refusal checks.

require lib/test.f
require test/compiler/insn-cases.f

package INSN-MANIFEST-TEST
private

public

: RUN ( -- )
   T-RESET
   COMPILER-INSN-CASES:HABU-SIDE
   COMPILER-INSN-CASES:REFUSAL-SIDE
   T-REPORT ;

;package

INSN-MANIFEST-TEST:RUN

\ insn-manifest.f - focused test for the shared instruction-encoding contract.
\
\ It runs the Habu half of the instruction parity binding: the frozen rows in
\ `test/compiler/insn-schema.f` asked of the shipped ARM64 assembler. That is
\ every encoding vector driven through the real emitter words into the real code
\ buffer, the overflow vectors that the shipped encoders do not refuse, the
\ reserved-register slots no check reaches, and the `>LIMM` packings.
\
\ It deliberately asks nothing of the proof assistant, and it spawns no child
\ engines. The other half - making Rocq prove the same rows about
\ `Habu.Common.Insn`, holding the model's assumption set empty, and running the
\ refusals that end a process - is `test/compiler/insn-proof.f`, which needs the
\ Rocq toolchain on PATH and therefore runs in the standalone gate rather than
\ in every resident test run.

require lib/test.f
require test/compiler/insn-cases.f

package INSN-MANIFEST-TEST
private

public

: RUN ( -- )
   T-RESET
   COMPILER-INSN-CASES:HABU-SIDE
   T-REPORT ;

;package

INSN-MANIFEST-TEST:RUN

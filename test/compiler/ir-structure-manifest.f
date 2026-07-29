\ ir-structure-manifest.f - focused test for the shared structure contract.
\
\ It runs the Habu half of the structure parity binding: the frozen rows in
\ `test/compiler/ir-structure-schema.f` asked of the shipped `IR-OP` and
\ `IR-FUN`. That is the frozen guard bodies, the call-closed guard rows, the
\ coverage of the sequence table, and every build sequence driven through the
\ real builder words.
\
\ It deliberately asks nothing of the proof assistant. The other half - making
\ Rocq prove the same rows about `Habu.Common.Structure`, and holding the model's
\ assumption set empty - is `test/compiler/ir-structure-proof.f`, which needs the
\ Rocq toolchain on PATH and therefore runs in the standalone gate rather than in
\ every resident test run.

require lib/test.f
require test/compiler/ir-structure-cases.f

package IR-STRUCTURE-MANIFEST-TEST
public

: RUN ( -- )
   T-RESET
   COMPILER-STRUCT-CASES:HABU-SIDE
   T-REPORT ;

;package

IR-STRUCTURE-MANIFEST-TEST:RUN

\ ir-intern-manifest.f - focused test for the shared interning contract.
\
\ It runs the Habu half of the interning parity binding: the frozen rows in
\ `test/compiler/ir-intern-schema.f` asked of the shipped `IR-SYM`, `IR-TYPE` and
\ `IR-ATTR`. That is the compared-field list of every scan predicate, the
\ check-before-write ordering of the four intern paths, the reference guards, and
\ every intern sequence driven through the real interners.
\
\ It deliberately asks nothing of the proof assistant. The other half - making
\ Rocq prove the same rows about `Habu.Common.Interning`, and holding the model's
\ assumption set empty - is `test/compiler/ir-intern-proof.f`, which needs the
\ Rocq toolchain on PATH and therefore runs in the standalone gate rather than in
\ every resident test run.

require lib/test.f
require test/compiler/ir-intern-cases.f

package IR-INTERN-MANIFEST-TEST
private

public

: RUN ( -- )
   T-RESET
   COMPILER-INTERN-CASES:HABU-SIDE
   T-REPORT ;

;package

IR-INTERN-MANIFEST-TEST:RUN

\ ir-storage-manifest.f - focused test for the shared storage contract.
\
\ It runs the Habu half of the storage parity binding: the frozen rows in
\ `test/compiler/ir-storage-schema.f` asked of the shipped `IR-CTX` and
\ `IR-ARENA`. That is the pinned capacity constants, the check-before-write
\ ordering of the nine guarded storage paths, the frozen guard and teardown
\ bodies, the shape of the vector table, and every vector row driven through the
\ real words.
\
\ It deliberately asks nothing of the proof assistant. The other half - making
\ Rocq prove the same rows about `Habu.Common.Storage`, and holding the model's
\ assumption set empty - is `test/compiler/ir-storage-proof.f`, which needs the
\ Rocq toolchain on PATH and therefore runs in the standalone gate rather than in
\ every resident test run.

require lib/test.f
require test/compiler/ir-storage-cases.f

package IR-STORAGE-MANIFEST-TEST
private

public

: RUN ( -- )
   T-RESET
   COMPILER-STORE-CASES:HABU-SIDE
   T-REPORT ;

;package

IR-STORAGE-MANIFEST-TEST:RUN

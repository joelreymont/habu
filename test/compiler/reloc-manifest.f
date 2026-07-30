\ reloc-manifest.f - focused test for the shared snapshot relocation contract.
\
\ It runs the Habu half of the relocation parity binding: the frozen rows in
\ `test/compiler/reloc-schema.f` asked of the shipped emitter. That is the
\ pinned band constants, the snapshot writer's frozen address-cell bodies, the
\ emit vocabulary held to the definitions src/habu/habu2.f actually carries, the
\ shape of the vector table, and every call and address-cell row driven through
\ the shipped `SNAP-RELOC:EMIT-CALLS` and `SNAP-RELOC:EMIT-XT` instruction
\ sequences.
\
\ It deliberately asks nothing of the proof assistant. The other half - making
\ Rocq prove the same rows about `Habu.Common.Reloc`, and holding the model's
\ assumption set empty - is `test/compiler/reloc-proof.f`, which needs the Rocq
\ toolchain on PATH and therefore runs in the standalone gate rather than in
\ every resident test run.

require lib/test.f
require test/compiler/reloc-cases.f

package RELOC-MANIFEST-TEST
public

: RUN ( -- )
   T-RESET
   RELOC-CASES:HABU-SIDE
   T-REPORT ;

;package

RELOC-MANIFEST-TEST:RUN

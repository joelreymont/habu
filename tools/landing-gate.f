\ landing-gate.f - every committed-pin suite, in one run, before a bookmark moves.
\
\ WHY THIS EXISTS. A per-landing gate runs the suites a change TOUCHED. That is
\ the right rule for a suite that tests the code it sits next to, and the wrong
\ rule for a RATCHET: a suite whose red is a disagreement between the tree and a
\ number, manifest, identity or model committed elsewhere in the repository. A
\ ratchet goes red because of a landing that never went near it - a library grew
\ past a tool's buffer, a primitive was added to the checker's axiom table, a
\ constant moved from one file to its neighbour, a dot that owned a trust row was
\ closed. Nobody's touched-suite gate runs it, so it stays red until somebody
\ runs the full gate, and by then several landings share the blame. That has now
\ happened three times (the storage manifest at 128K, again at 256K, and the six
\ reds this file was written for). This gate is the fix: one command, cheap
\ enough to run on the exact tree of EVERY landing, that runs every pin suite.
\
\ INCLUSION RULE. A suite belongs here when its failure mode is
\ "committed pin vs source drift": it compares the live tree against something a
\ human committed - a manifest block, a row count, a frozen identity, a Rocq
\ model, a classification table - so an unrelated landing can turn it red without
\ editing it. A suite that only tests its own neighbouring code does not belong
\ here; the touched-suite gate already covers it.
\
\ EXCLUDED, and why. Timing-dependent suites (codegen-compare, codegen-workload,
\ the perf contracts): their red is a measurement, not a disagreement, and they
\ need a quiet machine. Engine-build and engine-boot fixtures (hb-build-fixtures,
\ build-fixpoint-fixtures, boot-pin-fixtures, engine-size): they pin properties
\ of a BUILT engine, so what they judge is the binary in bin/, not the tree being
\ landed - they belong to the build gate, which rebuilds first.
\
\ HOW IT RUNS THEM. Through `TEST:SUITE`, the same declaration form and the same
\ spawn-per-file runner `test/gate-stdlib-cases.f` uses, over the same adapter
\ (`STDLIB-GATE:MAIN`) - so a suite here is run exactly as the gate runs it, one
\ child engine per file, and its label and file list are the gate's own. Any
\ member's nonzero exit fails the whole run: `SUITE-FAIL` prints the label, the
\ child's return code and its captured output, and dies.
\
\ Run: bin/hb --load tools/landing-gate.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f
require test/gate-stats.f
require lib/content-key.f
include test/gate-stdlib-lib.f
include test/gate-stdlib-inline-lib.f

STDLIB-GATE:MAIN

using TEST

\ ---- committed manifests and inventories -------------------------------------

SUITE stdlib-manifest
   tools/stdlib-manifest-test.f
;SUITE

SUITE trusted-inventory
   tools/trusted-inventory-test.f
;SUITE

SUITE primitive-effect-inventory
   tools/primitive-effect-inventory-test.f
;SUITE

SUITE nanogpt-inventory-lint
   tools/nanogpt-inventory-lint.f
;SUITE

\ ---- committed counts and documents ------------------------------------------

SUITE text-foundation-fixtures
   tools/lint/text-foundation-test.f
;SUITE

SUITE trust-lint
   tools/trust-lint.f
;SUITE

SUITE stale-status-lint
   tools/stale-status-lint.f
;SUITE

SUITE suite-coverage-lint
   tools/suite-coverage-lint.f
;SUITE

SUITE tool-boundary-doc-public
   tools/public-signatures-test.f
   tools/stale-status-lint-test.f
   tools/repair-schema-doc-test.f
   tools/examples-test.f
;SUITE

\ ---- frozen compiler identities ----------------------------------------------

SUITE compiler-ir-id-manifest
   test/compiler/ir-id-manifest.f
;SUITE

SUITE compiler-ir-intern-manifest
   test/compiler/ir-intern-manifest.f
;SUITE

\ ---- the Rocq parity gates ---------------------------------------------------
\ Each compiles its model under formal/Common and asks it about the same rows the
\ Habu side reads out of the shipped source, so a landing that moves either side
\ alone turns it red. They need `rocq` on PATH, like the standalone stdlib gate.

SUITE compiler-ir-id-proof
   test/compiler/ir-id-proof.f
;SUITE

SUITE compiler-ir-intern-proof
   test/compiler/ir-intern-proof.f
;SUITE

SUITE compiler-ir-structure-proof
   test/compiler/ir-structure-proof.f
;SUITE

SUITE compiler-ir-storage-proof
   test/compiler/ir-storage-proof.f
;SUITE

SUITE checker-model-proof
   test/compiler/checker-model-proof.f
;SUITE

SUITE compiler-reloc-proof
   test/compiler/reloc-proof.f
;SUITE

SUITE compiler-insn-proof
   test/compiler/insn-proof.f
;SUITE

RUN

;using

s" PASS: landing gate - every committed-pin suite green" type cr

---
title: Finish the native AOT compiler and meet the uncached Tender target
status: open
priority: 1
issue-type: task
created-at: "2026-09-11T16:38:06.249420+03:00"
blocks:
  - habu-preserve-debugger-watchpoint-74e8b1d9
  - habu-preserve-verified-input-d1bd23c6
  - habu-compare-complete-inherited-440084ec
  - habu-preserve-language-protection-a772a9a2
  - habu-bind-locals-by-a16875d6
  - habu-keep-a-row-f2c4f3d4
  - habu-check-arena-append-c7b1e040
  - habu-preserve-complete-addr-258c0288
  - habu-walk-the-dynamic-e03edf85
  - habu-track-retained-jit-1dc23a17
  - habu-build-engine-layout-abdd0188
  - habu-select-optimizing-compilation-cf2b21d4
  - habu-build-the-compiler-c348eab0
  - habu-wire-the-checker-eec26aea
  - habu-size-the-snapshot-1ca5db10
  - habu-declare-quotation-typed-e92b0571
  - habu-honour-the-committed-615f47a9
  - habu-idx-ir-sym-a35dd84d
  - habu-give-a-word-297b990d
  - habu-attr-and-remove-2b13e978
  - habu-make-spill-rewrite-ca192310
  - habu-idx-the-addr-3c5f6d9b
  - habu-read-an-ir-516b2416
  - habu-remove-test-requirements-fec97925
  - habu-pass-native-fixture-deafcd5a
  - habu-exercise-publication-of-ddd5c76f
  - habu-forge-the-artifact-25770093
  - habu-let-the-chain-9fe66f8e
  - habu-gate-build-byte-8d249e4d
  - habu-compose-every-fd-ffa78ad1
  - habu-give-the-stage-2f64be7c
  - habu-bring-the-no-29c5dc0b
  - habu-retire-the-pre-a37792de
  - habu-deliver-standalone-native-a86d4699
---

Plan: [PLAN.md](../../PLAN.md). Owner: Cedar for integration; implementation leaves are unassigned until claimed. This campaign reuses the existing speed work and the current correctness findings, without repeating landed session/allocator/hash work.

Completion: optimizing native compiler selfbuild/product rebuild, executable compilation independent of JIT invocation, correct first-generation layout and persistence, full native gate and Tender/Maki/Kestrel handoff. REPL/ordinary loader use JIT; AOT builds execute a compiled native compiler.

Speed acceptance: same pinned Tender source, all 3079 definitions counted through NCOMP, no object cache, normal checker/validators, under1.7s wall for complete optimizing load and trivial floor below500us. Report total executable-build wall including loading/capture/write alongside this target. Record source/bin IDs and actual compiler provenance. Run controlled quiet-machine before/after and per-definition pass curves; use existing Habu tools, no new measurement framework. Historical153.3→131.6s and4106→3668us used JIT-built compiler and do not prove all-AOT speed. If target still fails, keep campaign open and name measured remaining owner/pass; do not claim a projected sum.

Each leaf has source ownership, prerequisites and decisive behavior checks.
Final gate: rebuild exact source and run `bin/hb --load test/run.f`; resolve every
failure by behavior. PTX/Loom and later cache/digest work are outside this campaign.

Latest full gate: native B3 source 0c1ca1f3 with tier fixture 7ad2984b,
SHA 2ef6c87b233f5bd1f4a0e009b806851890aa67e85ccb13844d5a4f8570143781,
332 of 332 suites, 31 failures. Log `/tmp/cedar-B3-full-suite.log`; individual
outputs `/tmp/habu-native-suite-1062751897926583-17/`.

- compiler-native-string
- compiler-native-prefix-declarations
- check-cli-boundary
- native-defer-image
- process-image
- compiler-native-j
- compiler-native-leave
- app-image
- aot-section-reach
- internal-word-gate
- aot-wid-restore
- verify-prim
- aot-wide-format
- aot-chain-capture
- aot-prelude-band
- pre-trust-defer
- snapshot-writer
- core-prefix-mark
- native-fixture-paths
- cold-runtime
- build-fixpoint-fixtures
- cast
- program-diagnostics
- engine
- hb-build-fixtures
- native-gate-aot-positive
- dynamic-buffer-capture
- addrmap-inline
- protection-span
- p2-map-rewind
- field-proj

Current follow-up (2026-09-14): indexed B passes the repaired native-prefix
trust boundary and counted-loop cases. Product E (a99cd3fc, SHA
cefd25fdbd7db8126fb0f63feeb834b662eed2569c7a4cd49584ed69fef1c3d0) builds in
134.001 seconds and passes registry persistence, process-image recapture,
native-defer-image, field projection/boundary and graph suites. App-image passes
with the tier-1 startup fixture 568d66f6. Repeated registry copies caused the image
row overflow; preserving already allocated DATA storage resolves these fixtures.
The complete Tender closure still needs acceptance.

E-hosted native checker rebuilding exposed protected field-state accesses;
reviewed narrow boundary fix 165f2e32 is integrated as 3e4f0ec5 and passes its
whole native prefix regression. The combined F product builds from E in 134.074 seconds, rc0,
SHA 93fcbcd1600c0e3b1044ad54864e60ca3e6dc06d54aad7105e9645d4fb94412b.
Its full gate is next.
Reviewed dd71ead7 removes obsolete copier/scanner machinery and replaces its
fixtures with actual call/map/rollback behavior; rebuilt full gate remains open.
Fresh-process partial artifact execution is covered by reviewed 89c29acf, including
a native two-cell producer and wrong-type dependent refusals after fresh boot.
The cold dependent compiler and unknown imported provenance are explicit limits.

Tender 4cc58705, using E with its matching source, refuses native OPEN with
E-NELAB-MATCH -8650; log /tmp/cedar-tender-E/build.log, 44.924 seconds under
concurrent work, no executable and no performance acceptance. Cedar's parallel
lane owns attribution/reduction. Other old full-gate reds remain open until rerun
and reduction; they are not declared obsolete from their names.

Indexed B's measured trivial floor remains 1,233–1,239 microseconds versus B3
1,562–1,564 in three quiet pairs. The 500-microsecond ratchet still fails.
Native publication currently rebuilds the whole dictionary index; append leaf
habu-append-native-dictionary-7a7cc379 owns that measured 231–389 microsecond cost.
Full gate, quiet 3079-definition Tender timing and downstream acceptance remain.

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

- spaces,
- a
- fork
- fork
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

Current follow-up: native-defer-image passes after the source snapshot sweep fix
2d9145f2, including both restored compiler tiers and two captures. App-image and
process-image overflow at the actual 65,536-row bound; leaf 1ca5db10 owns attribution.
The native-prefix declaration failure is the trusted-only user-effect bypass
1fb5ad1d; its reviewed source repair is integrated. Native-j and native-leave
fixtures are being corrected to preserve the reviewed D04 behavior. Other red
suites remain open; they are not classified as obsolete without reduction.

B3 build: 143.626 seconds; actual optimizing trivial floor: 1,577 microseconds,
ratchet correctly rejected at 500. Reviewed indexed NDICT source is integrated
at f5f30e8f and its product build is in progress. The partial graph / logical-width
slice is approved and being composed; fresh-process native partial execution,
full gate, quiet Tender timing and downstream acceptance remain required.

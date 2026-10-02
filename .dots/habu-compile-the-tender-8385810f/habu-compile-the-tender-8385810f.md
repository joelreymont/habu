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
  - habu-compile-a-does-e4cd07c7
---

Plan: [PLAN.md](../../PLAN.md) "Required result". Claim: unassigned.

Completion: optimizing native compiler selfbuild/product rebuild, executable compilation independent of JIT invocation, correct first-generation layout and persistence, full native gate and Tender/Etch/Kestrel handoff. REPL/ordinary loader use JIT; AOT builds execute a compiled native compiler.

Speed acceptance: same pinned Tender source, all 3079 definitions counted through NCOMP, no object cache, normal checker/validators, under 1.7 s wall for complete optimizing load and trivial floor below 500 us. Report total executable-build wall including loading/capture/write alongside this target. Record source/bin IDs and actual compiler provenance. Run controlled quiet-machine before/after and per-definition pass curves; use existing Habu tools, no new measurement framework. If target still fails, keep campaign open and name measured remaining owner/pass; do not claim a projected sum.

State: all 66 leaves are closed. Open: the speed acceptance and the downstream handoff. The last measurement this dot recorded, on a composed product and not a quiet machine, put the trivial AOT floor at 953 to 996 us against the 500 us target, with JIT at 29 to 30 us; it records no timing of the complete uncached Tender load on an all-AOT compiler. The integration ledger it used to carry (gate runs J, K2, M and P2 with their `/tmp` logs) is in this file's history.

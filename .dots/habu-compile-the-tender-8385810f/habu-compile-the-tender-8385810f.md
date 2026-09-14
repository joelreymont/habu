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

Speed acceptance: same pinned Tender source, all 3079 definitions counted through NCOMP, no object cache, normal checker/validators, under 1.7 s wall for complete optimizing load and trivial floor below 500 us. Report total executable-build wall including loading/capture/write alongside this target. Record source/bin IDs and actual compiler provenance. Run controlled quiet-machine before/after and per-definition pass curves; use existing Habu tools, no new measurement framework. Historical 153.3→131.6 s and 4106→3668 us used JIT-built compiler and do not prove all-AOT speed. If target still fails, keep campaign open and name measured remaining owner/pass; do not claim a projected sum.

Each leaf has source ownership, prerequisites and decisive behavior checks.
Final gate: rebuild exact source and run `bin/hb --load test/run.f`; resolve every
failure by behavior. PTX/Loom and later cache/digest work are outside this campaign.

Latest full gate: native J, source/runtime 674900c6, SHA
3146f12e5bcd330ce85728423765c74050cf7f44b2cc801a9abc3adca097017e.
All 341 suites ran in 432.942 seconds: 329 passed, 12 failed.
Logs: /tmp/cedar-J-full-suite.{json,log}; individual outputs:
/tmp/habu-native-suite-1074470298998500-17/.

- tool-boundary-check-repair
- image-lifecycle-tasks
- native-window-owner
- checker-dead-path
- checker-rollback-sig-pool
- aot-wid-restore
- aot-wide-format
- aot-chain-capture
- native-fixture-paths
- build-fixpoint-fixtures
- engine
- program-diagnostics

Reviewed fixes after J: source-replay package import depth, required build module
assembly, growable Gforth primitive registry, private fixture boundaries/control
flags, and address-cell growth with actual aggregate artifact admission. Root's
independent focused checks pass. K2 source b8e069a5 built from J in 135.940 s,
SHA6428d167d119e683322c7f4f17b28cb6f7d10e63ffe38087df49ed355228a68f.
All eight independent storage/image suites pass; evidence is in
/tmp/cedar-K2-focused/. Shared-DATA store fix 79563f22 is accepted locally.
A separate concurrent first-registration race is reproduced through the public
K2 task API; serialized calls preserve all rows. Scoped diagnostic recovery
a775e66e/e0562cb2 is independently reviewed, with eight focused native suites
green, including complete engine and program diagnostics. Fresh combined
full-gate acceptance is pending.

The real current native builder does not use the old BF phase certifier. The
maker lane is auditing recovery/fixture consumers before adding machinery to
restore that pipeline. The emitted payload's retained-prefix type ownership is
a proven issue, tracked in 369d625d; ordinary duplicate declarations must still
reject. General warmed source-order binding remains separately in 0c9fe3d7.

Registry candidate P2 (source 6969dc7a, SHA
ef4a7aa34ada381c90435f98b10298aad4a9030e674cdc5cdc27ec103538ff4c)
built from G2 in 143.102 seconds. Pinned Tender 4cc58705 public build succeeds
in 63.822 seconds. Its executable and two recaptures preserve all 76,154 unique
rows and identical row bytes; the third image restores its public REPL. File and
DATA sizes still grow, tracked separately in 4e8a865e; this does not establish a
size fixpoint. Evidence:
/home/joel/.cache/cedar-capture-rows-u5l55np1/tender-P2/. Accepted downstream
pins and the uncached speed targets remain unchanged.

Three interleaved append-B/H pairs measured trivial AOT 994/953, 994/953, 996/961 us
and three-operation 705/645, 707/640, 707/641 us; JIT 29–30 us unchanged, exactly 200
NCOMP calls each. Own lanes drained; external Maki lint used about 34% CPU. This
is composed-product evidence, not an isolated reader result or quiet acceptance.
Frozen OPEN count falls 2895→328 with every read validation retained. Trivial
<500us, complete uncached Tender timing, combined full gate and downstream
acceptance remain open. Details and current ownership are in PLAN.md.

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

Latest full gate: native F, runtime source 3e4f0ec5 and suite source ef3fc770,
SHA 93fcbcd1600c0e3b1044ad54864e60ca3e6dc06d54aad7105e9645d4fb94412b.
All 338 suites ran: 320 passed, 18 failed. Log /tmp/cedar-F-full-suite.log;
individual outputs /tmp/habu-native-suite-1068277437369875-17/.

- compiler-native-prefix-declarations
- check-cli-boundary
- aot-section-reach
- internal-word-gate
- checker-rollback-sig-pool
- aot-wid-restore
- verify-prim
- aot-wide-format
- pre-trust-defer
- aot-chain-capture
- core-prefix-mark
- native-fixture-paths
- cold-runtime
- build-fixpoint-fixtures
- cast
- hb-build-fixtures
- native-gate-aot-positive
- protection-span

Latest private product I: runtime/source c0335a0d, SHA
13969ea2601f7e76eae2729e9d86d1fe10b297f361a7e178fc467db57cdaedae,
126.943 seconds from H. Prefix certification returns0 and preflight recovery
passes; stage2 certification now refuses ENGINE-HELPER:REGISTER at ADD. This
still blocks maker/WID suites; indexed lane owns the preserved generated source
reduction at /tmp/cedar-I-wid-maker-O129pcQz. General warmed-verifier binding is
tracked separately in0c9fe3d7. These are focused results, not a new full gate.

Since F, G2 passed CAST on both tiers including captured-core stamps, native
catch/MATCH/prefix declarations, payload rollback, verify-prim, cold-runtime,
the actual cold pre-trust fixture, hb-build-fixtures and registered
native-gate-aot-positive. H passed full checker CLI and both corrected source-root
fixture tiers. Provenance/section-reach fixtures passed F. Visibility leaf07e86028
has independently reviewed production and byte-identical enforcing B/C products;
internal/cursor gates, prefix mark and complete graph tests pass. Its final export
assertion repair and combined integration/full gate remain pending.

Tender4cc58705 requires75,900 valid distinct address rows, exceeding the65,536
fixed table. Both old and current symbol stores are supported readable data;
the old rows cannot be dropped. Digest lane owns growable registry/persistence
and actual aggregate artifact admission in1ca5db10. G2's64-second build produced
no executable. First-product growth, restore/recapture and complete standalone
acceptance remain open. Accepted Tender/Maki/Kestrel pins stay unchanged.

Three interleaved append-B/H pairs measured trivial AOT994/953,994/953,996/961us
and three-operation705/645,707/640,707/641us; JIT29–30us unchanged, exactly200
NCOMP calls each. Own lanes drained; external Maki lint used about34%CPU. This
is composed-product evidence, not an isolated reader result or quiet acceptance.
Frozen OPEN count falls2895→328 with every read validation retained. Trivial
<500us, complete uncached Tender timing, combined full gate and downstream
acceptance remain open. Details and current ownership are in PLAN.md.

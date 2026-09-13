---
title: Complete source-built stage runtime and callee closure
status: active
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-09-13T11:05:27.080398+03:00\\\\\\\"\\\"\""
blocks:
  - habu-track-retained-jit-1dc23a17
  - habu-preserve-complete-addr-258c0288
---

Plan: [PLAN.md](../../PLAN.md). Source-prefix repair by Cedar; remaining acceptance failures below stay open.

Own habu2.f cold-prefix assembly, stage source and related fixtures; exclude tier dispatch and target layout. Load/provide the cold stdlib once. Keep prelude/errors in bootstrap SRC_COMMON and mirror them in BF-APPEND-COMMON: PREFIX-REWIND removes them because they load after the core mark. Resolve actual target callees before emitting; missing names/sites refuse early. Verify aot-wide-format/aot-wid-restore/build-fixpoint-fixtures. Supported recovery repair is not the current-native selfbuild prerequisite.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Cedar production corrections: emit LAPPPROV/LAPPREQ for both source modes and select their actual failure label; the prior baked product branched to itself at 0x401a94. Restore the cold prefix's stdlib, providers and final freeze/seal tokens. Resolve PREFIX-MARK's final CURSORS record in the running dictionary rather than persisting a host ordinal. Fix CHECKER-CALLS' warm replay offset (`2 CELL *`, because its published CELLS query shadows `cells`). Publish NFEED:CAPTURE-PREPARE before sealing its package. Load NSTR inside a source host's compiler capture rather than leaving INTERN in the excluded tooling band.

A nonempty capture is not necessarily a runtime. Check compact-record membership of CHECKER-REG:DECLARATIONS and PREFIX-MARK:CURSORS; one-sided/missing membership refuses. Complete runtimes seed before source. Partial captures load the cold prefix, then seed once at the existing USER-END boundary; empty captures only load source. Keep the artifact format and existing signature sections. Runtime-kind fixtures cover empty, partial, complete and five incomplete cases.

Private evidence: source census 4919 certified / 0 rejected, compiler fixpoint, maker and partial hb-pwid boot pass. Prefix-mark/rewind, warm checker replay, cold runtime, source-independent native boot, small artifact roundtrip and the complete aot-wide-format suite pass. Logs are ignored under cedar-source-prefix/build/prefix-*; integrated full gate remains the landing check.

Integrated at3ed0b9d0 with independent review correction32a21b6e: PREFIX-MARK uses a protected private final BOUNDARY record, because its public CURSORS spelling can be retired. Original-source and partial restored boundary/rewind checks pass. Compiler-chain exact row validation is integrated through4aa70f9a, preserving the diagnostic-hook claim and checking every location/kind/target. Reordered rows and corruption refusals pass; sorted lookup avoids quadratic validation and passes at32768 rows.

Still open: aot-wid's two alias assertions select IDs from the full native host (next WID273), while the restored source product has next WID134. Actual prefix package IDs are CODE-RECLAIM=99 and CHECKER-TAPE=11. The maker has already allocated past those IDs, so its forward-only burn cannot construct the claimed collision; preserve these assertions until the fixture establishes a real target collision. Full rebuilt integration gate remains pending.

WID fixture correction: the maker reports AWBGATE's actually allocated source
WID. Each collision case boots in a private linked source tree whose top-row.f
copy appends a checked target-owner declaration before the existing seed. That
target burns forward, checks its exact ordinal and either seals the owner or
leaves it open. Probes retain executed boot entry, distinct restored ownership
and next-WID checks, and add target ordinal/sole owner/seal/value assertions.
There is no WID rewind, captured-row forgery or production extension seam.

H compiles the changed suite definitions at tier 1. Isolated target-setup probes
create the private tree and execute the generated source for both seal states:
actual ordinal 331, one owner, requested seal, value 41, rc 0 / empty stderr in
/tmp/cedar-wid-cold-{sealed,open}.{out,err}. These are helper/source checks only;
real captured-product collision acceptance awaits root's maker certificate
repair and the product that removes its old public-name collision.

2026-09-14, product F: the native maker's `E-UNDEFINED: 0<>` is an assembly
ordering failure. Rewind removes the prelude; COMMON loaded habu1.f (which
requires code-origin.f and its `0<>` caller) before restoring it. Move the
existing prelude and provided row to the beginning of COMMON. The reduced real
`--build` prefix changes from rc70 to rc0 without changing any emitter body.

The subsequent cold stage exposed a second ordering error: its core mark saved
require count 0 before the core provided rows raised it to 53. Rewind retained
the core definitions but removed all 53 rows, so `require layout-buffer.f`
failed on duplicate LBUF-GEN-CAP. Cold startup now takes its final core mark
after the provided rows, before stdlib. Full F's corresponding saved/live counts
were 41/105 and correctly restored to 41/41. The verify-prim cold parser corpus
also appended unused compiler machinery after its declarations; removing that
COMMON append preserves its complete checker prefix and exact corpus checks.

Actual F native `test/cold-runtime-test.f` now passes the stage build, cold boot
through its driver, and missing-source refusal. The unchanged native/verifier
corpus expectations pass with the unused COMMON removed. Evidence and reduced
before/after sources: `/home/joel/.cache/cedar-maker-prefix-xr276adl/`.
Root owns the next composed product and full gate; the other open cases above
are not closed by this focused repair.


The WID fixture now uses the current source NATIVE-EMIT:WRITE, through
`test/native-fixture-write.f`: empty cold engine, real source REPL/fixture
capture, then fresh artifact reader/writer. Both writers compile at tier 1 and
assert their code origin. The deliberately JIT-shaped wide-offset/HH0 payload
controls retain that tier; this is not an all-AOT build timing claim. The one
payload freeze follows every fixture declaration; repeated protection captures
retain that exact membership. Oversized-span and WID forgeries corrupt only
an ordinarily emitted image, checking the bound label, cell extent and original
value before rewriting/signing it. Reader/owned-input validation stays enabled.

Actual K2 sealed and unsealed collision cases both pass. Each target owns WID
158 alone and returns 41; AWBGATE rebases to 160 alone, executes its boot entry,
and the next WID is 168 above highest 167. Logs:
`/tmp/cedar-K2-wid-rebase-{sealed,open}.{out,err}`. The default real partial
image also boots and executes; `/tmp/cedar-K2-native-wid-gate*`. The complete
registered WID suite is running through its final PTY helper at this freeze;
its final outcome will be reported separately. Source prerequisites are the
independently reviewed cold ABI/owner leaves 299bd843/7f088cb6 and existing
quotation-store correction b8e069a5. Root owns review and integration.

Chain fixture migration remains separate: its legitimate serialized effect
pool exceeds the old text-only buffer, and its cold compiler also calls the
owner guard loaded in the excluded tool prelude. Those refusals are preserved
while their responsible storage/closure layers are repaired. Legacy public
recovery and generated-certifier ownership are tracked as a2551190/369d625d;
no new process certifier or BF fixture generation is introduced.

---
title: Complete source-built stage runtime and callee closure
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-13T11:05:27.080398+03:00\\\"\""
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

Still open: aot-wid's two alias assertions select IDs from the full native host (next WID 273), while the restored source product has next WID 134. Actual prefix package IDs are CODE-RECLAIM=99 and CHECKER-TAPE=11. The maker has already allocated past those IDs, so its forward-only burn cannot construct the claimed collision; preserve these assertions until the fixture can establish a real target collision. Compiler-chain capture now passes the INTERN band audit but ?XTOFF sees four in-window rows (one DATA pointer and three code targets), while WINDOW-CELLS=1 still assumes only DKEEP-HOOK and trapped XTs. Reconcile that tool with the complete address-row contract; do not change the count to hide missing restore behavior.

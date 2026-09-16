---
title: Preserve complete address rows through artifact IO and merge
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-13T14:51:13.059918+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.785395+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: AOT artifact IO still uses a narrower row width than the declared row, so a second row is lost while the roundtrip reports ok"
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: active; owner: Astra artifact lane, .jj-ws/cedar-artifact-rows.

Own aot-file.f row lengths/bases/counts/merge/version, aot-decl.f row contract and exact-row/MERGE fixtures. XTOFF-ROW=8 while IO uses4; cleared-buffer reproduction loses second row yet says roundtrip=ok. Use shared width; preserve fixed/window location and CODE/DATA target tags plus nullable offset+1. Shift window locations and nonnull targets by proper merged bases, checking range before publication. Reject truncated rows/incompatible old version. Verify fresh-buffer exact rows, both kinds/null/fixed cells, real merge and restored execution.25770093 owns other negative reader cases.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Confirmed during the real MERGE regression: `aot-decl.f` DATA-site rows use
`AOT-DSITE-CELL` for a raw address cell in defer metadata. `aot-file.f`
`MERGE-DSITES` treated the tagged `$800000e0` row as a chain offset and read
outside its blob, crashing at `c@`. The same merge owner must preserve the tag,
bound the masked site plus its width before reading, and rebase raw u64 values
or instruction-chain values according to the existing tag. Acceptance includes
exact relocated site tags and values for both forms in the real-capture fixture.

Focused verification passes in `test/aot-chain-capture-suite.f`: exact cleared
and fresh-process rows, real merge, and version/row/coordinate/site refusals.
A private bake of the valid artifact reached compiler fixpoint but then refused
`E-UNDEFINED: true` before engine emission; restored execution and the integrated
rebuild/full suite remain unverified here.

Integrated181a01cf after Cedar's independent source review of row widths,
tag/null rebasing and DATA-site readers, and an independent successful run of
`test/aot-chain-capture-suite.f`. The combined native suite is running in
`.jj-ws/cedar-correctness-verify`; restored executable execution remains open.

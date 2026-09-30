---
title: Expand max as a select
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T10:58:43.383322+02:00"
closed-at: "2026-09-30T14:58:53.005345+02:00"
close-reason: "Landed as 'Expand max as a select'. max expands to a diamond (one csel) when the definition's whole block budget fits IR-VERIFY:BLOCK-MAX, otherwise it keeps the straight-line mask; the choice is made per definition in NUMBER, and a does> parent never takes diamonds. Census signed-maximum 118 sites -> 0. Gen-2 aot/code-blob 1,390,344 -> 1,389,828 (-516 B) against master 028be759; gen 1 1,393,820. Engine image unchanged. NCT-M85/MIX85/MQUOT/MDOES/MCLAUSE compile and answer; NCT-M85 asserts all 85 sites are selects (a one-block miscount fails it). Gate: full suite 501/501; gens 2-5 byte-identical. Rejected on the way: per-site selector rule (+756 B), precise per-function fallback (+208 B), unguarded diamonds (refused 85-max words). Known conservatism: a body using exit is charged one block above its need (BLOCK-LIMIT = EXIT-ORD+2), so a definition that would fit exactly keeps the mask; never a refusal."
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.5). Line references are at master 8c9b75af; re-verify before editing.

Problem: EXPAND-MAXIMUM (src/compiler/native/elaborate.f:3445-3452) emits five HIR operations, 20 bytes after selection; the selector already if-converts a diamond into CMPSEL (REGION-PICK, select.f:3268).

Acceptance: max expands as `a b > brz` with arms handing a and b to the join. A tier-1 max word executed for equal inputs, mixed signs and MAX-N/MIN-N has a span of exactly cmp and csel for the maximum and no conditional branch. If the converter declines the diamond, the slice instead adds a selector pattern over the five-op expansion and the fixture stays. test/compiler/x64-* rows stay green (the diamond lowers as a branch there). Artifact: suite output; the signed-maximum census row (−12 bytes per site); tools/engine-size.f before and after; gen 2 == gen 3.

Files: src/compiler/native/elaborate.f, test/compiler/native-fold.f.

Verify: tools/native-build.f product; bin/hb --load test/compiler/native-fold.f; the x64 suites; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text.

Depends: habu-count-the-arm64-e13ae0a3 for the census row only. Parallel with habu-divide-through-div-1118f223-habu-select-shifted-idx-c57b5f1b (disjoint files).

Ownership: the files above; serialized with habu-remove-bounds-checks-22af10b0 on elaborate.f.

Claim: agent=heron workspace=.jj-ws/arm-max

---
title: Expand max as a select
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T10:58:43.383322+02:00"
closed-at: "2026-09-30T14:58:53.005345+02:00"
close-reason: "Removed after landing (b7241202 reverted). Gen-2 aot/code-blob 1,390,344 -> 1,389,828 (-516 B) with the product file unchanged at 2,477,047 B, for 386 added lines in elaborate.f, hir.f, verify.f and their tests. docs/engine-size.md, 'ARM64 code-generation changes that were removed'."
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.5). Line references are at master 8c9b75af; re-verify before editing.

Problem: EXPAND-MAXIMUM (src/compiler/native/elaborate.f:3445-3452) emits five HIR operations, 20 bytes after selection; the selector already if-converts a diamond into CMPSEL (REGION-PICK, select.f:3268).

Acceptance: max expands as `a b > brz` with arms handing a and b to the join. A tier-1 max word executed for equal inputs, mixed signs and MAX-N/MIN-N has a span of exactly cmp and csel for the maximum and no conditional branch. If the converter declines the diamond, the slice instead adds a selector pattern over the five-op expansion and the fixture stays. test/compiler/x64-* rows stay green (the diamond lowers as a branch there). Artifact: suite output; the signed-maximum census row (−12 bytes per site); tools/engine-size.f before and after; gen 2 == gen 3.

Files: src/compiler/native/elaborate.f, test/compiler/native-fold.f.

Verify: tools/native-build.f product; bin/hb --load test/compiler/native-fold.f; the x64 suites; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text.

Depends: habu-count-the-arm64-e13ae0a3 for the census row only. Parallel with habu-divide-through-div-1118f223-habu-select-shifted-idx-c57b5f1b (disjoint files).

Ownership: the files above; serialized with habu-remove-bounds-checks-22af10b0 on elaborate.f.

Claim: agent=heron workspace=.jj-ws/arm-max

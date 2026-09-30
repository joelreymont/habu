---
title: Store wide values through (STORE-CELLS)
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T10:58:43.426274+02:00"
blocks:
  - habu-report-certified-returns-2ddb20af
  - habu-expand-max-as-fec184ee
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.9). Line references are at master 8c9b75af; re-verify before editing.

Problem: WIDE-STORE (src/compiler/native/elaborate.f:3357-3367) makes a w-cell store w guarded word calls, 16 bytes per cell.

Acceptance: engine helper (STORE-CELLS) ( cells… addr n -- ) guarding each destination through GUARD-SPAN in source order, refusing at the first failing cell with the earlier cells written; the elaborator emits one call with the count. Fixture: a four-cell record store executes; refusal at each cell k observed through a shared buffer in a child; the span is w publishes, a movz and a bl. Artifact: suite output; wide-store-run census row (−(3w − 2) × 4 bytes per run); tools/engine-size.f before and after; gen 2 == gen 3.

Break-even: dispatch only if the habu-count-the-arm64-e13ae0a3 census counts more than 150 runs.

Files: src/habu/habu1.f, src/compiler/native/elaborate.f, test/compiler/native-elaborate.f, a store-protection fixture. Engine text: yes; two-stage host landing; seed mirror: no.

Verify: stage-1 build; stage-2 build; the two suites; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f.

Depends: habu-report-certified-returns-2ddb20af (shared habu1.f); habu-expand-max-as-fec184ee (shared elaborate.f); habu-count-the-arm64-e13ae0a3's count.

Ownership: the files above.

Census (tools/codegen-census.f, product of 2f165004, SHA-256 82148a2d…8c25): wide-store-run 21 runs (1,656 B, est. saving 1,228 B before the helper and elaborator cost), below the 150-run break-even. Not dispatched.

Claim: unassigned.

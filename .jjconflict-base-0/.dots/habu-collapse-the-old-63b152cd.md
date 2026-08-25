---
title: Collapse the old-vs-new harness after the cut
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T12:28:12.459453+02:00"
---

The blocked half of the retired habu-collapse-the-comparison-b7ada325 (CG-30), re-dotted so it survives that dot's closure: when the hard cut lands (epic habu-epic-hard-cut-a684f24d phase 4) and the old emitter deletes, collapse the old-vs-new comparison lane (~42 files) to one canonical corpus table, direct expected result/refusal/size relations, one runner, and the minimum reusable measurement code; retain the semantic oracle, the chain's committed baseline, and the clang reference. Blocked by habu-cut-colon-compilation-a5aa3f1f.

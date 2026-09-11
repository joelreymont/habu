---
title: Cut over staged native compiler
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.542653+02:00"
closed-at: "2026-08-06T16:07:09.655781+02:00"
close-reason: "Subsumed by habu-cut-colon-compilation-a5aa3f1f: restates the same deliverable in older design vocabulary, adds no requirement (thecut lane's reconciliation, 2026-08-06)"
blocks:
  - habu-prove-staged-compiler-5b5b145a
---

Full context: after one release-quality green checkpoint, make the staged compiler default, remove old direct compilation paths, disable/remove shadow fallback, and retain useful differentials. Acceptance: design section 21 native exits and all current master gates pass on the exact tree before verified master fast-forward.

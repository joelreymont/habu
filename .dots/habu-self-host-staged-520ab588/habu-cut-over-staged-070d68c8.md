---
title: Cut over staged native compiler
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.542653+02:00"
blocks:
  - habu-prove-staged-compiler-5b5b145a
---

Full context: after one release-quality green checkpoint, make the staged compiler default, remove old direct compilation paths, disable/remove shadow fallback, retain useful differentials, and lower trust/size ratchets atomically. Acceptance: design section 21 native/trust exits and all master gates pass on the exact tree before verified master fast-forward.

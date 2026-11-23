---
title: Reject a too-deep type, not die as cyclic
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:37:47.946329+01:00"
---

Problem: a valid row whose records chain past TWALK-MAX-DEPTH ($2000) walk levels stops the checker with the internal 'checker: term walk too deep (cyclic term)' (src/core/checker.f:3236-3240 TWALK-DEEPER) instead of a rejection; every walk through record fields that only TWALK-DEEPER bounds (ROOT-REGIONS*, ROOT-NEEDS*, CAST-INTRO*) is affected. Reproducer: /tmp/jerry-p1/bind4-probe/DEEP.f (2050 records, each FIELD run [ -- next ]). Acceptance: DEEP.f is either accepted or rejected with a Rust-style diagnostic at the row that names the depth; no internal error for an acyclic term. Depends: none. Claim: unassigned.

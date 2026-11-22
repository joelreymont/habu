---
title: "Take process.f's poll slot checks out"
status: open
priority: 2
issue-type: task
blocks:
  - habu-give-pollfd-one-1a69f5e7
created-at: "2026-11-22T14:18:22.415059+01:00"
---

Problem: src/lib/process.f PFD-SLOT throws E-A-BOUNDS for a slot outside 0..PFD-SLOTS-1 and POLL for a count outside 1..PFD-SLOTS, a runtime check on every call; test/gate-pool.f GT-POOL-PFD-SLOT does the same with E-TBL-BOUNDS. Acceptance: neither word checks its index at run time; each header states the range its caller keeps; tests of the thrown bounds sit in a disabled block under a49d054d; the stdlib-process-fixtures suite passes. Depends: habu-give-pollfd-one-1a69f5e7 (the same words). Claim: unassigned.

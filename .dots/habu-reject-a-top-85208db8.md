---
title: Reject a top-level EXPORT under --verify-only
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:01:16.116309+01:00"
---

Problem: tools/check.f --verify-only accepts a top-level EXPORT (rc 0) that the load and the default check reject; src/habu/verify-source.f RECORD-EXPORT (master too). Acceptance: --verify-only rejects it with the load's record. Depends: none. Claim: unassigned.

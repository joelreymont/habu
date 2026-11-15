---
title: Declare the sha256 context as a record
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:27:26.923123+02:00"
---

Problem: src/core/sha256.f's context is a bare byte count, though a STRUCTURE can now hold a run of bytes inline (`BUFFER name N`). Acceptance: span.f loads before sha256.f and sha256.f declares its context as a record whose buffer accessor answers a span. Depends: none. Claim: unassigned.

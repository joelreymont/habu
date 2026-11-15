---
title: "Drop FFI callbacks' per-call checks"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T04:52:35.261836+01:00"
---

Problem: src/lib/ffi-callback.f INT-CHECK, FLOAT-CHECK, DECLARED-CHECK and FRAME's null test run on every public ARG@, FARG@, INSTALL, ENTRY-ADDRESS and DESCRIPTOR call, testing bounds the callback's declaration or its caller already fixes. Acceptance: default code runs none of them; each bound is checked where the declaration or its caller is read; an instrumented build may add them back (a49d054d). Depends: none. Claim: unassigned.

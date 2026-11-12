---
title: "FFI's forget path has no caller"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:47:49.029531+02:00"
---

Problem: src/lib/ffi-abi.f FN-RESOLVE (:696) repeats FN-LOOKUP's (:486) check that FN-ADDR@ is already nonzero, then calls FN-LOOKUP. Acceptance: FN-RESOLVE is gone and its callers (:704, :710, :717, :726, :746) call FN-LOOKUP; src/lib/os-memory.f:5, which cites FN-RESOLVE, cites FN-LOOKUP. Depends: none. Claim: unassigned.

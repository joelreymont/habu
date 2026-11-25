---
title: "Check a callback group's close-time rules"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:43.306787+02:00"
---

Problem: the source check passes CALLBACK: groups the load rejects: a stray ;CALLBACK (verify-source.f CLOSE-FFI-CALLBACK returns quietly with no group open; the load raises E-FFI-SYNTAX at ffi-callback.f:542-546), a group with no FALLBACK, and a FALLBACK of the wrong result kind (ffi-callback.f:527-552); FUNCTION:'s check runs its clause and close-time rejections (FFI-CLAUSE, PLAN-FUNCTION, ffi-abi.f:1721-1731). Acceptance: each such source is rejected by the check with the load's own text; the rejections exist once, shared by run and check. Depends: habu-declare-cb-to-1f19ca30. Claim: unassigned.

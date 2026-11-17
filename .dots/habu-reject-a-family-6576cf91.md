---
title: "Reject a family value as constant's operand"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:44:45.779041+01:00"
---

Problem: `constant` publishes a family value as a raw cell: its row is `-- a` (src/habu/interpreter.f:2425), so `1 RC:LENGTH constant K : U ( -- n ) K 1 + ;` loads and `PK:MK constant C1` turns a NEWTYPE value into a raw `a`, while `!` and `,` reject a family value (src/core/checker.f FENCE-WHY :4250-4258). A role or handle escapes its owner's casts. Acceptance: `constant` of a family value is rejected at its line as `,` is, in the load and in tools/check.f; a typed constant is written `: K ( -- t ) ... ;`. Depends: none. Claim: unassigned.

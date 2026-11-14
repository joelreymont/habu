---
title: Print one record for a rendered rejection in check.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:10:31.923704+01:00"
---

Problem: after a rejection record the checker rendered (src/lib/ffi-abi.f's E-FFI-ARITY record at ;FUNCTION, reached through src/habu/verify-source.f FFI-STAGING), tools/check.f also prints the generic E-STATEMENT-THROW packet ('unknown_rejection', 'Inspect the token...') for the same stop: two records for one rejection. src/core/check-walk.f REPORTED-ERROR? lists the codes that count as already reported. Acceptance: check.f prints the rendered record alone for an FFI arity rejection, and every rejection whose record was rendered stops without a second record, as a check.f test. Depends: none. Claim: unassigned.

---
title: "Refusal rows take a later token's bindings"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.432116+02:00"
---

Problem: `: T ( ptr u8 bool -- ) if 1 then drop drop drop ;` prints expected "a b ptr u8 n" and declared_effect "a b ptr u8 bool -- a b": the expected, actual and declared_effect rows of a refusal are rendered after later tokens have bound the row variables, so they show bindings the refused token never saw. Same on the base engine and with habu-render-the-declared-f4f40bb8. Mechanism seen by reading: TAIL-CAPTURE, src/core/checker.f:4658-4660. Found by the declared-effect review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a refusal's rows are the rows at the refused token; the case prints expected and declared_effect with no binding made after the failure. Files: src/core/checker.f (TAIL-CAPTURE, the row captures), src/core/render.f. Verify: the case's prose and packet on a private engine; a case in test/compiler/input-underflow-refusals.f or test/diag-position-test.f that fails on the base. Depends: habu-render-the-declared-f4f40bb8 landed (same words). Ownership: the checker's diagnostic rows. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-refusal-rows-take-3a1d211f

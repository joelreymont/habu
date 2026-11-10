---
title: "A refusal's inferred effect and return stack show bindings made after the failure"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.174263+02:00"
---

Problem: src/core/render.f - after habu-refusal-rows-take-3a1d211f the expected, actual and declared rows are rendered as the refused token saw them, but inferred_effect and return_stack are still rendered from the live rows at the definition's end. Reported by the refusal-rows worker. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: every row a refusal prints is the row at the refused token. Files: src/core/render.f, src/core/checker.f. Verify: the lane's case.f against its cut.f for both fields. Depends: habu-refusal-rows-take-3a1d211f landed (same words). Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-refusal-s-4e54728f

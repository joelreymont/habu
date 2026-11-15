---
title: Record a verifier duplicate once
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.921781+01:00"
---

Problem: verify-source.f DUPLICATE! counts a duplicate twice and DUPLICATE-STOP throws bare; the record is built by the parent. Acceptance: one record per duplicate through INTERPRETER:DUPLICATE-REJECT, TALLY equals records. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-duprec.

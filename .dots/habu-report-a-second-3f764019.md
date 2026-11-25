---
title: Report a second does> first in the check
status: active
priority: 3
issue-type: task
created-at: "2026-11-25T17:13:53.973108+01:00"
---

Problem: for a body with two `does>` the load rejects with E-DOES-TWICE at the second; check.f --verify-only reports an unrequired token after it instead. Acceptance: both report E-DOES-TWICE at the second `does>`; a test fails without it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-does-twice.

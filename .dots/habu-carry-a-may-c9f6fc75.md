---
title: "Carry a may-flag across a stored xt's execute"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:31.876111+01:00"
---

Problem: /tmp/plan-vw/r4.f: Y executes an xt from a typed variable that reaches EVC, which renders, and Y is not deferred as a direct call to EVC would be; may-flags stop at an execute of a stored xt. Acceptance: Y in r4.f is judged as a direct call to EVC is. Depends: none. Claim: unassigned.

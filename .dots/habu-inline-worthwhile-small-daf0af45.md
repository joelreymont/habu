---
title: Inline worthwhile small native calls
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.872105+03:00"
---

Otto reports404 reachable routines of64 bytes or less and454 checked-store calls. Determine real cost and safe existing compiler opportunities; inline only where actual effects, checked-store bounds, exceptions, code-size and call semantics remain correct. Estimated3–5KB savings are unverified; do not remove checks based on Zig comparison. Acceptance: observable success/refusal E2Es written or selected first, measured same-source behavior and code-size improvement; native gate, convergence and Astra review.

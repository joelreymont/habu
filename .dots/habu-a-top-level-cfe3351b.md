---
title: A top-level stack underdepth writes no JSON record
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T13:57:06.562112+02:00"
---

Problem: under 'bin/hb --load tools/check.f -- --json-errors', a top-level 'drop', '+' or '1 2 drop drop drop' on an empty stack is refused by the engine's gate with prose only, 'hb: interpret stack underdepth: drop' (src/habu/interpreter.f:505 GATE, through REFUSE :322, which types the text and throws), and no record is written, so tools/gate-json-assert.f diag-contract fails with 'no JSON diagnostics'. Cases: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/toprec. The same cause: under tools/check.f --json-errors --all-errors a refused top-level '1 UFB' gets no packet (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pin-review/p8/s2.f and p3/all.f). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of these refusals writes one record in the top-step packet shape (src/core/render.f:1882 DIAG-JSON with TOP-STEP: a null word and null definition fields), with the token and its place; every refusal --all-errors meets writes its packet; a case pins it. Depends: none. Claim: unassigned.

---
title: A top-level stack underdepth writes no JSON record
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T13:57:06.562112+02:00"
---

Problem: under 'bin/hb --load tools/check.f -- --json-errors', a top-level 'drop', '+' or '1 2 drop drop drop' on an empty stack is rejected by the engine's gate with prose only, 'E-UNDERFLOW habu: <file>:1: interpret stack underdepth: drop', rc 70 (src/habu/interpreter.f:497 GATE, through REJECT :309, which types the text and throws), and no record is written. The same cause: under tools/check.f --json-errors --all-errors a rejected top-level '1 UFB' gets no packet: with ': UFB ( n n -- n ) + ;' on line 1 and '1 UFB' on line 2 it prints 'E-UNDERFLOW habu: <file>:2: interpret stack underdepth: UFB', rc 70. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of these rejections writes one record in the top-step packet shape (src/core/render.f:1877 DIAG-JSON with TOP-STEP: a null word and null definition fields), with the token and its place; every rejection --all-errors meets writes its packet; a case pins it. Depends: none. Claim: unassigned.

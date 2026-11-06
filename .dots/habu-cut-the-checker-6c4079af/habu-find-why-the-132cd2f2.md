---
title: Find why the linear count walks rows
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T13:05:45.585125+02:00"
---

Problem: LIN-SNAPSHOT and LIN-CHECK call LIN-TOTAL, which walks both whole stack rows through LIN-ROW-COUNT (R-RES and LIN-TYPE-COUNT per cell) for every applied word (src/core/checker.f:4387-4405; callers :4691-4732, :4994-5003, :5184-5207, :19723-19734), so the cost per token grows with stack depth (LIN-ROW-COUNT 40% at N=128). Root cause: which callers run per token, how many cells each walk visits, and whether the count can be known without walking (no linear type in the rows, or a count kept as cells are pushed and popped). Reproducer: /Users/joel/.cache/tmp/carl-ck/syn-{0,2,8,32,128}.f (package SYN-N: N pushes then N drops per definition; copied from tim). Baseline on installed hb 2026-11-06 (Mac, /usr/bin/time -l): syn-128 106.17e9 instructions 9.3 s, syn-8 22.35e9, syn-0 0.27e9 (startup). Acceptance: a reported cause with counts and an experiment measured on syn-128; the fix is a second brief. Keep any experiment inside the LIN rows and their callers (jerry 2026-11-06). Files: src/core/checker.f (read; experiment only). Verify: instructions retired before and after. Depends: none. Ownership: carl, worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-ck-lin.

---
title: "Find the unifier's cost per token"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T13:05:45.564307+02:00"
---

Problem: on syn-128 R-RES is 62% of the checker inclusive (tim's profile: unifier 38% on Maki's design, R-RES 28.9%, R-COMPRESS 13.2%, RV-NEXT? 5.1% self, TV-ENSURE 4.5% self, 95% through RVT). TVT/RVT/TVK call TV-READY, 1 TV-ENSURE, on every access (src/core/checker.f:494-505); R-RES walks a row variable's chain and compresses it (:1657-1712), COMPRESS? only outside trials (:778). Root cause: why R-RES per token grows with stack depth and what the access guard costs. Reproducer: /Users/joel/.cache/tmp/carl-ck/syn-{0,2,8,32,128}.f (package SYN-N: N pushes then N drops per definition; copied from tim). Baseline on installed hb 2026-11-06 (Mac, /usr/bin/time -l): syn-128 106.17e9 instructions 9.3 s, syn-8 22.35e9, syn-0 0.27e9 (startup). Acceptance: a reported cause with counts (calls and chain steps per token at N=2..128) and an experiment measured on syn-128; the fix is a second brief. Keep any experiment inside the unifier's own tables (jerry 2026-11-06: no reads through USIGS or HIDX memo cells; STORE+/TABLE+/CELL-ROW+ :2055-2112 and CHECK:OPEN/CLOSE are his). Files: src/core/checker.f (read; experiment only). Verify: instructions retired before and after. Depends: none. Ownership: carl, worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-ck-unify.

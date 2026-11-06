---
title: Find why checker stores pay the span guard
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T13:05:45.613900+02:00"
---

Problem: PROT-SPAN, the guard every store runs against DATA's protected bands (src/habu/habu1.f:240-290 GUARD-BAND/GUARD-SPAN, :5334 EMIT-PROT-SPAN; bands in src/habu/layout.f DATA-BANDS), is 10.7% self of the checker on Maki's design (about 12% with ! and c!). Root cause: which checker stores reach it, how many per token on syn-N, what each costs (call, latch, hull test), and whether the checker's own working stores need a guard at all; overlaps habu-take-checker-working-6703bc4f (checker working state out of DATA). Reproducer: /Users/joel/.cache/tmp/carl-ck/syn-{0,2,8,32,128}.f (package SYN-N: N pushes then N drops per definition; copied from tim). Baseline on installed hb 2026-11-06 (Mac, /usr/bin/time -l): syn-128 106.17e9 instructions 9.3 s, syn-8 22.35e9, syn-0 0.27e9 (startup). Acceptance: a reported cause with counts and an experiment measured on syn-128; the fix is a second brief. Files: src/habu/habu1.f, src/core/checker.f (read; experiment only). Verify: instructions retired before and after. Depends: none. Ownership: carl, worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-ck-guard.

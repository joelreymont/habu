---
title: Print every caller of a tied count
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T12:23:11.025773+01:00"
---

Problem: a row's caller lines pick each next caller as the largest count below the last printed (src/habu/prof.f C-PROF-REP-CALLERS, src/habu/prof-x64.f REP-CALLERS,), so a second caller with the same count never prints; x86-64-kernel-prof's (B, A) and (B, C) tie at 3 and print one line. Acceptance: every caller line prints on both arches, tested on each. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ties.

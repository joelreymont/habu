---
title: "Say why HTTP:START rejects its settings"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T15:09:15.131953+01:00"
---

Problem: src/lib/net/http.f START (:1448-1450) throws a bare E-WORKERS for workers under 1, workers past MAX-WORKERS and idle under 1 ms, with no reason text naming the setting, its value and the fix; http-arena.f ARENA-OPEN (:220-221) repeats the two workers checks for its one caller. Acceptance: each rejection's REASON:TEXT$ names the setting, the value, what was expected and the fix; ARENA-OPEN checks nothing START already did; tests. Depends: none. Claim: unassigned.

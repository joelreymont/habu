---
title: Range-reject cp!/ndict! writes into PROT-GUARD bands
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T23:18:52.391094+02:00"
---

2b-v destruction review S1 (pre-existing 2b-i sink omission, newly exposed): the code-emit sinks cp!/ndict! bypass BOTH PROT-GUARD bands. src/habu/habu1.f:1001 BCPSET compiles only B-TASK-LIVE-GUARD (task-live concurrency check) with no range guard, so 'data-base $3CB8 + cp!  : FOO 1 ;' emits JIT code over PROT-WID-N-CELL post-seal with no E-SEAL-VIOLATION; same hole covers band 1 crown jewels [$20,$A8). Fix: range-reject cp!/ndict! (and any other code-emit sink) values landing in [FRIEND-ARENA,+FRIEND-ARENA-LEN) or [PROT-REG-OFF,+PROT-REG-LEN) — legit FORGET marks live in code >= DATA-START so are unaffected. Include boundary tests: band-2 upper-boundary trap at PROT-REG-OFF+PROT-REG-LEN-1 (rc 83) and a positive proving $3D00 stays writable (destruction findings T1/T2). layout.f protection comment references this dot until closed.

---
title: "Say how to start hb on x86-64 when its region cannot map"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.173450+01:00"
---

Problem: src/habu/boot-x64.f:803 and :827 still print 'hb: cannot map fixed data region' and '... code region' (rc 78) with no size and no help, as under a ulimit -v below the region; the arm64 boot says what it asked for and what to raise. Acceptance: the x86-64 boot's line says what failed, how much it asked for and what to raise, as arm64's does. Depends: the x86-64 bring-up. Claim: unassigned.

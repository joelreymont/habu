---
title: Reduce GC root costs
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:16.866764+01:00\""
closed-at: "2026-02-19T00:04:29.850835+01:00"
close-reason: "completed: merged frame root staging and removed closure prepass"
---

src/interp/vm.zig collect path. Cause: root-set assembly overhead dominates CAS loops. Fix: compact root staging and avoid duplicate root walks.

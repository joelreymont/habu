---
title: Delete the x86-64 task-live guard nothing calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T21:52:33.107862+01:00"
---

Problem: src/habu/kernel-x64.f TASK-LIVE-GUARD, (:166) and its target LIVE-HELPER, (:185, through LIVE-CELL and LIVE-LBL) are reached by no production path since compilation while tasks run landed; only test/x86-64-kernel-engine.f BUILD (:22, three images) calls it. Acceptance: the guard, its helper and label and the test images that exist only for it are gone; an x86-64 engine builds and test/x86-64-kernel-engine.f passes on pulsar. Depends: none. Claim: unassigned.

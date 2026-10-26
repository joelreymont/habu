---
title: Take checker working state out of DATA
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T15:50:44.404285+02:00"
---

Design /Users/joel/.cache/tmp/carl-file/checker-scratch-design.md (approved by Joel 2026-10-26, 'Spans + region'; §3B deferred). Checker per-check scratch is declared inside CHECKER-REG:SCRATCH{ / }SCRATCH spans and per-definition arenas live in one checker bump region; one retire before every IMAGE-TAKE and IMAGE-DIFF unmaps the region and zeroes the spans, so no file object compares or carries checker residue. Acceptance: census (loader-common.md) shows no checker site; frame and slots e2e no new TFAIL; full suite and convergence at the tail. Children are the lanes.

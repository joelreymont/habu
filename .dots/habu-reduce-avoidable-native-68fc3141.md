---
title: Reduce avoidable native header padding
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.712901+03:00"
---

Otto reports3,400bytes of Mach-O header padding versus Zig text starting at1,896. Inspect required load-command capacity, section alignment, signing and native startup before selecting any smaller offset. Goal is unnecessary bytes removed at writer boundary, not compressed. Measure whole-file page thresholds: payload relocation saves a page for Dots but none for zzusage/blackboard per /tmp/otto-bb-zzu.md; never promise uniform16KiB savings. Acceptance: standard Mach-O checks plus real executable behavior and exact unchanged-source size/layout evidence; focused writer E2E and required capture/generation gates.

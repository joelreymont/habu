---
title: Census the reuse refusals behind the four red rows
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T01:58:03.654671+03:00"
closed-at: "2026-10-24T02:07:30.512783+03:00"
close-reason: "census-red.md written: refusal per red assertion"
---

Stack tip zoqvzolv gate (carl-hgate.Cxyb): suite RED only ffi-callback-file-cache, native-file-state {30 35}, native-file-replay {68 85 86 95 96 121 122}, native-file-auto {9 15 98 147 150 210 211 217 218}; all four pass on master pntsltsp, so they block landing. Worker in .jj-ws.noindex/carl-census maps each failing assertion to its build step, refused file, reason and word. Report: ~/.cache/tmp/carl-file/census-red.md.

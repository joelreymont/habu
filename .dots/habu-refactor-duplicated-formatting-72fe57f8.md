---
title: Refactor duplicated formatting
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:59.525484+02:00"
---

src/interp/vm.zig:4010 - Duplicated numeric formatting for D/X/B/O. Extract helper per base. Low severity.

---
title: Fix parser nil/t string compare
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:49.783852+02:00"
---

src/reader/parser.zig:398 - String compares for nil/t, rule violation. Intern then compare symbol identity. Medium severity.

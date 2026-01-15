---
title: Wire string comparison primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:27.302135+02:00"
---

src/compiler/compile.zig: Add builtin symbols for string<, string>, string<=, string>= around line ~300-320. Add dispatch cases around line ~6811 (near string=). Map to primitive calls. Dependencies: habu-add-str-primitive-525ca92c. Verify: all 4 comparison ops compile.

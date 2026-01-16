---
title: "Enhance reduce with :from-end/:initial-value"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:21.903859+02:00"
---

lib/stdlib.habu or src/compiler/compile.zig: Enhance reduce
- Add :from-end parameter: process sequence in reverse
- Add :initial-value parameter: initial accumulator value
- Handle case where sequence is empty + no initial-value (error)
- Preserve existing reduce behavior as default
- Add tests for from-end and initial-value cases
- Est: 20 min

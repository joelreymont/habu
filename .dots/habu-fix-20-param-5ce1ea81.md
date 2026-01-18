---
title: Fix 20-parameter limit in lambda forms
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-18T15:12:33.082112+02:00\""
---

src/compiler/compile.zig:2573: After parsing 20 params, the 21st param_item becomes t (raw=2) instead of next symbol. Quoted lists work fine with 21+ items, so bug is specific to lambda parameter lists. Reader/heap corruption or parser issue. Workaround: reduce loop-generate-code params from 24 to 19.

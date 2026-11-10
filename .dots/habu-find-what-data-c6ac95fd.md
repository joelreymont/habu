---
title: "Find what data-base answers at a build's top level"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:16:12.788699+02:00"
---

Problem (read, not run; from the F1 design, .jj-ws.noindex/jerry-wasm/.out/f1-design.md section 10): at the top level of a file a build loads, 'here' is in the build's own data but 'data-base' looks like hb's DATA base (src/habu/linker.f HOST :94-96 and DATA-N :100 use it that way). The F1 reproducer's number 1099515828264 is 2^40 + 4200488, which fits a distance from hb's DATA to the build's space, so 'here data-base -' at a build's top level would not be an offset in the program's DATA at all, cold or warm. Acceptance: a reported cause: what 'data-base' answers at a build's top level and in the built program, shown by a three-line program that prints both; if they differ, a second dot for the fix. Files: none (a root-cause hunt). Verify: the program's two printed numbers. Depends: none. Ownership: unassigned. Claim: unassigned.

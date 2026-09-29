---
title: Build app-image.f and the window child once per gate
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.080889+02:00"
---

Design first. src/habu/app-image.f is compiled from source by 14 rows (~7 s each); test/native-window-owner-child.f is rebuilt by 15 rows. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L1-compiler-native.md cross-cutting (b), (c). Acceptance: a keyed built-image provider (the fixture-writer dot's mechanism) serves these rows with the same assertions; per-row savings measured. Files: rows named in the report. Depends: Cache the native fixture writer dot. Claim: unassigned.

Also (~/.cache/tmp/kestrel-gate/test-review/L2-compiler-other.md): every aot-data-cell-refusals and aot-nested-body child compiles tools/aot-build*.f and src/habu/aot-*.f from source (~95 s in that lane); the same provider should serve a prebuilt linker.

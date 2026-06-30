---
title: AD scatter-add lowering primitive
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T09:10:55.073654+02:00"
closed-at: "2026-06-30T09:23:05.125368+02:00"
close-reason: "completed locally: typed SCATTER-ADD/ROW-SCATTER-ADD primitives lower to red.global.add.f32, LOAD/ROW-LOAD VJPs default to scatter-add, checked static fixtures and PTX text tests pass, full local suite cold+warm green; zed/device proof untouched"
---

Local-only slice of habu-ad-scatter-add-dc9a3184. Implement the typed SCATTER-ADD/ROW-SCATTER-ADD PTX primitives plus emit-mode lowering to red.global.add.f32, switch the LOAD/ROW-LOAD VJP default from plain STORE/ROW-STORE to scatter-add, and prove with checked static fixtures and PTX text output. Do not run zed/device gradcheck; leave parent open for Orin proof and read-once witness capability.

2026-06-30 local proof: added checked `SCATTER-ADD` and `ROW-SCATTER-ADD` PTX DSL primitives, emit-mode lowering to `red.global.add.f32`, VJP defaults for `LOAD` and `ROW-LOAD`, static checker fixtures, PTX text assertions, TRUSTED manifest rows, and docs. Focused PTX static suite, PTX text suite, typed-local-diff-lint, trust-lint, dot-dep-lint, stale-status-lint, host-lint, and filemap-lint passed. Full local suite passed cold 43664ms internal / 45.935s wall and warm 24915ms internal / 27.140s wall. Zed/device gradcheck intentionally untouched.

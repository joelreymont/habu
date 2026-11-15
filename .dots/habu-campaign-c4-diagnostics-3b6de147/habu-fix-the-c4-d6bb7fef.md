---
title: "Fix the C4 fix re-review's findings"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:59:35.521480+01:00"
closed-at: "2026-11-16T04:07:36.144361+01:00"
---

Problem: the fix re-review of vxxnynxv..mrmumopw found: test/linker.f SCRATCH? misses STAMP-CELL, so the linker suite fails 5 cases; docs/forth.md ?TARGET item says rc 70 where it stops with 74; test/name-length-test.f:305 describes an exit 67 no case has; structure-make.f and type-family.f PF-REWIND/FVX-RETIRE comments false after the add-and-commit; decl-event.f FIELD-SCHEMA@ duplicated TFAM's PF-PENDING-SCH@; type-family.f TFAM-RESET and type-schema.f public comments restate; engine-suite RSN7 pins only an absence. test/wasm/hb-build.f:441 still expects 70 for a Wasm build's ?TARGET, which stops with 74 since skkkknro (wasm-device fails F75). Acceptance: each fixed; linker and engine suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

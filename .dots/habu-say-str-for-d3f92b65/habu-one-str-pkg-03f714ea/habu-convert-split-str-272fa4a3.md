---
title: Convert split-str batch arch-01
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T11:49:34.498757+01:00"
---

Problem: 20 arch and src/habu files left marked (/tmp/jerry-p1/unmark/b-arch-01.txt: the x86-64 disassembler, kernel objects, x86-64 and Wasm runtime, crash and profiler files); they load only in target or kernel builds, so check.f alone cannot judge them. Acceptance: no marker; each target build that loads them (arm64, x86-64, Wasm) passes and its suites pass. Depends: none. Claim: unassigned.

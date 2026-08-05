---
title: Let a test own a frameless definition
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.415939+02:00"
---

tools/codegen-workload-test.f's bare-limit rule can only be exercised against engine words because only the engine's primitive emitter produces a record with no prologue — every colon definition gets sub sp, sp, #16 — so the bare half of the inline-boundary rule cannot be pinned by a test-owned fixture of chosen size (the framed half can and is). A frameless-definition capability (or a test-only emitter entry that publishes a bare record of controlled size) would let both halves of the rule run against fixtures the suite owns. Checker/emitter capability, not a test hack.

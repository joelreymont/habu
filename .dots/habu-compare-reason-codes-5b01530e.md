---
title: Compare reason codes in the checker parity gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:34:27.076458+02:00"
---

Full context: from agent modeldecl 2026-07-30. The checker parity gate (test/compiler/checker-model-proof.f) compares VERDICTS only; the model's reason codes (MD-CON-TRUNC, MD-CON-VAR, MD-DEPTH, and the rest of the MD table in formal/Common/Control.v) were verified against the shipped checker's rendered diagnostics BY HAND and nothing holds them there automatically - a checker diagnostic can drift from the model reason with the gate fully green. Extend the shared vector rows with an optional expected-reason column: Habu side compares the rendered diagnostic class through the real CHECK-QUIET-CANDIDATE path, Rocq side pins check_reason, one value written once per row. Falsify by mutation: change one reason latch in src/core/checker.f (e.g. the MD-CON-VAR site) and the gate must red on exactly that row while the verdict is unchanged. Start with the construct and depth rows where the by-hand measurements exist (recorded in dot habu-model-construct-row-bcdd5ef6).

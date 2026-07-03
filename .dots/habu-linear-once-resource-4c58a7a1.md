---
title: Linear-once resource capability
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.850475+02:00"
---

Checker capability for state-frame boundaries currently TRUSTED: evaluate/include frames (EVAL-FRAME $3800..$39FF overlap class - LESSONS.md:74-79), mmap slots, snapshot phases. Design: linear/once witnesses - a resource role produced exactly once and consumed exactly once (DEFLINEAR exists at src/core/roles.f:80-82 - extend to enforcement in the checker rather than convention), so acquire/release pairing is proven, discharging the trusted wrappers around frame push/pop. Also gives once-space witnesses the AD design wants (docs/maki/autograd.md:49-57). Design doc + capability + migrate include.f INCLUDE-PUSH/POP as the worked example.

---

## Progress — execute-path conservation landed, 2026-07-03

`DEFLINEAR` + concrete-count conservation already enforce linear-once for DIRECT
copy/drop/leak/double-consume. Found and fixed a soundness hole: `RSEXEC` (execute)
applied a quotation's effect WITHOUT the conservation `CHECKER-STEP` runs per token,
so `[: dup ;] execute` / `[: drop ;] execute` copied/dropped a linear undetected
(incl nested execute). Fix baked in `src/core/checker.f` (`RSEXEC`,
`RSEXEC-LIN-EXPLICIT?`); regressions in `test/engine-suite.f`; model + boundary in
`docs/effects.md`. Fixpoint byte-identical, gate green, ratchet ok, diff-lint 0.

Residual (NOT done here): polymorphic laundering defeats concrete-count conservation
— `[: FREE ;] KEEP` (KEEP's `over` copies a polymorphic `a`) and `[: dup FREE ;]
execute` (dup copies before FREE binds the linear). These need a linear/affine kind
discipline; spun off as `habu-linear-kind-inference-c31475b8`. Discharging the
eval-frame/mmap-slot TRUSTED wrappers is blocked on that dot PLUS rewriting those
wrappers to thread a linear frame token (src/core/include.f etc., out of this wave's
checker territory) — no TRUSTED row discharged by the execute fix alone.

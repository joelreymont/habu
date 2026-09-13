---
title: "Declare persisted quotation cells where their type is decided"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T09:23:14.720422+03:00"
blocks:
  - habu-track-retained-jit-1dc23a17
  - habu-walk-the-dynamic-e03edf85
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own mark-only code-cell primitive/xt! shared marker in habu1.f, bootstrap/cg/forth.fs primitive mirror, checker effect and layout-buffer.f typed DATA declarations/tests; exclude DBUF lifecycle. Pair with ptr-cell-mark and declare admitted quotation fields including null, without marking numeric slots. Preserve kind conflict; mapped callbacks remain legal unregistered transients. Publish primitive before source consumes it using current native bridge. Verify declaration at both tiers with AOT callback, first/second restore, and refusal to save retained JIT callback. No value heuristic.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

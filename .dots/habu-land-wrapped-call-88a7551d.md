---
title: Land wrapped-call clobber-lint extension
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T16:51:06.687088+02:00\""
---

Final content slice of the size-guard recovery (habu-recover-size-guard-31d26b61 adjudication): the wrapped-call clobber-lint extension (+84/+30) isolated as the top commit of the held size-guard-rebase chain (bookmark recover-size-guard-rebase, tip 942a4117 — read its diffs from the store with jj --ignore-working-copy; some chain commits are conflicted, do not check out). Intent: extend tools/lint/clobber-lint.f register-clobber analysis to BL-able emitter routines that WRAP another BL-able routine (a wrapped call's clobber set must include the wrapped routine's clobbers transitively, and the lint must reject a wrapper that clobbers a register its caller contract declares preserved). Re-derive the exact intent from the held diff, reimplement cleanly on current master (the PROT-GUARD:CALL fold added new wrapped-call sites - BSTORE etc. wrapping (PROT-SPAN) - which are exactly the shape this lint must cover), add positive+negative fixtures, keep the repo lint count at zero findings. After landing, the size-guard recovery dot can close: all adjudicated content slices (AOT-helper-closure, PROT-GUARD fold, shadow lint, clobber lint) merged; the four empty claim tips and prototype workspaces are already retired. Owning gates: clobber-lint test suite, gate-stdlib, lints.
Claim: agent=clobber workspace=.jj-ws/habu-land-wrapped-call-88a7551d

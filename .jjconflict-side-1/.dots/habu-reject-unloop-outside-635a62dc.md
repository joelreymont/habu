---
title: Reject unloop outside a loop body
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T22:25:30.140351+02:00"
---

Residual from habu-orphan-control-word-0370b49d (2026-07-15): ': X ( -- ) unloop ;' is accepted (rc 0) - the checker's CF-UNLOOP is a typing no-op and the engine emits runtime code that manipulates the return stack with no loop frame present, producing undefined runtime behavior when X is called (silent corruption class, not a compile crash). The checker should own this: track loop-nesting depth in the control-flow model (it already distinguishes ?DO frames for I/J/LEAVE) and reject unloop when no enclosing loop frame exists (E-diagnostic, verdict reject), matching how leave outside a loop already rejects. Engine-side: decide whether a compile-time guard is also warranted (unloop compiles a fixed return-stack pop - if the engine tracks loop openers in CFSTK records, a same-shape orphan guard applies). Acceptance: negative fixture (top-level unloop in a definition rejects, both checked and unchecked paths fail closed), positive (legal DO ... unloop exit ... LOOP still certifies), engine gate regression. Files: src/core/checker.f (CF-UNLOOP), possibly src/habu/habu2.f, test fixtures. Verify: checker suites, engine gate, full run.f. Ownership: checker control-flow model.

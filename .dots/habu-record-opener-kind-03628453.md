---
title: Record opener kind on the control-flow stack
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T15:49:37.098810+02:00"
---

Full context: while fixing the loop-closer segfault (habu-fix-loop-closer-9e5d012e), the worker verified that the remaining mispairing family is memory-safe but silently wrong: 'do IF ... loop', 'do BEGIN ... loop', 'IF ... until', 'IF ... again', 'BEGIN ... THEN' and 'IF ... ENDOF' all compile to WRONG CODE at exit 0 on the engine - never a crash, but a closer closes a frame of the wrong kind. The checker rejects every one of these programs, so checked code is safe; the engine gap matters for unchecked code and for defence in depth. Cause: src/habu/habu2.f:1457 records as a deliberate design decision that the control-flow stack carries no per-frame opener kind, so a closer can only check that SOME frame is open, not that the right kind is. Required result: each CF frame records its opener kind; every closer verifies the innermost frame's kind and rejects with the existing closer-without-opener diagnostic shape on mismatch. This touches every opener and closer plus the layout, and the stage0 mirror bootstrap/cg/forth.fs needs the same change. Acceptance: each program above is rejected by the engine with a clean diagnostic; the checked path is unchanged; the loop-opener regression battery stays green; a new battery covers closer-kind mismatch for every closer. This is an engine design change - dispatch it as its own lane with a frozen contract, not as a rider on another fix.

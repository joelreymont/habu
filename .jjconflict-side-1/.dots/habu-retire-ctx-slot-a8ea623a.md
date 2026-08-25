---
title: Retire context slot on the throw path
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:54:45.287262+02:00"
---

Full context: DEFECT found by the Rocq storage proof and CONFIRMED live against bin/hb. src/compiler/ir/arena.f lines 20-26 state that every arena resolution probes IR-CTX:SERIAL-LIVE? so an arena whose context tore down rejects with E-IR-ARENA-STALE before any pointer is touched. That is FALSE on the throw path: CTX-ENTER retires its slot and truncates the depth in the two lines AFTER execute (src/compiler/ir/context.f:329-330), so a throw skips both while MEM:WITH-BYTES still releases the 64K mapping. Proven: a nested context that throws leaves SERIAL-LIVE? answering TRUE for its serial (normal exit answers false) while its storage is gone. Nothing is exploitable today because no arena handle can survive the throw — handles are sealed nominals and catch truncates the stack — but that means the guarantee rests on the HANDLE SEAL, not on the liveness probe the comment credits. Secondary cost: the arena registry slot is never swept (SWEEP sees a live owner), so throws leak arena slots until an enclosing context exits. Fix: wrap the body in catch and re-throw after the two retirement lines, or move retirement into the MEM:WITH-BYTES teardown. Regression: extend test/compiler/ir-context.f with the nested-throw probe requiring SERIAL-LIVE? false on the inner serial, and replace the counterexample in formal/Common/Storage.v with a theorem. Also correct the arena.f comment.

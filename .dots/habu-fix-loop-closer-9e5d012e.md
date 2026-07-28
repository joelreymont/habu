---
title: Fix loop closer crash without do opener
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-28T13:03:57.113716+02:00\""
---

Claim: agent=loop_closer workspace=.jj-ws/habu-fix-loop-closer-9e5d012e

Full context: bin/hb SEGFAULTS (exit 134, crash handler register dump) when 'loop' or '+loop' closes a control frame that is not a do frame. Found while modelling checker control flow in Rocq. Minimal reproducer, no checker involved: a file containing '0 set-check' then ': UA  1 IF 2 drop loop ;'. Also crashes on ': A ( i64 -- i64 ) MK-BOOL IF STEP1 loop ;' and on 'BEGIN STEP1 loop'. Static invariant: a 'loop' compiles only when the innermost open control frame is a do frame. Owner: the engine's control-flow opener/closer pairing check, which already refuses 'control-flow closer without opener' for THEN, ENDOF and ENDCASE but does not cover loop or +loop, so 'loop' reads the do-loop compile stack unconditionally and dereferences whatever is there. The CHECKER handles the same text correctly - CHECK-CANDIDATE! answers 0 (rejected) with 'at loop' - and 'until' and 'repeat' over a wrong opener are refused cleanly, so this is specifically the engine's pairing table. Required result: the engine refuses a loop or +loop whose innermost open frame is not a do frame, with the same diagnostic shape as the existing closer-without-opener refusal, and never dereferences the do-loop stack when it is empty or holds another frame kind. Acceptance: all three reproducers exit with a clean diagnostic and a non-crash status; a negative regression covers loop and +loop over an IF frame, a BEGIN frame, and no frame at all; the checked path stays as it is. Note the reproducer uses '0 set-check' but a segfault is not an acceptable outcome for any input - unchecked Habu may be rejected, it may not crash the engine.

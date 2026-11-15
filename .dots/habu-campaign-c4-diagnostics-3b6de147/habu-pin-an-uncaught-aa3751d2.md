---
title: "Pin an uncaught record's head through one test word"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T10:36:17.689114+01:00"
closed-at: "2026-11-16T04:07:35.194355+01:00"
---

Problem: seven tests each build the head line of an uncaught throw's record (tools/load-argv-test.f LA-THROWN-HEAD$, src/lib/vocabulary-test.f THROWN-HEAD$, src/lib/fs-mutate-test.f THROWN-HEAD$, src/lib/queue-test.f Q-THROW-NEEDLE, src/lib/ffi-test.f FFI-T-THROWN-HEAD$, src/lib/task-test.f TASK-THROWN-HEAD$, test/gate-debug-lib.f GDB-THROWN$); src/lib/ffi-test.f FFI-T-HUGE-BASE asserts the stderr lacks `uncaught`, which no record holds, so it cannot fail; test/gate-dictionary-lib.f "structures reject nesting" pins an E-BAD-DECLARATION line where src/core/structures.f now rejects a nested BEGIN-STRUCTURE with E-UNCLOSED, and its failure ends native-gate-dictionary before DATA-OVERFLOW runs. Acceptance: TEST-EVAL:THROWN-HEAD$ ( n -- ptr u8 n ) in src/lib/test/eval.f builds the head and the seven call it; FFI-T-HUGE-BASE asserts no `error[`; the nesting case pins the E-UNCLOSED record; native-gate-dictionary passes. Depends: the master merge into C4. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

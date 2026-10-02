---
title: Report a guard fault from foreign code instead of hanging
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T14:44:08.206529+02:00"
---

Problem (measured 2026-10-02, eval-floor step-2 review, probes /private/tmp/claude-501/-Users-joel-Work-habu/48f16cef-908f-4f90-a73f-c7636b222de1/scratchpad/rev-floor2/probes/k2-foreign.f, k4-lldb.txt, k-foreign*.sample): calling 'FUNCTION: STRLEN$ strlen' on an address in the data stack's low guard spins forever at 100% CPU, identically on the engine before the floor work. Cause: BFFI-CALL-N-CORE parks x20 and uses it as the frame sp across the BLR (src/habu/habu1.f:1982-1983, :2002-2003). C-CRASH-DATA>R24 (src/habu/crash.f:92-94) then yields a native-stack address, the handler's descriptor loads fault inside the handler with SIGBUS/SIGSEGV masked (sa_flags $28 MACOS-SA-SIGINFO at crash.f:25, installed at :279, no SA_NODEFER), and the thread re-executes that load forever. The comment at crash.f:176-179 that an implausible descriptor falls through is wrong for the same reason. Acceptance: a fault whose pc is in foreign code exits with the crash report instead of hanging; the handler finds DATA independently of x20, or validates it before any descriptor load; a nested fault inside the handler exits rather than spinning. Verify: the k2 probe exits non-zero with a report; test/compiler/native-eval.f, the stack-guard rows, Gforth recovery.

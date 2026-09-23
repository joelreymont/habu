---
title: Refuse breakpoints outside the live code region
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-23T10:07:44.631639+03:00\""
closed-at: "2026-09-23T10:30:41.186416+03:00"
close-reason: "landed with chain FO2: BPADD refuses targets outside [dbase+DICT-SIZE, cp@-4] with E-BP-TARGET"
---

Problem: on green 45608866 / engine 3da80b23, BP+ on the atomic-cas builtin (xt 0x40fff0) publishes a breakpoint slot then exits 134. strace proves mprotect(0x40c000,32768,RW)=0 removes X from BPATCH32 itself; it faults at its next LDR, PC 0x413c2c, before the target STR. Acceptance: debug.f refuses targets outside the live JIT/native code region by name before changing any breakpoint slot or page permissions; fixture pins exact refusal, unchanged whole slot table and unchanged target code; supported tier0/tier1 breakpoint rows remain green. Keep BPATCH32 and protection unchanged. Verify: native-gate-debug, debugger-resume, engine-stack-debugger and every owning row reading debug.f. Evidence: ~/.cache/habu/profiler-verification/source-45608866. Ownership: Alder. Claim: Alder. Hazel reviews before landing.

Implemented in debug.f: BPADD checks the live code bounds and instruction
alignment before converting the target and entering the table/patch path.
E-BP-TARGET is the debugger's named refusal (-9300); its range is declared
beside the on-demand debugger, so this adds nothing to the baked engine.
GDB-BREAKPOINT-REFUSAL reproduces exit 134 before the fix, then pins the exact
refusal and byte equality of the entire slot table (including an already armed
valid slot) and the target's first 16 code bytes. GDB-BREAKPOINT-NATIVE proves
a supported tier-1 breakpoint still fires and resumes.

Focused validation is green on a private 45608866 tree and 3da80b23 host:
native-gate-debug, debugger-resume, engine-stack-debugger,
engine-runtime-regressions (including the PTY breakpoints), the complete
seven-file tool-boundary-lints row, all eleven aot-wid cases, and the strict
error-code lint (zero findings). No full gate or shared engine was run or changed.
Awaiting Hazel's review and integration.

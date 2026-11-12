---
title: Keep cells under catch when x86 constant fails
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T20:15:09.366697+02:00"
---

Problem: on older x86-64 engines, at tier 0 and tier 1, `check@ 0 set-check s" dup set-check drop constant K1" ' evaluate catch 0 set-check . . . . cr` printed 'E-UNDERFLOW: K1' then '70 384 70 70': the failed `constant` wrote cells below the catch depth. A review traced it to the constant definer, src/habu/interpreter.f:2029 DEF-CONSTANT, whose underflow test is at :2033. That form is now refused before it runs ('catch: opaque xt', rc 70). On arm64, `TRUSTED: P ( -- ) check@ 0 set-check s" dup set-check drop constant K1" evaluate ;` then `' P catch 0 set-check depth . .` prints 'E-UNDERFLOW: K1', 1 and 70, rc 0, so arm64 keeps the stack and the fault, if it remains, lies in x86-only recovery. The cause on x86 is unknown. The x86-64 tests are disabled until habu-re-enable-the-959502b5. This is an acceptance case of the parent (habu-move-the-interpreter-d35780fa): fix it in the code that stays, never in the machinery the parent replaces.
Acceptance: the TRUSTED: case, under bin/hb --load on an empty stack, is run on an x86-64 Linux host; if the stack after the catch differs from arm64's, the cause is reported at file:line and fixed there, x86-64 prints what arm64 prints, and a rejected-program test through the ordinary load path holds it; if it matches arm64, the dot closes with that run as evidence.
Depends: none.
Claim: unassigned.

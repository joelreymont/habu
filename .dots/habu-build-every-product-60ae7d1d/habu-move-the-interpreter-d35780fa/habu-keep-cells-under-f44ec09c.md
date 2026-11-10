---
title: Keep cells under catch when x86 constant fails
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T20:15:09.366697+02:00"
---

Problem: on x86-64 engines of an older master and the t0x86 stack, at tier 0 and tier 1, `check@ 0 set-check s" dup set-check drop constant K1" ' evaluate catch 0 set-check . . . . cr` printed 'E-UNDERFLOW: K1' then '70 384 70 70': the failed `constant` wrote cells below the catch depth. A review traced it to the Habu loop's constant definer, src/habu/definers.f:801 DEF-CONSTANT, whose underflow test is at :805 (`depth 0= if s" E-UNDERFLOW: " REFUSE then`). That form is now refused before it runs ('catch: opaque xt', rc 70), and arm64 now runs the same DEF-CONSTANT: on the audit's arm64 engine, some commits behind master, `TRUSTED: P ( -- ) check@ 0 set-check s" dup set-check drop constant K1" evaluate ;` then `' P catch 0 set-check depth . .` printed 'E-UNDERFLOW: K1', 1 and 70, rc 0, so arm64 keeps the stack and the fault, if it remains, lies in x86-only recovery. The cause on x86 is unknown. The x86-64 tests are disabled until habu-re-enable-the-959502b5. This is an acceptance case of the parent (habu-move-the-interpreter-d35780fa): fix it in the code that stays, never in the machinery the parent replaces.
Acceptance: the case is run on an x86-64 host; if the stack after the catch differs from arm64's, the cause is reported at file:line and fixed there, x86-64 prints what arm64 prints, and a rejected-program test through the ordinary load path holds it; if it matches arm64, the dot closes with that run as evidence.
Files: src/habu/definers.f and whatever the cause names.
Verify: first, on an x86-64 Linux host, bin/hb --load on a file holding the TRUSTED: case above, which must print 'E-UNDERFLOW: K1', 1 and 70 (`constant` reads the whole depth, so the case runs on an empty stack); then the same on arm64.
Depends: none.
Ownership: unassigned. Claim: unassigned.

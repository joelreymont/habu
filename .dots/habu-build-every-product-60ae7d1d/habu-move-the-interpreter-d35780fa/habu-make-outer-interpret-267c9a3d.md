---
title: "Make a package throw with a task live exit 0 on x86-64"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T17:22:27.666660+02:00"
---

Problem: on the x86-64 engine, a file that closes its includer's package, starts a task and throws 42, caught by the includer, prints the expected `42` and `6` but the run exits 79 with empty stderr; arm64 passes. 79 is TASK-LIVE-RC (src/habu/kernel-x64.f:79), the exit of TASK-LIVE-GUARD, (:249), which x86-64 puts on every body that moves engine state while any task is live (the rule arm64 dropped when main could compile while tasks run; :244-248). Since stdout is complete, the guard fires after the case's last line, at a load boundary or the process end. Leading hypothesis, unchecked: that refusal, which habu-lift-the-task-754c94f6 lifts on x86-64. The program, restated from a deleted test and not yet run on its own: spin.f holds `require src/lib/task.f`, `: OI-SPIN-BODY ( -- ) begin TASK:PAUSE false until ;`, `TASK:MIN-STACK TASK:TASK OI-SPINNER`, `: OI-SPIN ( -- ) ['] OI-SPIN-BODY OI-SPINNER TASK:ACTIVATE ;`, `TYPED-VARIABLE OI-PARKED ptr u8`, `variable OI-PARKED-U`, `: OI-PARK ( ptr u8 n -- ) OI-PARKED-U ! OI-PARKED ! ;`, `: OI-INC-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ included ;`; inner.f holds `;package OI-SPIN 42 throw`; case.f holds `package OI-T s" : OI-TP ( -- n ) 6 ;" evaluate` and `s" <abs path>/inner.f" OI-PARK ' OI-INC-PARKED catch . OI-TP .`. Run it as `hb --load spin.f case.f` (the engine) and as `hb --load spin.f test/interpreter-loop-on.f case.f` (the Habu loop), each expecting stdout `42\n6\n`, empty stderr, exit 0. This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: both runs of the program pass on x86-64 once habu-lift-the-task-754c94f6 lands; if either still exits non-zero, the cause is reported at file:line.

Depends: habu-lift-the-task-754c94f6, habu-re-enable-the-959502b5 (x86-64 builds are off until it lands, docs/xcomp.md:157).

Claim: unassigned.

---
title: "Give T0's own files their compiler names"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.607239+02:00"
closed-at: "2026-11-04T11:48:48.780798+02:00"
close-reason: "Census 2026-11-04, done: Premise superseded: \"T0 compiling for a second machine\" named COMP/ARM64/X86-64/LABEL; landed 9d12e9ee \"Move T0 into COMP\" (t0.f:33)."
---

Joel 2026-10-29: src/compiler/{compile,lower,emit,label,arm64,x86-64}.f, packages COMPILE LOWER EMIT LABEL EMIT-A64 EMIT-X64; NCBSTUB folds into compile.f. Top of the T0 stack before landing.

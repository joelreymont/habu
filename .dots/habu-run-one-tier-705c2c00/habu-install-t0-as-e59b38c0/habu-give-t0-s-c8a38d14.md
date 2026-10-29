---
title: "Give T0's own files their compiler names"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.607239+02:00"
---

Joel 2026-10-29: src/compiler/{compile,lower,emit,label,arm64,x86-64}.f, packages COMPILE LOWER EMIT LABEL EMIT-A64 EMIT-X64; NCBSTUB folds into compile.f. Top of the T0 stack before landing.

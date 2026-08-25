---
title: Choose which live values a call site keeps in registers
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T15:02:53.041925+02:00"
---

src/compiler/native/select.f KEEP-N keeps a SUFFIX of the live list - the values nearest the top of the caller's stack - and stops when the callee's un-destroyed registers of that file run out. The suffix is forced: the values that ARE saved have to name data-stack slots zero upwards with nothing missing, which is the shape src/compiler/native/regalloc-verify.f measures a store run as. So when the room is smaller than the live count the choice is made by position on the stack rather than by how soon or how often each value is read, and a value read every turn of a loop can be the one that goes out to a slot while one read once stays in a register. Making the choice by next-use needs the store run to stop being contiguous in the live list's order - the slots can still be contiguous if the site records which live value went into which - and it is a measurement against the committed tables, not a guess. It does not affect correctness: the allocator refuses a crossing value a clobbered register (SB-FORBID, MB-FORBID) and the validator refuses the assignment (E-A64RAV-CLOBBER) whatever this word chooses. Owners: A64SEL.

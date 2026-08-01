---
title: Spill a class of more than one value
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T20:29:42.065528+02:00"
---

src/compiler/native/regalloc.f (MB-SPILLABLE?) will only put a class of ONE value into a frame slot. A class of several - the values an argument-carrying edge ties together, the two ends of a schema tie, the two ends of a coalesced copy - would have every member's own definition store into one slot, so the slot is written more than once. src/compiler/native/regalloc-verify.f (FLOW-CK) decides a reload against 'a slot is written exactly once', which is the decidable form of 'no two values share a slot' from one module. Generalising it across a routine that branches means deciding, from the module alone, that the store a load reads is the one on every path reaching it - which needs a dominance statement the validator does not make yet. Until it lands a loop-carried value cannot be spilled, and a routine whose only held classes are loop-carried is refused E-A64RA-SPILL (test/compiler/native-regalloc.f MB-CARRIED).

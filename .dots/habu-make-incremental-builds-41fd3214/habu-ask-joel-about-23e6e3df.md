---
title: Ask Joel about public words without callers
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:13:41.799673+02:00"
---

Once lane 11 lands, one interview question (context inside it) on each public word with no production caller: keep, make private, or delete with its tests. Words: GENIO FAULT@, LATCH, LIVE-DEVICE, FORWARD, FORWARD-EMIT; XREF:BEFORE-RETIRE!, RETIRE-HOOK, BEFORE-RETIRE; AOT-CELL ALLOC-BASE export, AOT-CELL:FREE-WINDOW?; FFI:OBSERVE/UNOBSERVE/OBSERVER-CELL, FFI:FORGET-CACHE, LIB-ROW@/FN-ROW@/MAKER-BANK/SYMBOL-CAP/MAX-ARGS/NEXT-FN; TASK:OWNED-USER-NEXT, TASK:OWNED-USER-READERS, CHECKER-OWNER:UNIT-FACTS, FFI-DECL:PENDING; NUNIT-OBJECT:FILE-REFUSE, NUNIT-DEP:FILE-EXPORT; REG-POINTERS-CLEAR (src/core/checker.f, a global outside any package; after lane 11's GROW its only callers are test/compiler/native-checker-storage.f:23,24,27). Recheck each against the landed tree first. Claim: agent=carl, no workspace.

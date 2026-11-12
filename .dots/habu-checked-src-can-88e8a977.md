---
title: Checked source can read through NULL-PTR and crash the engine
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T18:24:27.410741+02:00"
---

Problem: a checked program that reads or writes through the null ends the whole process: ': T ( -- n ) NULL-PTR BYTE-VIEW c@ ;' passes the checker and 'T drop' exits rc 139, under --load and tools/check.f, and 'catch' does not help (22 accepted programs: /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nullread-hunt.md item 1). In a server every connection dies with the one request. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed"; 2026-11-12, of this fix: "why not do it now?". Settled 2026-11-12 (design /Users/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-memory-safety.md, D1): a memory fault in Habu code throws ENGINE-ERROR:MEMORY-FAULT (110) on the faulting thread after one line 'E-MEMORY-FAULT habu: memory fault at <addr> pc <pc>'. The cause, null admitted as a ptr, is habu-take-the-null-3e98f000 (design in work). Acceptance: each of the 22 programs prints the line and exits 110; under catch the catch answers 110 and a second fault is caught too; a task that faults ends with 110 and the others run; a built program exits 110; the tests that expected death by signal expect the throw. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fault.

Acceptance case (found by the review of the allocator record and typed vector): typed storage of another package's OPAQUE record is zero-filled and never passes its maker, so its pointer fields are nulls the checker cannot see (a stored cell). 'require src/lib/arena.f  TYPED-VARIABLE A ARENA:arena  : USE ( -- ) A 8 ARENA:ALLOC drop c@ . ;  USE' ends the installed hb by SIGSEGV, rc 139; 'TYPED-VARIABLE V VEC:vector<n>  V VEC:DELETE' does the same at ALLOC:FREE's read of the NULL allocator (VEC:COUNT answers 0 and VEC:AT E-VEC-BOUNDS on the same record). Acceptance: both stop with a catchable code or are refused by the checker with a reason. Verify: the two programs.

---
title: silent process-lifetime caps in the native chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.906648+02:00"
---

Problem: src/compiler/native/inline.f:37 NINL ROWS-MAX 64 - after 64 small routines publish in a process every later one is declined a row and never inlined again, DECLINED read only by tests; string.f:17-19 NSTR 512 KB / 8192 bodies with no reclamation and trap.f:21-23 NTRAP 1024 rows refuse every definition past the cap for the rest of the process; a64-effect.f:325-329 SEQ-MAX-N 10 positions so any word with more than ten inputs or outputs cannot get a contract (abi.f:57-60 E-A64EFF-SEQ); regalloc.f:1438-1445 FRAME-ONCE-CK lets only function 0 own frame slots (migrate.f:437 'a routine that calls still cannot spill'). None is logged; the project's rule is no silent caps. Acceptance: each cap either grows (arena) or refuses loudly with a named code and a count in the census; the refusal inventory (char literals, +loop, definers, exit placement, catch/tail arity, using-imported names, family rows) is recorded on the CUT dot habu-cut-colon-compilation-a5aa3f1f as measured. Files: src/compiler/native/inline.f, string.f, trap.f, a64-effect.f, regalloc.f, migrate.f. Verify: a probe past each cap shows the named refusal. Depends: none. Ownership: native chain. Claim: unassigned.

---
title: Let the chain compile a body that calls more than one word
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T10:38:37.623966+02:00"
---

src/compiler/native/migrate.f DEFINE-CALLING states ONE callee: a single spelling, entry and arity parked in M-CALLEE. A body that names two different words needs two rows in the word model, and the model itself already holds any number - HIR-WORD:DECLARE-CALLABLE appends a row per word and MODEL-ROWS counts them - so the limit is the migration entry's parked state and nothing below it. The acceptance suite works around it by chaining one call per word (NMG-L1 calls NMG-L2 calls NMG-L3). Fix: a list rather than a slot, sized like the recorder's other ceilings, and DEFINE-CALLING taking a run of callee declarations. Largely subsumed by habu-resolve-a-callee-0340dfde, which removes the parked state altogether by resolving each name off the dictionary as the tape is walked; keep this dot only as the record of the limit until that lands.

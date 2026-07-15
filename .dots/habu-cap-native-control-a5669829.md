---
title: Cap native control-flow stack depth
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T22:25:30.133899+02:00"
---

Residual from habu-orphan-control-word-0370b49d (2026-07-15): the native compiler's control-flow stack push (LCFPUSH, CFSTK-OFF 24-byte records) has NO overflow cap, while the checker caps its CFS at 31 (deep nesting -> UNCK). A definition nested deeper than the native stack region would overwrite whatever sits above CFSTK in DATA - same memory-safety class as the underflow just fixed, opposite direction. Fix: mirror the underflow guard - LCFPUSH checks depth against the region capacity and branches to a named fail-closed reject (catchable RC-REJECT via the LDIAGRET/LEVALREC tail, rc 70 at top level, diagnostic naming the token); pick the cap consistent with the checker's 31 (or the region's true capacity, documented); red-first regression (a generated : deep-nest with cap+1 IFs -> crash-or-corruption before, rc 70 after), engine gate case, bootstrap mirror guard, fixpoint x2. Files: src/habu/habu2.f (LCFPUSH), bootstrap/cg/forth.fs, test/gate-engine-lib.f. Verify: per the just-landed GE-ORPHAN-CLOSER pattern + full run.f. Ownership: engine compile-time memory safety.

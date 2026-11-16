---
title: "Judge an export's operand by the load's scope"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T12:58:29.639931+01:00"
---

Problem: the load's EXPORT finds its operand through interpreter.f FIND-SCOPE, which skips a word only a using brings in; the verifier binds through checker.f EXPORT-SYM (CHECKER-FIND-ACTIVE-SYM), which sees it, so 'using U package XP public EXPORT XW' is rejected by --load and accepted by --verify-only (also on master). The two loaders also judge a miss in different orders: EXPORT create says 'a top-level keyword, not a word' though create is a primitive, and a used HOST gets the keyword record under --load. Acceptance: one scope rule and one miss verdict shared by both loaders; a used word's record names its package; EXPORT create is E-EXPORT-PRIM; --load, check.f and --verify-only print one record for each probe. Depends: none. Claim: unassigned.

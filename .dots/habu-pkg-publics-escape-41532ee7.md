---
title: "Package publics escape internal marking: two live defects"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T15:04:26.429714+02:00"
---

VERIFIED ON MASTER 2026-08-20 (seal-2): internal-mark.f:83 IMK-GLOBAL-COLON? classifies only wid-0 records, so package PUBLIC colon words are never internal-marked - the executable top-level universe no longer equals the checker's (the file's own stated invariant). Two live consequences, both reachable from ordinary user source, both cold-prefix paths: (a) 0 0 SCHEMA-REG:REWIND exits 0 and wipes the schema registry (next declaration dies 'tfam: bad schema node' rc 76); (b) PRIM-LINK:COUNT aborts SIGABRT rc 134 reading 6 cells below base - the c5be6634 U-TYPE crash class alive behind a qualifier. FIX (sized by seal-2): extend IMK-WALK to classify package public colon records - build the wid->package map from the wid==-1 records (slot 0 = public wid, same slot FIND-NMATCH reads), form PKG:TAIL, ask the checker via SYM-FIND (checker.f:4212) / CHECKER-RESOLVES?. Privates need nothing. Blast radius: 73 pre-hook package publics, 18 without axioms (PRIM-LINK 4, CHECKER-TAPE 4, CHECKER-PREFLIGHT 3, CHECKER-BOUND 3, SCHEMA-REG 2, TYPE-NAME 2) - each earns a PPRIM row or correctly fails closed. ALSO: test/internal-word-gate.f has ZERO qualified-name child programs - add the qualified coverage in the same change; the three-way discriminator (private E-UNDEFINED qualified / public-no-axiom internal-word rc 70 / public-with-axiom runs) is the acceptance. Blocks c65f76cc (the seal pilot).

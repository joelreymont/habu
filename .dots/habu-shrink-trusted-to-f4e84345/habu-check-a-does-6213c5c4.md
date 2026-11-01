---
title: "Check a does> clause's tail as a colon body's"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T18:08:04.649979+02:00"
closed-at: "2026-11-01T04:13:36.756182+02:00"
close-reason: "Landed 7e9bab81 'Check a does> clause's tail as a colon body's'. Proof: the batch (bb3e773d fcall, 1bd54f5a casterr, 7e9bab81 dtail, 6b969738 PTY fix) merged with master 972122a1, candidate engine rebuilt from it: full suite ran 514 of 514, rc 0 (/tmp/jerry-big/batch-suite3.log); generation chain on the merge rc 0, gen 5 the same image as gen 4 (later folds change checker refusals, comments and a test, no codegen); the merge's tree equals the rebased tip."
---

Problem: CHECK-DOES-RUN (src/core/checker.f) ends a clause differently from CHECK ends a colon body, so the same body is admitted as a colon word and refused as a clause, or the reverse (lane dfrsh review, probes /tmp/jerry-dfrshrv-tmp). (1) It coerces the declared outputs even when the body never returns: `does> ( n -- ptr u8 ) drop 5 throw` is refused on master while its colon twin is admitted; CHECK runs SUNI-COERCE only under CHECK-RETURNS? and otherwise FRESH-CLAIM. (2) It never runs NP-COLLECT / NP-CHECK, so `does> ( a -- n ) drop` and a clause unifying two declared quantifiers are admitted where every colon twin is E-NONPARAMETRIC-EFFECT. Acceptance: both cases answer as their colon twins, rows in test/certify-does-definer.f or test/rigid-region-suite.f failing on master; engine-provided clauses still build. Fix: CHECK's tail in CHECK-DOES-RUN, NP-COLLECT after RAW-SIG! and before SIG-BODY-FRESH, `OK @ IF NP-CHECK THEN` before CHECK-VERDICT. Files: src/core/checker.f, a test file. Verify: engine build, full suite (shared checker behaviour). Depends: lane dfrsh landed. Ownership: checker.f CHECK-DOES-RUN. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-dtail, brief /tmp/jerry-big/dtail-brief.md, launched 2026-10-31.

Batch attribution: certify-does-definer F94 (CDD-UXJ: the clause's unknown type placed at column 4 of the walked clause text instead of the clause start) fails on the stack candidate only; casterr's candidate passes the row (it lacks section 13). Worker: brief /tmp/jerry-big/dtail-fold5-brief.md, workspace .jj-ws.noindex/jerry-dtail, @ qmtlsunm.

F94 resolved (squashed into kkyrvrwz bf71bbcb): column 4 is the type's true place in the clause text, the coordinates every native place uses (the baseline places `5` at column 34 of the same text). Master 4b54ed21 replaced C-PARSE-CREATED-SIG with C-CAPTURE-CREATED-SIG, which copies the signature into the checked text, so SGBAD-IN-SOURCE? now places the token; the empty-span fallback stays for J-DOES's interned copy (C-PLACE-CREATED-SIG). CDD-UXJ now expects column 4, bytes 3..9; the SGBAD-IN-SOURCE? comment names both copies. Rows certify-does-definer, does-clause-record, checker-soundness, rigid-region rc 0 on the stack-tip candidate.

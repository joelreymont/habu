---
title: "C2d: write declarations' facts and move the forward PRIM: rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.603881+02:00"
blocks:
  - habu-c2c3-write-the-f9f63514
---

Problem: design r2 D4 (/tmp/jerry-big/design-facts-r2.md) and DECISIONS 'where a word's facts text is laid': every writer of checker facts finds its record through the dictionary, and the 125 forward PRIM: rows name words not yet defined. Acceptance: the amenders write through CHECK:FACTS! at their funnels (RECORD-CONTROL-AT! for INTRINSIC, names:, generates:; TRUST; PARSE-IMM; CHECKER-PARSES-ROW; DFER-ADD; NORET-AXIOM moved below the words it names); a word a definer creates at run time takes its definer's created text by INTERN:FIND at RECORD-PUBLISHED-LOCAL, never laying (settle which cell carries the created effect from the top-level scan to the publication); an EXPORT alias copies its source's [32]; the 125 forward PRIM: rows move below their definitions and write through PE-CLOSE-SYM, kernel PRIM: rows write no entry; CHECK:FACTS@ ( n -- ptr u8 n ) and D8's cache read each record's [32] (none: the stores until C5); the checker's separate stores for those facts go. Acceptance case: a text the arm64 engine loop's `evaluate` recovery dropped is not answered as a live entry's facts in an old-built hb. Files: src/core/checker.f, the files holding the forward rows. Verify: candidate build; bin/hb --load test/run.f. Depends: C2c3. Ownership: those files. Claim: unassigned.

---
title: "Campaign C3: the ergonomics traps"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.631985+03:00"
---

Problem: MISSING.md and Tender's LESSONS list rules an AI must memorise because the checker does not enforce them: nominal integer types are engine constants and cannot be declared in source (Foundation A1); locals have no defined precedence against the dictionary, shadow words case-insensitively and cannot take natural names (Foundation B); quotations cannot see locals; repeated structural types have no transparent alias; packages cannot nest. Acceptance: the positive and negative fixtures named in MISSING.md pass for A1 and B; the explicit integer conversion count in Tender's sources drops measurably after A1; a local shadowing a control word is a located error; the alias and package-hierarchy dots are closed. Children (open): habu-declare-nominal-int-c1241747 habu-give-locals-a-3396aeb1 habu-warn-when-a-8c4d889a habu-share-reopen-name-92885254 habu-accept-comments-inside-145f82eb habu-type-system-md-2c7212b3 habu-nest-generated-family-70b2f31a . Absorbed on 2026-09-16: 190 dots closed with the reason 'superseded by habu-campaign-c3-the-a2477c89'; find their text with dot find. Files: src/core/checker.f, src/habu/habu2.f locals and package code, docs/forth.md, MISSING.md, docs/roadmap.md section C3. Verify: test/engine-suite.f TLOC fixtures, new A1 and B fixtures, byte fixpoint after B, test/run.f green. Depends: none. Ownership: checker and engine lanes. Claim: unassigned. Absorbed: see the archive entries closed with 'superseded by' this id.

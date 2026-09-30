---
title: "Campaign C3: the ergonomics traps"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.631985+03:00"
---

Problem: MISSING.md and Tender's LESSONS list rules a model must memorise because the checker does not enforce them: locals have no defined precedence against the dictionary, shadow words case-insensitively and cannot take natural names (Foundation B); quotations cannot see locals; repeated structural types have no transparent alias; packages cannot nest. Foundation A1, nominal integers declared in source, landed as DEFTYPE.

Acceptance: the positive and negative fixtures MISSING.md names for Foundation B pass; a local shadowing a control word is a located error; every child below is closed.

Children (open): habu-give-locals-a-3396aeb1 habu-share-reopen-name-92885254 habu-accept-comments-inside-145f82eb habu-nest-generated-family-70b2f31a habu-convert-the-type-d5aad352.

Absorbed on 2026-09-16: 190 dots closed with the reason 'superseded by habu-campaign-c3-the-a2477c89'; find their text with dot find.

Files: src/core/checker.f, src/habu/habu2.f locals and package code, docs/forth.md, MISSING.md, docs/roadmap.md section C3. Verify: test/engine-suite.f TLOC fixtures, new B fixtures, byte fixpoint after B, test/run.f green. Depends: none. Ownership: checker and engine lanes. Claim: unassigned.

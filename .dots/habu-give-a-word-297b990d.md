---
title: Give a word table the interner its ordinals belong to
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.105604+03:00"
---

Problem: HIR-WORD:LOOKUP applies the intrinsic-binding gate only to LINK-CLONE rows because a reader holds a row arena and a symbol and no interner, so a LINK-NONE table cannot read the spelling INTRINSIC-BOUND? needs; the session-compile stack (565f80a8) therefore enforces one policy at two points (ROW-BOUND? at the reading token for session rows, BVOCAB? at the writing declarers for other tables). Acceptance: HIR-WORD:NEW takes the module's interner so ROW-SPELL is one uniform word and the gate lives at LOOKUP for both link kinds; SESS-KEY/SESS-POOL/SESS-ROWS retired; the native-hir and native-word-binding suites (including OWN-GATE-CASE and PLAIN-CASE) green; the ten call sites updated. Files: src/compiler/native/hir-word.f, its callers under src/compiler/native. Verify: the suites, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.

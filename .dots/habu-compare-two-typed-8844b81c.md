---
title: Compare two typed pointers for identity
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:49:41.202894+02:00"
---

Problem: two pointers to a declared record type cannot be compared: `STRUCTURE rq FIELD a n ;STRUCTURE : EQ2 ( ptr rq ptr rq -- bool ) = ;` is refused rc 70 'at = expected: n n actual: ptr rq ptr rq' (p2, installed 8f6ec543), while `( ptr u8 ptr u8 -- bool ) =` certifies (p1) and `ptr thing NULL-PTR =` certifies (docs/forth.md 1452). Code that needs identity converts both to numbers by subtracting NULL-PTR (lane carl-arena2 lxuqxvqn test: `h HOLDER-ARENA @ NULL-PTR - a NULL-PTR - T=`; the idiom appears in about 20 files under src and test). The language has no direct way to ask whether two typed pointers name the same record. Acceptance: two pointers of one type compare with the language's ordinary equality (or the rule the design settles, stated in docs/forth.md and the card), no address conversion; the NULL-PTR subtraction idiom used only for identity is replaced at its sites; pointers of different types stay refused. Files: src/core/checker.f (the effect of = and <>), docs/forth.md, docs/forth-card.md, the idiom's sites. Verify: p1, p2 and a two-type control under --load and tools/check.f. Depends: none. Ownership: checker equality effect. Claim: unassigned.

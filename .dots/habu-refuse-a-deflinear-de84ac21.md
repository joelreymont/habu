---
title: "Refuse a DEFLINEAR name that equals a linear type's ignoring case"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:52:43.548291+02:00"
---

Problem: the linear-entry review (lane commit 8d32e48c). 'DEFLINEAR QL:t' then 'DEFLINEAR QL:T' in one package: hb --load accepts both, a build stops at the piece's end with 'duplicate definition: QL:T' (src/habu/packages.f:444-447): LINEAR-ENTRY (src/core/checker.f) finds QL:t's record, the exact-name test fails, TYPE-ENTRIES (src/habu/linker.f) calls OUTER:ALIAS-RECORD. Root: the type table keeps names apart by case (docs/forth.md 'DEFLINEAR name'), the types wordlist does not (card: a wordlist is a no-duplicate set, case-insensitively). Decided by the lead (carl, 2026-11-10, not Joel's words): one rule for load and build, the wordlist's: TYPE-RESERVED? refuses a DEFLINEAR name equal to an existing linear type's ignoring case, with 'checker: bad or duplicate signature type'; a signature still spells the token as declared. The other option, naming entries so case cannot collide, keeps two types apart by case alone, which no program needs. Not confirmed: a family entry that differs from a linear name only in case; the fix lane checks it and reports. Acceptance: both declarations are refused the same under hb --load, tools/check.f and a build; docs/forth.md and the card say it. Files: src/core/checker.f TYPE-RESERVED?, test/linear-suite.f, docs/forth.md, docs/forth-card.md. Verify: linear, type-linear. Depends: habu-make-a-linear-953febee landed. Ownership: TYPE-RESERVED?. Claim: unassigned.

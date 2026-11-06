---
title: "C3: a type is an entry"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:02.478114+02:00"
blocks:
  - habu-c2d-write-declarations-f94670f2
---

Problem: design r2 D6 (/tmp/jerry-big/design-facts-r2.md): types live in the checker's family registry (src/core/type-family.f), not in the declaring program's wordlists (DECISIONS 'where the checker's facts live'). Acceptance: STRUCTURE, ENUM, NEWTYPE, SUMTYPE and DEFTYPE define a record named for the type in the open section whose facts text is the type's declaration in the definer's clause syntax, every referenced type named with its package; run as a word it refuses by name; the registry becomes a cache filled from that text on first use; built-in types (n, u8, ptr, span<...>) stay grammar; a type and a word of one name in one package are twins and are refused; the public clashes (NUM index, byte-len, alignment; TCP4 ADDRESS, PORT; QUEUE QUEUE) are renamed by one rule, the type keeps the noun and the word takes a verb. Files: src/core/type-family.f, the definers, src/core/checker.f, the renamed words' files and callers. Verify: candidate build; bin/hb --load test/run.f; a rejected program per refusal. Depends: C2d. Ownership: those files. Claim: unassigned.

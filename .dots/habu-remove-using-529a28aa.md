---
title: Remove using
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T18:10:26.244622+02:00"
---

Problem: the user chose always prefix (2026-11-25), and `using` no longer parses, yet its machinery stays: src/compiler/native/dict.f USING-REJECTED?, the checker's USING- words and its replay of `using`, REJECTION:USING-*, E-USING-AMBIGUOUS, E-USING-SHADOW-GLOBAL, codes 7144, 7141 and 7106, the use band (USE-MAX) and the kernel's used slots. Acceptance: those words, slots and error codes are gone. Depends: none. Claim: agent=bob workspace=.jj-ws.noindex/bob-u2b1, bob-u2b2, bob-u3 (machinery lanes).

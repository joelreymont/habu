---
title: Rename the checker tape's K- kinds as approved
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.569645+02:00"
---

Source, Joel, approving: "CHECKER-TAPE:WORD carrying only the entry index (no seeded flag: DICT answers it). Kinds drop K-: CHECKER-TAPE:WORD NUMBER FLOAT STRING CHAR COUNTED-STRING PRINTED-STRING CONTROL LOCAL-NAME LOCAL-READ LOCALS-END CONSTRUCT MATCH MATCH-ARM MATCH-OF MATCH-END TICK IS OPERAND; package name stays."

Problem: the entry-index half landed in fd390676f7 (a name event's a0 is the bound record's index, read at src/habu/t0.f:1626). The rename did not: the kinds of the checker's token tape, the events the checker records per token, are still the K- constants at src/core/checker.f:16207-16228, read as CHECKER-TAPE:K-... by src/habu/t0.f (26 uses) and by tier 1's src/compiler/native/feed.f, which goes with habu-delete-tier-1-f5c44fb4. Mapping to the approved names: K-NAME WORD, K-INT NUMBER, K-REAL FLOAT, K-LOCAL-DECL LOCAL-NAME, K-LOCAL-REF LOCAL-READ, K-LOCAL-CLOSE LOCALS-END; the rest drop K-. K-UNREACHED (:16228, a token no path reaches) came later and is not in the list; by "kinds drop K-" it becomes UNREACHED. A public tail a global also owns is refused (docs/forth-card.md section 3), so check each new name against the globals first; one that collides goes back to Joel.

Acceptance: no K- kind name is left in CHECKER-TAPE or its readers; each kind has the approved name; the package is still CHECKER-TAPE.

Files: src/core/checker.f, src/habu/t0.f, and src/compiler/native/feed.f unless tier 1 is deleted first.

Verify: `rg 'CHECKER-TAPE:K-' src` finds none and checker.f's CHECKER-TAPE block (:16197) defines no K- constant (other packages' K- constants, such as src/compiler/ir/type.f's, are not in scope), the engine builds, and the T0 tests (test/t0-install.f, test/t0-arm64-rows.f) pass.

Depends: none.

Ownership: unassigned. Claim: unassigned.

---
title: Rename the checker tape's K- kinds as approved
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.569645+02:00"
---

Source, Joel, approving: "CHECKER-TAPE:WORD carrying only the entry index (no seeded flag: DICT answers it). Kinds drop K-: CHECKER-TAPE:WORD NUMBER FLOAT STRING CHAR COUNTED-STRING PRINTED-STRING CONTROL LOCAL-NAME LOCAL-READ LOCALS-END CONSTRUCT MATCH MATCH-ARM MATCH-OF MATCH-END TICK IS OPERAND; package name stays."

Problem: the kinds of the checker's token tape are still the K- constants at src/core/checker.f:14119-14140, read as CHECKER-TAPE:K-... by src/habu/t0.f and by tier 1's src/compiler/native/feed.f, which goes with habu-delete-tier-1-f5c44fb4. Mapping to the approved names: K-NAME WORD, K-INT NUMBER, K-REAL FLOAT, K-LOCAL-DECL LOCAL-NAME, K-LOCAL-REF LOCAL-READ, K-LOCAL-CLOSE LOCALS-END; the rest drop K-. K-UNREACHED (src/core/checker.f:14157) is not in the list; by "kinds drop K-" it becomes UNREACHED. A public tail a global also owns is refused (docs/forth-card.md section 3), so check each new name against the globals first; one that collides goes back to Joel.

Acceptance: no K- kind name is left in CHECKER-TAPE or its readers; each kind has the approved name; the package is still CHECKER-TAPE.

Depends: none.

Claim: unassigned.

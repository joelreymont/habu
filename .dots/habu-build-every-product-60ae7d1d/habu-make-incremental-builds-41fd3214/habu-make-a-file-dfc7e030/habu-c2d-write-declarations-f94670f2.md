---
title: "C2d: write declarations' facts and move the forward PRIM: rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.603881+02:00"
---

Problem: every writer of checker facts must find its record through the dictionary and write its bytes 32-39 ([32], the facts text's offset) through CHECK:FACTS! (src/core/checker.f:12147) or FIND-FACTS! (:12157). The amenders already do, through HOLD-AMENDED (:12184) and src/core/check-hook.f AMENDED (:267). Still open:
- A word a definer creates at run time takes its definer's created text by INTERN:FIND at RECORD-PUBLISHED-LOCAL (checker.f:16663), never laying it; settle which cell carries the created effect from the top-level scan to the publication. Today, in a session, '42 constant ANSWER', 'create BUF 8 allot' and 'variable V' each leave an empty facts text (CHECK:FACTS@ answers length 0), where a colon word answers '( n -- n )' and an engine constant '( -- a ) raw a'; SEE (habu-describe-a-published-501824c2) has no effect to show for them until this lands.
- An EXPORT alias copies its source's [32].
- The 125 forward PRIM: rows, which name words not yet defined, move below their definitions and write through PE-CLOSE-SYM (checker.f:12231); kernel PRIM: rows write no entry. The checker's separate stores for those facts go.
- A failed REPL line gives an entry it amended its previous [32] back (CHECKER-RETRACT-ROWS restores it). check-hook.f RAW-DECLARED (:181) stores through FIND-FACTS! for LASTC at a does> patch, and a definer that runs `does>` with no `create` patches a word made on an earlier line. REPL case: `create X 5 ,` / `: PATCHC ( -- ) does> ( -- n n n ) @ dup dup ;` / a line `PATCHC` that then fails gives X's [32] its previous value back (today it keeps the patched text or 0, and an offset into the failed line's rewound DATA, which a later `create JUNK 200 allot` overwrites). Beside it: X's code patch survives the failed line while the checker reverts X's effect.
- A callee's facts text parsed on first use during a caller's check runs NMAP-RESET (checker.f:6900) and overwrites the NMAP that NP-LETTER reads; that parse must save and restore NMAP.
- record-facts! exits SEAL-VIOLATION (83) with no message for a record below the seal floor, so an amender that can name a sealed entry refuses it by name before the call.

Acceptance: the items above hold, each shown by a case; a caller's check of a created word (a does> definer's FOO; a PTR-VARIABLE) reads its definer's text through CHECK:FACTS@; READ-PACK and MUT-PACK (src/lib/c2-memory.f:41-42) and OWNER-VIEW-PACK (src/lib/c2-owner.f:13) round-trip through facts text; a text the arm64 engine loop's `evaluate` recovery dropped is not answered as a live entry's facts.

Depends: none. Claim: unassigned.

---
title: Type a diverging quotation by its declared input
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:17:10.715075+02:00"
---

Problem: the checker refuses any token after a call that never returns (docs/forth.md 'A word that never returns ends its branch'), and a colon word ending in such a call takes its declared effect, but a quotation ending in one is typed by the callee's declared result: `[: GATE-CELL GATE/stuck ;]` is `[ -- gate ]`, so TTHROWSQ ( [ -- ] n -- ) refuses it (src/lib/test/assert.f:143). A declared colon word, `: STUCK-ROW ( -- ) GATE-CELL GATE/stuck ;`, loads; the FSM lane (commit "Write each FSM table call last in its arm") found this and its review dropped the wrapper with the redundant assertion. Acceptance: a quotation whose body ends in a call that never returns (die, throw, an FSM table call to a final state that throws) takes whatever quotation type its use expects; a token after the call inside the quotation is still E-DEAD-CODE; an accepted-program test passes such a quotation where `[ -- ]` is expected (as TTHROWSQ takes); the forth.md rule says quotations follow it too. Files: src/core/checker.f (quotation end typing; the no-return store NORETS), the checker's accepted- and rejected-program tests, docs/forth.md 'A word that never returns ends its branch'. Verify: the checker's tests, with an accepted case for the quotation and a rejected-program case for the dead token inside a quotation. Depends: the FSM landing. Ownership: checker.f's quotation typing. Claim: unassigned.

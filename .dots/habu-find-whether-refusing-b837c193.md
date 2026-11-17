---
title: Find whether refusing a loop that refreshes a caught cell is sound
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.050833+02:00"
---

Problem: a loop body that replaces a carried caught cell with a fresh value (lineage 0 against the entry's -1) is refused; hunt programs K3, F1, N6, S1 (lane carl-loopp, change 92ee5249, 'Let a loop carry a cell that lost its catch proof'). A fifth, from Maki (2026-11-17): a plain number read through a word from a caught value carries that value's status into the loop, which neither forth.md:2752-2756 nor the card states (forth.md says a word drops a cell's proof): `: STEPW ( SPAN:span<cell> -- SPAN:span<cell> ) {: q :} TICK @ 0= if 9 throw then q ;` `: M1 ( -- n ) SP [: STEPW ;] catch {: reply code:n :} code 0<> if code throw then 2 3 0 ?do {: at:n :} reply at SPAN:CELL@ {: len:n :} at len + loop ;` is E-LOOP-CARRIED at `loop`; the same loop in a word taking reply as input passes, as does seeding the carried cell from the reply. Is each refusal needed for soundness (a later read could see a stale value) or an over-refusal of the carried-cell rule (CP-CARRIES?, GROUP-META=/ROW-META=)? Acceptance: a hunt report per program: refusal sound with the stale read it prevents, or over-refusal with the rule's cause as path:line; an over-refusal becomes its own fix. Files: src/core/checker.f loop carry rule. Verify: the four programs on the carl-loopp engine, outcome table. Depends: carl-loopp landed. Ownership: none (read-only hunt). Claim: unassigned.

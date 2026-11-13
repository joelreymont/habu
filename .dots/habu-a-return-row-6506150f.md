---
title: A return-row refusal at a join says the row does not balance at exit
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T20:13:39.335682+02:00"
---

Problem: ': X ( bool -- ) if 1 >r then r> drop ;' is refused at 'then' with the reason 'return stack: the return row does not balance at exit' and a suggestion that speaks of the definition's exit. The refusal is RSBAD 1 from RS-CAPTURE (src/core/checker.f:5379) called by RSUNI-JOIN (:5385) where two paths meet with different return rows; exit is not reached. RS-REASON$ (src/core/render.f:1745) maps every RSBAD value but 2, 3 and 4 to that text; endcase, loop-end and match joins answer alike. Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a refusal at a join carries a reason and a suggestion true of a join ('return stack: the paths that meet here leave different return rows'; 'Leave the same return-stack cells on every path that meets here.'), and the exit balance keeps its own; test/diag-position-test.f pins the program above and an endcase, a loop-end and a match join. Depends: habu-a-refusal-s-4e54728f. Claim: unassigned.

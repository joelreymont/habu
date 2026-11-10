---
title: A return-row refusal at a join says the row does not balance at exit
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:13:39.335682+02:00"
---

Problem: ': X ( bool -- ) if 1 >r then r> drop ;' is refused at 'then' with the reason 'return stack: the return row does not balance at exit' and a suggestion that speaks of the definition's exit. The refusal is RSBAD 1 from RS-CAPTURE (src/core/checker.f:4896) called by RSUNI-JOIN (:4905-4909) where two paths meet with different return rows; exit is not reached. src/core/render.f RS-REASON$ (:1676) maps every RSBAD value but 2, 3 and 4 to that text; endcase, loop-end and match joins answer alike. The commit 'Refuse a return-stack read at its own token' puts the reason into the JSON packet. Found by its review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rowread-review.md, M4, which gives the edits). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a refusal at a join carries a reason and a suggestion true of a join ('return stack: the paths that meet here leave different return rows'; 'Leave the same return-stack cells on every path that meets here.'), and the exit balance keeps its own. Files: src/core/checker.f (a join capture beside RS-CAPTURE, RSBAD's comment), src/core/render.f (RS-REASON$, SUGGEST-TEXT, RS-ROWS), test/diag-position-test.f. Verify: a diag-position case for the program above that fails before. Depends: habu-a-read-of-a035fab4 (the reads commit lands first). Ownership: the return-row join's diagnostic. Claim: unassigned.

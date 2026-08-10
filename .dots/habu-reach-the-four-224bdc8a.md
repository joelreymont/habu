---
title: Reach the four dict quotation walls with a real row
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T16:34:19.401472+02:00"
---

dict.f's quotation descent carries four fail-closed walls no fixture reaches (ROW-INDEXABLE? din/dout, negative cells, terms!=cells with a quotation involved, the descent up-pairing): reaching them needs a certified row whose terms and cells disagree WHILE carrying a quotation - a sumtype variant with a quotation and a layout payload. Build that fixture family (or prove the checker cannot certify one and record the walls as backstops with the reason). Also: elaborate.f MODELED-AS?'s kind gate is unfalsified by any fixture (pre-existing, found by a mis-aimed mutation) - falsify it in the same pass. Files: test/compiler/native-quot.f or a sumtype fixture file, src/compiler/native/{dict,elaborate}.f comments. Depends: none.

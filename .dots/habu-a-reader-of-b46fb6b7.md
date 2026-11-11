---
title: "A reader of an earlier piece's stored quotation links its old code"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:28:40.175481+02:00"
---

Problem: src/habu/linker.f SEE (:1175, kind 1, storer-time tree) gives the reader no WORDS row for an emission below FILE-EM reached without its record. P holds HOOK and `: INSTALL [: 42 ;] HOOK xt! ;  INSTALL`; R runs `P:HOOK @ XT execute constant Q`. Editing 42 to 43 compiles P, but HOOK's word and extent keep their times (the cell names the quotation by owner identity), so R links with 42 where a cold build gives 43; the same through a middle storer that stores P's quotation (feature review of storer-time, finding 8; read from the code, not reproduced). Acceptance: a file-objects scenario where the warm build's Q equals the cold build's after the edit; SEE gives a WORDS row on EM-OWNER for an emission below FILE-EM, and the own-cell walk (OWN-CELL?, KEPT-CELL?, OWN-SEE, OWN-SEE-SPAN, STORES-KEPT?, EM-WORDS, about 60 lines) is deleted once the reader covers what it checks. Files: src/habu/linker.f, test/fixtures/file-objects/build.f, test/file-objects.f. Verify: test/file-objects.f, cold then warm Zen/Tender/Maki builds. Depends: habu-an-edit-recompiles-48b4760e. Ownership: those files. Claim: agent=jerry, no workspace yet (reproduce first, in the storer-time stack).

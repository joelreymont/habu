---
title: "clobber-lint-core.f names keyword words the row walk's removal deletes"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:49:40.819466+02:00"
---

Problem: tools/lint/clobber-lint-core.f (~:285-305, :499-513) names, in strings, engine words that the row walk's removal (dot habu-delete-the-jit-d9fdaf67, lane rowwalk-2) deletes from habu2.f and jit.f; the lane left the file alone, so after it lands the lint looks for words no file defines. Acceptance: the lint's lists name only words the tree defines, or the lists go with the checks they fed; a run of the lint over src reports what it reported before for the words that remain. Files: tools/lint/clobber-lint-core.f. Verify: rg -i --no-ignore each listed name against src finds a definition; the lint's own test rc 0. Depends: habu-delete-the-jit-d9fdaf67 landing rowwalk-2. Ownership: that file. Claim: unassigned.

Measured: `hb --load tools/lint/clobber-lint.f` already exits 1 on the installed tree (96917d9d): one CLOBBER line (habu2.f em-eval-throw-recover: x13 written, clobbered by engine-emit:lcodeinv, then read), `routines=288`, `routine census below floor`. On the row-walk stack's tip: five CLOBBER lines (habu1.f bcallback-entry x9, bsourceexecrun x15, emit-find x17 and x13; the habu2.f one), `routines=124`, the same floor refusal. No test or suite runs this lint. Lane rw-rest (brief rw-rest.md) deletes the rows naming deleted words; left for this dot: the floor, and whether each CLOBBER line is a defect or a missing row.

rw-rest deleted every row in the reviewed blocks whose name has no definition left, with KWCMP-MASK and the jit.f visit; the lint prints the same before and after. Kept because still defined: Ltok, Lfind, Lfindused-core, Lnum, Lbcs, g-print9, does-rec. Left: the rows for `MOVKHW,`, `MOVNHW,` and `MOVZHW,`, which src names only in comments.
Also named by nothing, already before the row walk (fold review F7): the `C-FIND-GLOBAL` row (:207, with the comments at :189-190 and :215-217) and GLOBAL-FIND?'s arms, and the `MOVZHW,`, `MOVKHW,`, `MOVNHW,` rows (:285-286).

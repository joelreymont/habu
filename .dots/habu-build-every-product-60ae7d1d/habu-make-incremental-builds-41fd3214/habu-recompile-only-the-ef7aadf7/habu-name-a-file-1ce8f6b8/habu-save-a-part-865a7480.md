---
title: "Save a part's data as extents with times"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T16:38:52.021136+02:00"
closed-at: "2026-11-11T08:21:59.599423+02:00"
close-reason: "landed on master as 689490272530; the lane's evidence is above, and six-edit run8 on master with the installed hb gives warm = cold unsigned in all 12 edits"
---

Problem: a part's object saves its data as one run of D bytes with cell rows keyed by offset from the part's first data byte and string rows (src/habu/linker.f DATA+, STRING+), so nothing says which word owns which bytes and data that moved cannot be told from data that changed. Lane 1 of four of the design in the parent (carl-layout/.out/design-data-by-owner.md sections 3 and 4). Acceptance: PIECES' DATA section holds extents cut at record bodies, dispatch cells and string length cells, each with kind, sum, count, length, bytes and its cell rows; TIMES lists each extent with a time kept while its span is unchanged; DATA-CK, TIMES-CK and LINK-DATA read the new layout; data references stay tag 3 and nothing reads an extent's time yet. Files: src/habu/linker.f. Verify: ev.sh prints rc=0 for hb1-hb5, warm rewritten 0, cold=warm unsigned equal, file-objects rc=0; ext-times.py before and after a comment appended to src/core/util.f are equal with more than one extent per object and no time 0. Depends: none (base 01ea2425). Ownership: src/habu/linker.f in the lane. Brief: carl-layout/.out/briefs/lane-data-extents.md. Claim: merged into the stack as vmqrtpnm 61bd2a6c (workspace retired), lands with the feature. Evidence (worker, on bf9c6f8f after the rebase): ev.sh hb1-hb5 rc 0, warm rewritten 0, cold = warm unsigned, probe rewritten 3, restored = cold, hb3 wall 1.7 s as before; file-objects passes once test/file-objects.f CAP is 16384 (main.f's object is 8526 bytes); ext-times.py before and after a comment appended to util.f equal, 8 extents in each of util.f's two objects, no time 0; a cold engine build has 496 objects and 12832 extents (7902 word, 4908 string, 22 head), none sharing kind, sum and count within an object. Not measured: hb3 user CPU.

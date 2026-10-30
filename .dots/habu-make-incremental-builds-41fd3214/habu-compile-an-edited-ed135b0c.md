---
title: Compile an edited file whole
status: active
priority: 1
issue-type: task
created-at: "2026-10-30T19:50:04.342777+02:00"
---

Problem: the spike checks an edited file's own text head by head: dc EARLIER-FRESH? skips the file's own source row (NFILE-INPUTS:LATER-ROWS) and SPIKE-SEEK searches ahead for any row whose text matches, so after a comment edit to src/lib/memory.f every head matches and the file counts as reused (gate-A review: memory.f row 1 7 in the comment step, same as an unedited warm build). Lane 11's acceptance (parent dot, file is the unit) needs the edited file recompiled. Ruling (carl): an object is consulted only when every input row holds, its own source row included; in an unchanged file each head is the next stored row's or has none, so SPIKE-SEEK compares only row INS-ROW and the search ahead and its digest cache go. Brief carl-dots/.out/carl-file/brief-fix-own.md. Acceptance: P0 counts unchanged (146/9, 13518/663), warm == cold; BODY-EDIT-CASE asserts no definition installs after a same-file edit, red before and green after; gate row 2ec826c7's comment and immediate steps see memory.f recompiled once merged. Files: tools/native-definition-cache.f, tools/native-unit-object.f, tools/native-file-inputs.f, docs/architecture.md, test/native-file-auto-e2e.f. Claim: carl worker, workspace Habu .jj-ws.noindex/carl-fown.

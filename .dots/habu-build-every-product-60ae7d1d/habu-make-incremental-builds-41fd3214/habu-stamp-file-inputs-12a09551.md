---
title: Stamp file inputs by change time
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:27.622149+02:00"
closed-at: "2026-10-30T18:48:29.517508+02:00"
close-reason: "Landed in 'Stamp file inputs by change time' (kzxnttxv): inputs stamp ctime; REPL output stamped after install; same-stat edit recompiles (test/native-file-tree-share-e2e.f). Opus review fold-ins applied (architecture.md wording, CAPTURE/SUCCESS in the test)."
---

Problem: tools/native-file-inputs.f STAT= (65-71) trusts size + mtime, so CHECK-SOURCE (103) skips hashing when a same-size edit restores mtime (touch -r), reusing stale code; the shortcut is new in the unlanded stack (not on master 9df78aae). Fix: record and compare ctime (FILE-META csec/cns) in place of mtime in STAMP-FILE/STORE-STAMP/STAT= and their callers; ctime cannot be set back by user tools, no hashing cost. Acceptance: an e2e edit that restores mtime with the same size recompiles the file. Files: tools/native-file-inputs.f + STORE-STAMP callers, one e2e case. Depends: none.

Claim (carl, 2026-10-30): worker in .jj-ws.noindex/carl-ctime on master, brief carl-dots/.out/carl-file/brief-ctime.md; revision brief-ctime-rev.md (case moved to test/native-file-tree-share-e2e.f, ADD-MTIME renamed ADD-CTIME).

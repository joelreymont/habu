---
title: Rename FS-LIST words that shadow globals
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T21:23:31.402786+02:00"
---

Problem: package FS-LIST (src/lib/fs-list.f:32) defines private MAP (:71) and OPEN (:87) and public EACH (:245) with the names of engine globals (src/habu/habu2.f:11936 MAP, src/habu/aot-arm.f:19 OPEN). Inside the package every later unqualified use of that name binds to the package word, which is how fs-anchored.f CLOSE captured the engine close inside package FS (fixed in the FS packaging lane by renaming it CLOSE-DIR). tools/lint/shadow-lint.f skips package-scoped definitions by design (they cannot clobber a global for other programs), so nothing reports these. Found by the FS packaging review (habu-put-fs-f-22edbd16), left for a follow-up lane. Acceptance: the three words get subject names per docs/forth-card.md section 1 (no operation-only or global names), callers of FS-LIST:EACH updated; a census of src/lib packages for a word named like an engine global whose package body also means the global, each one renamed. Files: src/lib/fs-list.f, src/lib/fs-list-test.f, callers of FS-LIST:EACH, any package the census names. Verify: bin/hb --load src/lib/fs-list-test.f; rg shows no package word in src/lib named MAP, OPEN or EACH; the lint and stdlib gate rows for the touched files. Depends: habu-put-fs-f-22edbd16 (the FS packaging lane lands first). Ownership: FS library. Claim: unassigned.

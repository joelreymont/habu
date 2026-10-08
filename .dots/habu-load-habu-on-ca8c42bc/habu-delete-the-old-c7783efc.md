---
title: Delete the old Gforth checker under bootstrap/
status: active
priority: 2
issue-type: task
created-at: "\"2026-10-08T23:11:23.413675+02:00\""
---

Problem: bootstrap/habu.fs, habu-lib.fs, habu-repl.fs, habu-tui.fs, examples.fs and bootstrap/src/*.fs are an older Gforth-hosted reader and checker. docs/compilation.md rules one reader; nothing outside bootstrap/ loads them, and docs/effects.md:611,890,1024 cite them only in prose.
Acceptance: confirm first that no file under bootstrap/cg, tools/ or test/ requires or names them, then delete them; reword docs/effects.md's citations so they no longer point at the files; `rg -n "habu-lib.fs|habu-repl.fs|habu-tui.fs|bootstrap/src" .` finds nothing outside .dots/ and the archive.
Files: bootstrap/habu.fs, bootstrap/habu-lib.fs, bootstrap/habu-repl.fs, bootstrap/habu-tui.fs, bootstrap/examples.fs, bootstrap/src/, docs/effects.md.
Verify: the rg above; `tools/bootstrap.sh` paths still name only files that exist.
Depends: none. Ownership: the files above. Worker: worker-light. Claim: agent=worker-light (lead carl) workspace=.jj-ws/carl-oldgf.

---
title: "Put process-env.f's PROC-ENV words in a package"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T09:41:56.226382+02:00"
---

Problem: src/lib/process-env.f defines its environment table, its defaults and their lookup words as top-level globals (PROC-ENV-N, PROC-ENV-DEF-N, PROC-ENV-VEC-NAME-IDX, PROC-ENV-VEC-HAS-NAME?, PROC-ENV-SAME-NAME?, PROC-ENV-NAME-LEN, PROC-ENV-CHECK-ENTRY and more), with only PROC-ENV-ROWS and PROC-RAW as packages; CLAUDE.md says every module has a real package. After habu-a-later-cmd-377637bf lands, PROC-ENV-VEC-HAS-NAME? has no caller. Acceptance: one package for the file's words; words only the file and its library callers (process-command.f, process.f) use go private or become that package's public words with their callers named; dead words go; the public list goes to the critic and then to Joel for approval (public words, card section 1) before a worker starts. Files: src/lib/process-env.f, its callers (rg PROC-ENV- src tools test), src/lib/process-env-test.f, test/process-image-subject.f. Verify: process-env-test.f, process-command-test.f, test/process-image-subject.f's row. Depends: habu-a-later-cmd-377637bf. Ownership: process-env.f. Claim: unassigned.

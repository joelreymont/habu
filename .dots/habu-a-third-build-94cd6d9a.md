---
title: A third build in one out root fails with E-STALE
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:34:16.692317+02:00"
---

Problem: three builds through tools/hb-build.f in one out root fail on the third with rc 74 and 'threw -9692 LINKER:E-STALE' when the first source requires src/lib/fs-mutate.f, the second requires src/lib/at-exit.f directly and the third requires fs-mutate.f again; on the engine before and after 78d0aacb. Not every alternation does it: fs-mutate.f, string.f, fs-mutate.f builds. Sources: /Users/joel/Work/reviews/habu-10x-2026-11-10/cells-review/st/ (cleanup.f, actions.f, cleanup.f in that order). Inferred by the reviewer, not shown: this is the E-STALE residual habu-file-objects-l6-c4ee4e01 names. Found by the review of habu-a-process-cell-9ddedca3. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names the cause as path:line (which saved object is judged stale and why); then the sequence builds three times, or the cause is recorded in habu-file-objects-l6-c4ee4e01 as an acceptance case if that dot's change is what removes it. Files: src/habu/linker.f, tools/hb-build-lib.f. Verify: the three builds in a fresh out root on bin/hb; a case in test/file-objects.f. Depends: none. Ownership: the file linker. Claim: agent=bob workspace=.jj-ws.noindex/bob-stale

---
title: Root three test fixtures under HB_TMP
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:05.356928+03:00"
---

Problem: test/exit-hook-test.f:52 PREPARE, src/lib/net/http-test.f:577 MAKE-STATIC and src/lib/net/http-test.f:1081 SAVE-STDERR make their scratch roots with TMPDIR-MKDIR, which nobody reaps (src/lib/fs-mutate.f:592-598), instead of HB-TMP-MKDIR, whose tree goes away with the HB_TMP the pool owns even when the row is killed. The HB_TMP census on the env commits listed them (~/.cache/tmp/jerry-census-run2/). The other TMPDIR-MKDIR callers each state a measured reason: test/nf-path-test.f:70-78 (path length) and the socket roots test/gate-pool.f:499, test/db/pg-cluster.f:187, src/lib/fs-mutate-test.f:234. Acceptance: the three roots come from HB-TMP-MKDIR, or each keeps TMPDIR-MKDIR with a measured reason beside the call; a row killed mid-run leaves nothing under TMPDIR from them. Files: test/exit-hook-test.f, src/lib/net/http-test.f. Verify: bin/hb --load test/exit-hook-test.f and src/lib/net/http-test.f with a fresh HB_TMP and a fresh TMPDIR, both rc 0, TMPDIR empty afterwards. Depends: none. Ownership: those two files (http-test.f is under carl's http ruling: never raise deadlines). Claim: unassigned.

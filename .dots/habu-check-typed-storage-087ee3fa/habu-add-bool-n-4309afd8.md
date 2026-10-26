---
title: Add bool>n to the prelude
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T18:40:25.333903+02:00"
closed-at: "2026-10-26T22:10:27.591935+02:00"
close-reason: Landed in batch 1 on master 283e1aaf
---

Joel approved 2026-10-26: public bool>n ( bool -- n ), true -1 false 0, in src/lib/prelude.f; replaces checker Q-FLAG>N. The 1/0 helpers (http-request, curl FLAG>N; ir/schema BOOL>N) encode C's 1 and stay. Migrations (mbool) use it. Workspace carl-booln on 47fb81ee; brief /Users/joel/.cache/tmp/carl-file/brief-booln.md.

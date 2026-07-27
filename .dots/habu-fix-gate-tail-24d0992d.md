---
title: Fix gate-tail-process standalone entry
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T11:42:29.013318+02:00"
---

test/gate-tail-process.f is red run standalone on master (E-UNDEFINED: 0<>, rc 70): it never requires lib/prelude.f and only works when test/gate-stdlib.f happens to have loaded the prelude first - a standalone entry that cannot stand alone is a broken seam, found and reproduced in the main tree during internal-word-gate packaging (its working invocation is bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- tail-process). Owned result: make the file's require closure complete so the documented standalone bin/hb --load path is green, without changing the pooled-slice behavior; acceptance = standalone rc 0 AND the tail-process slice green AND a require-closure check (standalone-load-test.f pattern) covering it so the class cannot silently return.

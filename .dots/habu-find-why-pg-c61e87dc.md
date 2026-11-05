---
title: Find why pg fails under a long TMPDIR
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T21:39:39.576866+02:00"
---

Problem: the pg suite failed (exit 67, 'pg-kill-test REPORT-READ: a deadline expired', then 'hb: uncaught throw code -2502') under TMPDIR=/tmp/jerry-prune/fold3/f1-tmp at 0a92202d and at F1 (/tmp/jerry-prune/fold3/f1-proof.log), and passed at the same commits with the default TMPDIR (/tmp/jerry-prune/fold3/f1-pg-deftmp.log). Suspected, not proven: test/db/pg-kill-test.f:158-160 says every pool level nests TMPDIR deeper and postgres refuses a socket path over 103 bytes on macOS; pg-kill-test runs a second pool (GT-POOL-START :156), whose socket root (test/gate-pool.f:491-494 GT-POOL-SOCK-ROOT-NEW) is made under HB_SOCK_TMP or TMPDIR. Acceptance: a reproducer with a long TMPDIR, the cause stated from its output, then the fix at the layer that is wrong (the socket path must not grow with TMPDIR's depth), and pg passing under the long TMPDIR. Files: test/db/pg-kill-test.f, test/db/pg-cluster.f, test/gate-pool.f. Verify: env TMPDIR=<a path of about 40 bytes> bin/hb --load test/run.f -- pg. Depends: none. Ownership: test/db/, test/gate-pool.f. Claim: unassigned.

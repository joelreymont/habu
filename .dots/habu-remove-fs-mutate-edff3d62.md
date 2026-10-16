---
title: "Remove fs-mutate-test's root after a failed case"
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:05.369301+03:00"
---

Problem: src/lib/fs-mutate-test.f:370 FMT-PREPARE registers the TMPDIR root (FMT-ROOT!, :231-234) with CLEANUP-DIR+, which removes it only when empty, and the fixture files inside it are registered only in FMT-TEST-CLEANUP (:1201-1214) at the end of the run. A case that throws before then leaves its files, the exit-time CLEANUP-RUN cannot remove the non-empty root, and it stays under TMPDIR, which nobody reaps. Observed while landing the HB_TMP fixes. Acceptance: a run that throws in any case after FMT-PREPARE leaves no habu-fs-mutate* directory under TMPDIR; a passing run behaves as now; the HB_SOCK_TMP path (a root the pool owns) is unchanged. Files: src/lib/fs-mutate-test.f. Verify: a copy with a throw injected after FMT-PREPARE, run with a fresh TMPDIR, leaves it empty; bin/hb --load src/lib/fs-mutate-test.f rc 0. Depends: none. Ownership: src/lib/fs-mutate-test.f. Claim: unassigned.

---
title: "Own a killed pg case's TMPDIR in the killer"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:30:07.123937+02:00"
---

Problem: test/db/pg-cluster.f kills a case at its HB_PG_CASE_MS deadline; a killed process runs no exit hook, so the temporary directories it registered with FS:CLEANUP-TREE+ stay (seen: a Maki review case that timed out left /tmp/maki-jj-author-360810645003000-0; reported by tim). src/lib/fs-mutate.f names the fix: the killing process gives the case a TMPDIR it owns and removes that tree after the kill. Acceptance: pg-cluster.f runs each case under a per-case TMPDIR it creates and removes after a kill or a normal exit; a regression case that is killed at its deadline leaves no directory; no Maki change. Files: test/db/pg-cluster.f, src/lib/fs-mutate.f if the owner helper is missing. Verify: the pg-cluster rows; the regression case. Depends: none. Ownership: test/db/pg-cluster.f. Claim: unassigned.

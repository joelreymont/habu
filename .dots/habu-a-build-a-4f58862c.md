---
title: A build a FAIL-CLOSED exit refuses leaves its scratch directory in TMPDIR
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T17:11:58.070214+02:00"
---

Problem: tools/hb-build.f makes a private directory under TMPDIR (tools/hb-build-lib.f:223-228) that only the exit hook removes; a refusal that ends through FAIL-CLOSED (src/habu/outer.f:388-398: SEALED-FAIL, TASK-GUARD) clears EXIT-HOOK-CELL before it dies, so the hook never runs and an empty hb-build-native-<n>-0 directory stays in the caller's TMPDIR. Reproducer: a build of test/fixtures/file-objects/seal-main.f (after habu-a-load-accepts-db95cbfb lands) with TMPDIR set to an empty directory. Found by the third re-review of the seal commit (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-rereview3.md finding 1, outside the commit). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: every exit path of a build that skips the hook, as path:line, and where the directory should be removed on each. Acceptance: a refused build leaves nothing in TMPDIR; test/file-objects.f's SEAL-CASE asserts it, and BUILD-RC drops its TMPDIR once Q's build (kept-zip.f's open archive) also leaves nothing in TMPDIR. Files: named by the hunt. Verify: the reproducer. Depends: habu-a-load-accepts-db95cbfb landed. Ownership: the build tool's scratch. Claim: unassigned.

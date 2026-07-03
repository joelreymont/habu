---
title: Emit open-failure diagnostic in baked source read
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T21:28:22.037173+02:00"
---

Problem: when bin/hb runs outside the repo, the baked source prefix (PFX-LOAD-BASE-FILES) fails to open and the engine exits 74 SILENTLY - the sopenerr path in src/habu/habu2.f EMIT-SOURCE-READ does '0 74 MOVN ... NR-EXIT-GROUP SYS' with no message. tools/diagnose-hb.f (landed) diagnoses this after the fact, but the engine itself should say what failed. Fix (baked, fixpoint-risking): extend the sopenerr path to write 'hb: cannot open <path>' to stderr (fd 2, write syscall; the failing path pointer is in the source-read registers at that point - map them first with lldb on a deliberate failure) before exit(74); optionally resolve the prefix relative to argv[0] dirname or $HABU_ROOT before failing. Regression: run bin/hb from an empty cwd outside the repo, assert stderr names the file and rc=74 (see tools/diagnose-hb-test.f for the harness pattern). Full fixpoint + gate required.

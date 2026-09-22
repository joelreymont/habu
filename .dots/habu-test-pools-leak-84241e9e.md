---
title: Test pools leak their temp trees and ignore HB_TMP
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T10:41:14.484143+03:00"
---

Killed or failed test runs leave their temp trees behind, and the makers put them under $TMPDIR instead of HB_TMP. Measured 2026-09-22 under /tmp (the shared RAM tmpfs, whose inode cap - not its bytes - is what a run exhausts): 247 gate-pool-test-battery-*, 251 habu-src-shape-*, 167 hb-build-native-* and 45 habu-native-suite-* dirs older than 3 h. Makers: test/gate-stdlib-lib.f:110 and test/gate-pool-test.f:138 (GT-START, lib/test/runner.f:83: TMPDIR-MKDIR + CLEANUP-TREE+, removed only when GT-CLEANUP runs, so a red report that dies, an uncaught throw or a timeout kill leaves the pool); tools/hb-build-lib.f:336-338 (HBB-PREPARE-TMP: MAKE-TEMP-DIR under BF-TMP$ or TMPDIR-MKDIR, CLEANUP-TREE+, removed by HBB-CLEANUP - the 167 leaks say an exit path skips it); lib/test/src-shape-test.f:35 (SST-SETUP: TMPDIR-MKDIR and no cleanup registered at all). A run with HB_TMP=/tmp/hazel-die-nl/tmp still produced /tmp/habu-native-suite-409123619450333-17/pool-... (its run.f log). Acceptance: each maker removes its tree on every exit path the process controls (green, red report, die, uncaught throw); the parent that reaps a timed-out or killed child removes that child tree; all four makers place their tree under HB_TMP when it is set; a test kills a child mid-battery and finds nothing left under HB_TMP.

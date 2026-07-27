---
title: Fix snapshot trailer corruption
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T18:16:53.142846+02:00"
---

Pre-existing red on master e34902b3, proven on a clean checkout with a real copied bin/hb and wiped HB_TMP (not symlink or temp-state artifacts): the standalone stdlib gate (bin/hb --load test/gate-stdlib.f, SUITE-ALL) fails owner-wid-internal - the self-hosted image refresh produces a snapshot image whose child run dies 'hb: snapshot trailer corrupt' (owner-wid-child abnormal run-file on HB_TMP hb-new --load test/owner-wid-state.f; asserts 1/2/3 fail with values 1 / false / 14). The resident test/run.f path stays green, so the defect is in the snapshot write or read path exercised only by the standalone image-refresh harness. Owned result: root-cause the trailer corruption (writer, reader, or refresh harness), fix at the owner, and add a regression that round-trips a snapshot through the exact refresh path and validates the trailer before any child runs it. Debugger-evidence-first per the native crash RCA rule; the image dumpers in docs/debugging.md are the starting tools. Blocks any full-standalone-gate green claim on master.

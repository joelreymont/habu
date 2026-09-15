---
title: Build the cold fixture engine once per gate
status: open
priority: 2
issue-type: task
created-at: "2026-09-15T19:15:20.180441+03:00"
---

Problem: thirteen gate suites (aot-wid-*, aot-data-*, aot-wide-format, aot-chain-capture, field-proj-boundary, native-window-owner and the stripped-* family) each spawn test/aot-wid-build.f or a window build that compiles the whole core prefix through the optimizing chain: 213 s per build alone, 26 suites over a minute, twelve load timeouts and a 95-minute gate on the fF pin (scratchpad gate-fF.log). Acceptance: one cold fixture engine (and one hb-pwid variant per mode that differs) is built once per gate run into the gate's HB_TMP and shared by every suite that needs it, keyed by the tree's content key so a stale artefact is never reused; suites that must build a variant build only their delta; per-suite child bounds go back to seconds; gate wall time reported before/after. Files: test/gate-pool.f, test/gate-stdlib-cases.f, test/aot-wid-build.f, test/aot-data-span-forge.f, test/aot-wide-format-suite.f, test/aot-chain-capture-suite.f, test/native-fixture-write.f, lib/engine-candidate.f. Verify: bin/hb --load test/run.f wall time; every suite green. Depends: habu-replace-per-transfer-8523fb98 (guard removal shrinks the build first). Ownership: gate. Claim: unassigned.

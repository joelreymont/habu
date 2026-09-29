---
title: Bound engine-suite HIDX churn and fold exit-hook builds
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:03:11.965916+02:00"
closed-at: "2026-09-29T23:07:09.054269+02:00"
close-reason: "Landed 6a8a2a83 (review PASS): HIDX churn bounded past LOAD-MAX (engine 23.8->1.4 s), private temp root, exit-hook folded into stripped-image (25.8->0.8 s), address-cell tier placement, catch-stale check.f children and two redundant rows removed. catch-stale image fold into control-capture declined (different engine). Engine-stack item was already done by c9ee0d90. Gate 503/503."
---

Problem: test/engine-suite.f ES-HIDX:CHURN (2183-2216) is 19 s of an 18.9 s run: the same key each cycle walks a growing stale chain; fixed /tmp paths (1889-1921) race between gates. test/exit-hook-test.f spends 27 s of 27.6 s on one tools/hb-build.f spawn for two stripped cases that stripped-lifecycle-prepare's subject already builds. address-cell-tasks/-index/-recovery set the tier before require lib/test.f and lib/task.f. engine-stack-lifecycle.f:208 runs its suite on require, so engine-stack-jit/wide/debugger run it again. catch-stale: two check.f children mirror in-process JSON assertions; image section folds into control-capture. Delete dictionary-record-shapes and argv-stdlib-capacity-after-dashdash (covered by lib/argv-test.f:95-101). Evidence and measured designs: ~/.cache/tmp/kestrel-gate/test-review/L5-test-a-f.md. Acceptance: churn bounded by HIDX:LOAD-MAX cycles or a spread that crosses it, all three assertions kept; paths under HB_TMP; exit-hook's stripped cases asserted on the prepared subject; the rest as listed with witnesses named. Files/Ownership: test/engine-suite.f, test/exit-hook-test.f, test/address-cell-*.f, test/engine-stack-*.f, test/catch-stale*.f, test/dictionary-record-shapes*.f, test/argv-stdlib-capacity*.f, registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: Build stripped-image fixtures dot (exit-hook reuses its subject). Claim: unassigned.

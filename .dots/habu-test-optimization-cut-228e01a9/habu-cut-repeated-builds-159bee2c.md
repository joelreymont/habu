---
title: Cut repeated builds and fixed waits in native gate rows
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.053665+02:00"
---

Problem: native-gate-aot-positive makes 18 builds (~10 s each), LAYOUT-STORE a subset of LAYOUT-FETCH, DATA/DATA-WINDOW fit in BUNDLE, ABS-CHAIN/ADR-MEMBER belong in gate-aot-negative's in-process forks; native-gate-debug reruns test/prop-test.f, does 3M dlsym (100k suffices) and reruns identical programs; native-build-entry loads tools/native-build.f (7.2 s) before rejecting argv, twice; process-wide-image is a subset of process-image-subject; fixed sleeps in gate-pool-test, proc-capture-signal, pty-tty. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L6-test-g-z.md. Acceptance: each listed redundancy removed with its surviving witness named; tools/native-build.f validates argv before loading its closure; waits become events or minimal bounds. Files/Ownership: test/gate-aot-*.f, test/gate-debug*.f, test/native-build-entry*.f, tools/native-build.f argv entry, test/process-*image*.f, test/native-resource-image*.f, test/gate-pool-test.f, test/proc-capture-signal*.f, test/pty-tty*.f. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: unassigned.

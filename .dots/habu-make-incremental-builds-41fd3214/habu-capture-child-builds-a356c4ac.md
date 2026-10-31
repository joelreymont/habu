---
title: Capture child builds whole in fixed-buffer tests
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T10:27:49.435752+02:00"
---

Found by the fdiag review: these tests spawn tools/native-build.f with a fixed capture and inherit the outer environment, so running them with HABU_NATIVE_CACHE_DIAG=1 (about 10 MB of stderr per build) should die E-PROC-TRUNCATED as test/prefix-provided-unfilled.f did: test/product-host-identity.f (CAP $4000, :21 :111 :124), test/native-build-entry.f ($4000, :21 :48 :55), test/host-checker-row-e2e.f ($4000, :21 :119 :132), test/native-layout-transition.f ($2000, :15 :34/:41 :52/:59), test/compiler/native-hookless-reject.f ($4000, :23 :362 :380), test/compiler/native-build-edit-e2e.f ($20000, :40 :372/:379 +4), test/source-root-exe-test.f (IO-CAP $10000, :31 :684-690). Unconfirmed: test/cold-naming-test.f, test/native-window-owner.f. Not measured. Acceptance: each runs under DIAG with rc 0 through CMD's growing capture, as prefix-provided-unfilled.f now does. Claim: unassigned.

---
title: Discharge the native-chain test engine seam
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T22:40:57.854408+02:00"
---

The test/compiler native-chain suites reach the engine through three boundaries the checker cannot express, and each one is a TRUSTED word in a test file: 'evaluate' (EV/EV-N/EV-CATCH in native-chain.f, native-clobber.f, native-feed.f, native-inline.f, native-migrate.f, native-publish.f and native-vocab.f), the checker's source-tape observer entrypoints (FAKE-SCAN/FAKE-TOKEN in native-feed.f), the code-address read CODE-A in native-publish.f, and the routine-entry words in native-run-fixture.f - EXEC0..EXEC3 over ffi-call-bounded and ENTER0..ENTER3/ENTER-SPAN/ENTER-SPAN1 over execute. These 28 sites are owner-of-record for this dot in TRUSTED.md's inventory classification block (class test-metaprog); patch32 in the same fixture (POKE) stays with habu-checker-capability-gate-14022ba9, which is owner-of-record for that boundary. What this dot has to decide, per boundary: whether the checker can be taught to type it (an xt with a declared effect for execute; a typed C-ABI call for ffi-call-bounded; a compile-time source form for evaluate), or whether it is permanent by design and belongs behind a cap: anchor with audited documentation. Until it is decided the rows must not be reassigned to the narrow feature dots that happen to touch these files - those close, and a closed owner leaves the boundary unowned. Found while reconciling the drifted ratchet suites (habu-reconcile-the-drifted-48eefbd9): the 91 unclassified sites the TRUSTED.md ratchet reported were the native-chain campaign landing test files without their classification rows.

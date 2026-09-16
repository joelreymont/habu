---
title: Encode the captured DATA image with variable gaps
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.403455+03:00"
---

Problem: after the run merge at the format arithmetic minimum (ec569af9: 8-byte rows, gaps under 8 carried as zeros) the captured DATA still costs 3,038,770 image bytes for 1,121,787 non-zero bytes: 84,724 rows plus 990,670 carried zeros, because the checker signature store interleaves non-zero cells with zero cells. A fixed (offset u32, length u32) row cannot do better; the next step is a denser encoding: varint gap-and-length pairs, or a per-page presence bitmap with dense payload, chosen by measurement on the release heap (the threshold curve in the merge commit is the baseline). Acceptance: one encoding, its reader in the stripped startup (src/habu/aot-lib.f EMIT-DATA-COPY, pinned by test/gate-aot-image.f CHECK-COPY, so the pin moves with it) and its writer at capture, engine size reported before and after with tools/engine-size.f (target: the DATA class under 1.5 MB), boot time of a trivial program unchanged within noise (120 runs, median), byte fixpoint, full gate, stripped-application suites; the Gforth seed does not write this format (state it). Files: src/habu/aot-decl.f, aot-capture.f, aot-lib.f, tools/engine-size.f, test/gate-aot-image.f. Verify: engine-size; tools/native-build.f fixpoint; test/run.f. Depends: habu-drop-private-signatures-974304d0 first (it removes half the interleaved cells, so measure after it). Ownership: capture format. Claim: unassigned. Parent: habu-ship-only-the-d7d38629.

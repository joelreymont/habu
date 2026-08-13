---
title: "Let a no-exit routine's memory order go unread"
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T17:32:52.411292+02:00"
---

Found by the again landing (c42f89ec): a begin...again body that neither calls nor touches memory threads no order through its loop, so the a64.dtake minted at entry is read by nothing and regalloc-verify.f ORDER-VALUE-CK (the k USES-AT 1 < rule) refuses E-A64RAV-ORDER. The correct statement, per the finding: a zero-reader order value is legal exactly when no terminator that LEAVES the routine is reachable from its defining block - REACH-FILL and RET-ORD in the same file already express reachability and leaving. This is the machine-dialect/spill owner's invariant (native-dead-path.f section 7 assigns this class there); a wrong validator rule ships miscompiles, so it was correctly NOT patched in the again lane. Population today: zero (every begin-again in the tree calls or touches memory); pinned with a live twin in test/compiler/native-again.f. Files: src/compiler/native/regalloc-verify.f. Depends: none.

---
title: Derive baked build facts from the tree not the host
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.734351+03:00"
---

Problem: tools/native-build.f bakes record indices and shim facts read from the HOST dictionary, so an engine built from a changed tree on an unchanged host differs from its own rebuild (K3 vs K4: 76 bytes, record offsets shifted by 3 while the host still carried the transition shims; G6 vs G7 the same pattern after the guard-page words appeared) and the fixpoint needs two builds after every engine change; a stale host can bake wrong indices silently. Acceptance: the baked facts come from the window being compiled, a single build from any valid host is byte-identical to its own rebuild, the fixpoint check becomes one build. Files: tools/native-build.f, src/habu/aot-capture.f, src/habu/habu2.f. Verify: build revision R on host A and on host B (a previous engine), cmp. Depends: habu-bake-prefix-source (tree-relative names). Ownership: hazel line. Claim: unassigned.

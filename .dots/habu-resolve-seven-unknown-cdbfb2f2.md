---
title: Resolve seven unknown-classification workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:35:34.583826+02:00"
---

Workspace forensic sweep 2026-07-18 left seven workspaces UNKNOWN - neither proven superseded nor clearly stranded; each needs an individual call before deletion or recovery: owner-persist 6382c5a0 (OWNER-WID landed but 28 early support/test adds absent - src/core/checker-image.f, checker-types.f, src/habu/aot-cert.f, aot-live.f, owner-wid-aot-seal.f, snap-drive.f, tools/image-doctor.f, many owner-wid tests; likely superseded by the landed aot-capture/aot-closure rework - spot-check the 28 files); checker-current-wid 25f0a3f9 (11 test/checker-arena-* probe fixtures with no checker-arena scaffolding on master; likely orphaned scaffolding from the abandoned WID campaign); catch-return 494ece8b (AOT-CLOSURE landed but this lane's src/core/code-cert-seal.f CODE-CERT sealing and test/aot-closure-probe.f are absent); v2-region 5383275d and v2-toolchain 817a1b8d (package REGION/FUSION-REGION and package TOOLCHAIN + maki/target/toolchain.f absent, but master's mature maki/target and maki/lower likely reworked them under different names); snapshot-core-combined fa74389e (conflicted modifications-only change to src/core/sumtype.f, structures-effects.f, habu2.f for a core record loader seam); primitive-recipes a150c176 (recent Jul-18 undescribed snapshot touching docs/effects.md, test/gate-debug-lib.f, test/prop-test-core.f). Verdict per workspace: superseded (delete), stranded (route to an owning dot), or recover-now. None may be deleted before its verdict.

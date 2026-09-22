---
title: Admit a below-the-mark callee only if the tree names it
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T12:30:30.901838+03:00"
---

Problem (from the bake-facts measurement, report ~/.cache/tmp/hazel-bake-facts/report.md §6 C7, engine 6dc6cd7b at c921e431): src/habu/aot-capture.f ACAP-SITE-BAND (:515-517) admits a window call to ANY host record below the prelude mark on the stated assumption that the target is built from the same prefix (:446-473), and a native build declares an empty band (tools/native-build-core.f:290), so every host record below the rewind point is a legal callee and its NAME is baked for LFIND at the product's boot. The check runs against the host dictionary, not the tree: a stale host still carrying a word the tree dropped (the G6/G7 transition-shim shape) bakes a name the product cannot resolve, and the failure is at the boot of the shipped engine, not at the build. No bytes differ today (A1 == B1); it is a boot-time failure mode read from the code, not yet provoked. Acceptance: a below-the-mark callee is admitted only if its name is in the window's records or the tree's primitive rows (src/habu/prims.f PRIM-SPEC, which habu2.f PRIM-TABLE-COMPLETE already walks at emit time), refused by name otherwise at the build; a regression provokes it (a host tree with one extra prefix word the product tree lacks, called from the window) and pins the refusal text; the cost is one lookup per distinct below-the-mark callee (75 in A1). Files: src/habu/aot-capture.f, tools/native-build-core.f. Verify: the engine chain, tools/engine-size-test.f, test/run.f. Depends: d5df4419 (lands first; same capture file). Ownership: hazel. Claim: unassigned.

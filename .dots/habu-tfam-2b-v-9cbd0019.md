---
title: "TFAM 2b-v(f): boot integration test for protected WIDs"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:34:10.910432+02:00"
---

Split from habu-tfam-2b-v-0a0e48a9 per the 2b-v design report (2026-07-04): candidate-boot assertion that a protected record with wid>255 survives AOT seed capture/restore with full u32 WID, WIDN advanced past registry entries, registry persisted, and a user forge cannot publish into it. Needs a live friend producer: either item 8's generated-ctor-package creation calling PROT-WID-ADD (habu2.f C-PACKAGE-ALLOC-WIDS ~:2878) or a metabuild-only hook in src/habu/stdin.f CAPTURE-REPL (:84) that PROT-WID-ADDs a synthetic wid before ACAP-CAPTURE; assert via the gate-aot harness (test/gate-aot-positive-lib.f), suite test/aot-wid-suite.f wired per GE-TYPE-DECL-SUITE pattern. DEPENDS: 2b-v slices a-e (registry+persistence+guards), item 8 (or the stdin.f hook).

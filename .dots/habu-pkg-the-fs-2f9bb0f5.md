---
title: Package the FS cleanup registry
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T22:25:58.771949+02:00"
---

Problem: src/lib/fs-mutate.f exposes its cleanup registry internals as global words (FS-MUT-CLEANUP-STRIDE/-FIRST/-ROW/-CAP/-DECLARED/-LOCK/-AT/-KIND/-SLOT/-GROW/-ACQUIRE/-RELEASE/-REMOVE/-APPEND/-NAMES?/-FIND/-DROP, FS-MUT-CLEANUP+), so a caller can grow, append or drop outside FS-MUT-CLEANUP-LOCK or write ROW/CAP and corrupt the registry (l3reg major review, finding 1). A word outside a package cannot call that package's private words, so the public API must move into the package. Change: one package FS-CLEANUP owning the registry; internals private; public words for add file/dir/tree, forget, run, reset, at-exit, count and path; migrate every caller (Habu ~130 files; Tender ~72, Maki 5, Zen 1) and drop the global CLEANUP-* and FS-MUT-CLEANUP-* names, no forwarders. Acceptance: no FS-MUT-CLEANUP-* or CLEANUP-* global remains; registry internals resolve E-UNDEFINED outside fs-mutate.f; consumers migrated and proven before the install. Verification: check.f on fs-mutate.f, fs-mutate-test, build-cache-test, process-fork-test, gate-pool-test, image-lifecycle, full registry; Zen/Tender/Maki affected suites.

Joel approved 2026-10-28 (interview, relayed by carl): one FS-CLEANUP package, internals private, no forwarders.

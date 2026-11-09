---
title: Prove sha256-blocks on Linux arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T15:28:40.738304+02:00"
---

Problem: src/habu/habu1.f SHA2-PROBE, (the Linux-only walk past envp's NULL into auxv for AT_HWCAP bit 6, HWCAP_SHA2, called by BSHA256BLOCKS) has never run. tools/next-hb.f builds only its builder's own target, and spark's installed hb (2026-11-01) predates the linker path, so no Linux arm64 hb with the primitive exists. A wrong walk crashes or misreads every hash on Linux; a wrong 0 only falls back to sha256.f's own SHA-BLOCK. Acceptance: on spark (aarch64, /proc/cpuinfo lists sha2), a Linux arm64 hb built from a tree holding commit 'Add the sha256-blocks primitive' answers 1 for one block (create S 64 allot create B 64 allot  S CELL-VIEW B 1 sha256-blocks .), its digests of a file equal sha256sum's, and it rebuilds itself byte for byte. Files: src/habu/habu1.f (SHA2-PROBE,, BSHA256BLOCKS) only if it fails. Verify: the one-block answer, a SHA256-FILE-HEX-IN digest against sha256sum, cmp of two generations. Depends: a linker-built Linux arm64 hb (after arm64 macOS lands: one arch first). Ownership: src/habu/habu1.f SHA2-PROBE,. Claim: unassigned.

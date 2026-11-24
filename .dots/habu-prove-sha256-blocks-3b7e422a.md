---
title: Prove sha256-blocks on Linux arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T15:28:40.738304+02:00"
---

Problem: src/habu/habu1.f SHA2-PROBE, (the Linux-only walk past envp's NULL into auxv for AT_HWCAP bit 6, HWCAP_SHA2, called by BSHA256BLOCKS) has never run. Spark's installed hb carries the primitive (habu1.f:3965 registers it), but nothing shows it answers or hashes right. A wrong walk crashes or misreads every hash on Linux; a wrong 0 only falls back to sha256.f's own SHA-BLOCK. Acceptance: on spark (aarch64, /proc/cpuinfo lists sha2), spark's installed hb answers 1 for one block (create S 64 allot create B 64 allot  S CELL-VIEW B 1 sha256-blocks .), its digests of a file equal sha256sum's, and it rebuilds itself byte for byte. Files: src/habu/habu1.f (SHA2-PROBE,, BSHA256BLOCKS) only if it fails. Verify: the one-block answer, a SHA256-FD-HEX-IN digest (src/core/sha256.f:248) against sha256sum, cmp of two generations. Depends: none. Ownership: src/habu/habu1.f SHA2-PROBE,. Claim: unassigned.

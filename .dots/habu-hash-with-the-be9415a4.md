---
title: Hash with the SHA instructions on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.513473+01:00"
---

Problem: object checksums take 14.4 ms of a warm engine build on arm64, which hashes with the CPU's SHA instructions; x86-64's sha256-blocks has no such path and takes about 395 ms. Acceptance: x86-64 sha256-blocks uses the SHA extensions, with the same digests (the SHA-256 test vectors). Depends: none. Claim: unassigned.

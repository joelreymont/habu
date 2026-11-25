---
title: "Compress SHA-256 with x86's instructions"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T20:52:22.008075+01:00"
---

Problem: the x86-64 kernel's sha256-blocks answers 0 (src/habu/kernel-x64.f:3875), so src/core/sha256.f compresses every block in Habu, while arm64 uses the CPU's SHA-256 instructions (src/habu/habu1.f BSHA256BLOCKS). In a warm build of Maki SHA-BLOCK and its byte reads are 36% of the x86 samples against 2.5% on arm64, which makes the build 2.2 s against 1.4 s. Acceptance: sha256-blocks runs the SHA extensions where CPUID shows them (and SSSE3 and SSE4.1) and answers 0 elsewhere; digests are unchanged; Maki's warm build on pulsar takes no more user time than on spark. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-sha.

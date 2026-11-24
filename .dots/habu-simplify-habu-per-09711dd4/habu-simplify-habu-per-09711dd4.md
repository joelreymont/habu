---
title: Simplify Habu per the repo review
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:13:24.449693+03:00"
---

Campaign. Joel asked (2026-10-15) for a deep review of the repo for inefficiency, overengineering and code smells, then: dot and implement, coordinate with carl. Fable reviewed ten lanes at master a58b751e. Each kept finding is a child dot; one child = one commit, reviewed by Fable code-reviewer when nontrivial, landed on master and closed. Not reviewed: tier-1 compiler internals (src/compiler/native, select/regalloc/emit, encoders), left out for a one-pass replacement that is now withdrawn; documented stdlib modules without in-tree callers (map.f, crypto/evp.f, PROCESS-PTY, genio, ffi-callback, zip, task mailbox and semaphores, fmt SB-SCI/SB-SCALED, type/extent-role.f, public surface); src/arch/tic6x and arm32 (substrate for habu-campaign-c6-targets). Build-tooling findings B4 (source SHA hashing) and I6 (native-layout raw offsets) went to carl. Coordination: carl-final-core carries a long unlanded chain touching checker.f, type-family.f, habu1.f, habu2.f, include.f, error.f, hb-build-lib.f; children owning those files start from master after that chain lands. Ownership: jerry lead.

Overengineering to fix (Joel, 2026-10-25: note the examples in dots for later fixing). Each is a second copy of something the engine already has; its dot holds the evidence:
- checker's second dictionary: habu-stop-the-checker-4c138a8c
- types and constants written twice: habu-take-a-word-01856558, habu-one-def-for-91f66b4c
- overlapping restriction marks and owner rows: habu-move-engine-internal-fc2ba551, habu-delete-the-internal-3a818b8a
- keyword ladders: habu-dispatch-do-tok1-e63cab6f
- profiler as two assembler twins: habu-write-the-profiler-b7cacfe0
- verify-source re-implements the front end: habu-check-src-through-1198f4ed

---
title: Simplify Habu per the repo review
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:13:24.449693+03:00"
---

Campaign. Joel asked (2026-10-15) for a deep review of the repo for inefficiency, overengineering and code smells, then: dot and implement, coordinate with carl. Fable reviewed ten lanes at master a58b751e. Each kept finding is a child dot; one child = one commit, reviewed by Fable code-reviewer when nontrivial, landed on master and closed. Not reviewed: tier-1 compiler internals (src/compiler/ir, src/compiler/native, select/regalloc/emit, encoders), left out for a one-pass replacement that is now withdrawn; findings that restate architecture.md target rules the code does not yet follow (second symbol table, keyword ladders, PRIM rows, two interpreters, JIT folder, capture decode passes, profiler written as assembler, verify-source mirror, lints as dictionary walk); documented stdlib modules without in-tree callers (map.f, crypto/evp.f, PROCESS-PTY, genio, ffi-callback, zip, task mailbox and semaphores, fmt SB-SCI/SB-SCALED, type/extent-role.f, public surface); src/arch/tic6x and arm32 (substrate for habu-campaign-c6-targets). Build-tooling findings B4 (source SHA hashing), I6 (native-layout raw offsets), I8 (build-fixpoint spawn wrappers) went to carl. Coordination: carl-final-core carries a long unlanded chain touching checker.f, type-family.f, habu1.f, habu2.f, treeshake.f, include.f, source-producer.f, aot-capture.f, errors.f, hb-build-lib.f, tools/json.f; children owning those files start from master after that chain lands. Ownership: jerry lead.

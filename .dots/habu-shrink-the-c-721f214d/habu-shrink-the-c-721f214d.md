---
title: Enforce compact native engine architecture
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-07T07:57:05.323412+02:00\""
---

Supersedes the obsolete 100000-byte gross-file goal with a payload contract derived from the retained AOT REPL architecture and Mach-O/ELF writers. A sub-100000-byte Mach-O requires __text <= 77824, but the compact pre-type AOT engine already measured about 89992 bytes; reaching 100000 again requires a separate AOT redesign, not normalizing compiler growth. Current master measures __text=132576 and macOS bin/hb=165367; the size workspace measures 132392 after commit cf9fab59 made shared engine helpers AOT-closed. Hard contracts: immutable __text <= 110592 on every target; macOS file <= 132343; Linux file <= 114880 subject to exact Orin verification; native type integration <= 8192 total, with compile/adt <= 3072 and compile/p2wide <= 3072. The exact per-platform baseline remains a no-growth ratchet and must fall on every shrink. First remove the proven primitive-guard duplication, then subdivide the mixed definition/semicolon/keyword/runtime regions, detect repeated emitted blocks, and simplify only measured duplication or needless control structure. Acceptance: current region map sums exactly to the emitted payload; immutable ceiling and exact baseline fail independently; macOS and Orin artifacts meet their ceilings; byte-exact before/after evidence for every cut; native fixpoint x2, bootstrap recovery, full native, maki, ptx-stdlib, host/filemap/dot gates green. Files: src/habu/engine-size.f, src/habu/habu1.f, src/habu/habu2.f, test/gate-build-size.f, test/gate-engine-lib.f, docs/size-rca.md, and focused Habu-native analysis tools.

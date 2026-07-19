---
title: Package ARM64 disassembler
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:50:59.680033+02:00"
---

Current master census: src/arch/arm64/disasm.f:6-78 is correctly absent from default bin/hb and loaded explicitly by jitdump/imagedisasm, but each loading image receives 23 generic globals including U64@, U32@, FRD, FRN, OP?, SPC, mutable decoder state, and a raw table. Only DIS1 and DISASM cross file boundaries. Put the module in package ARM64-DISASM; export ONE and RANGE, keep the table, field extractors, byte readers, formatting, and decoder state private, and update tools/jitdump-core.f, tools/imagedisasm.f, tests, and docs to qualified calls without aliases. Preserve every decode/unknown-op byte and keep the default binary exactly unchanged. Prove old generic globals and qualified internals reject, public calls certify, all instruction goldens and unknown rendering remain byte-identical, concurrent/nested decoding cannot share corrupt scratch, and explicit-load behavior remains deterministic. Measure loaded dictionary-name/JIT/DATA/CODELEN and decode throughput before/after; default bin/hb size/dictionary must be identical, proving the disassembler stays on demand. Coordinate consumer packaging with habu-pkg-reusable-tools-84cca130.

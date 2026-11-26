---
title: Convert the last seven split-str files
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:19:46.544488+01:00"
---

Problem: src/core/sha256.f, src/core/arena-pages.f, src/habu/intern.f, src/habu/main.f, src/lib/c2-memory.f, src/lib/process-tree.f and src/lib/span.f still start with the split-str marker, so they keep ^u8 n rows Joel ruled out (2026-11-19: str or vector); the marker machinery cannot go until they convert. Two rows must change with them: the primitive sha256-blocks (src/habu/prims.f) takes its bytes as a str, not a block count, in all three bodies (habu1.f BSHA256BLOCKS, kernel-x64.f SHA256-BLOCKS-BODY, Wasm WKWORDS:SHA-BLOCKS); and a C2 view (c2-memory.f's stow rows, casts and WITH- scope words, checker.f's scope kinds) is a SPAN:span<u8> where it is written and a str where it is only read. Acceptance: none of them carries the marker; the engine fixpoint and the suites that cover them pass on arm64 and x86-64. Depends: none. Claim: unassigned.

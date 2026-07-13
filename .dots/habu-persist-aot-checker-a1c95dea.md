---
title: Persist AOT checker schemes transactionally
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T13:23:14.132614+02:00\""
---

Problem: snapshot AOT code survives source replay removal while checker USIGS remains mmap-backed outside snapshot DATA, so AOT-only words lose certified effects and fail checked lookup. Fix: encode canonical SHA-256 scheme graph rows plus TYPE/FAMILY identities in the AOT envelope; validate all bounds/tags/refs/cycles and package visibility before mutation; reserve transaction capacity without semantic highwater changes; allocation-free commit effects/symbols/control metadata; warm LIVE validation with zero append; bind the materialized interval and scheme frame into the persistent AOT certificate. Include quotation exceptional roots G/H and fail closed on malformed flags/roots. Acceptance: no REPL source replay; fresh and warm snapshot processes keep CP/NDICT/UEND stable; checked AOT-only calls work; corrupt scheme/hash/identity/graph/role/cert rejects before mutation; pre-v3 snapshots reject; native/bootstrap/fixpoint parity. Files: src/core/checker-image.f, src/habu/aot-scheme-capture.f, src/habu/aot-scheme.f, src/habu/aot-capture.f, src/habu/habu1.f, src/habu/habu2.f, bootstrap/cg/forth.fs, tools/build-fixpoint.f, tests/manifests/FILEMAP. Depends: habu-owner-seal-persist-1f23e205. Ownership: checker scheme artifact and transaction; coordinate habu2/aot-capture overlaps with owner persistence.

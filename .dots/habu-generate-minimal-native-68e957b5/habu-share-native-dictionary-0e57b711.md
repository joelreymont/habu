---
title: Share native dictionary name comparator
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T19:54:06.374597+02:00"
---

Measured at master 3909bbac. The same case-insensitive byte-comparison loop is emitted four times in src/habu/habu1.f: duplicate checking around 2726-2734, qualified private-tail lookup around 2849-2857, hash-probe validation around 2887-2895, and authoritative linear FIND around 2920-2928. Each loop is 20 ARM64 instructions/80 bytes: index bound, two byte loads, two five-instruction ASCII A-Z folds, compare/mismatch, increment/backedge. Total duplicated loop code is exactly 320 bytes across primitives/find and primitives/hash-index. Root cause: every search path owns an inline comparator instead of one target routine. Fix: emit one callable folded-name-equality helper with an explicit pointer/length/result register ABI and clobber contract; make duplicate, qualified, hash, and linear paths call it while retaining their own candidate filters and next-record/probe control. Acceptance: before/after disassembly proves four 80-byte loops become one helper plus four measured call/result stubs with net CODELEN shrink; inline and external names, mixed case, zero/long lengths, qualified private/public lookup, wordlist mismatch, duplicate rejection, stale/truncated hash entries, collisions, exhausted probe, and linear fallback preserve exact results; FIND performance stays within PERF-VERDICT; bootstrap mirror, clobber lint, AOT, snapshot, fixpoint x2, both targets, full dictionary/checker gates, and exact ratchets pass. Files: src/habu/habu1.f, bootstrap/cg/forth.fs, dictionary/hash/package tests, performance verdict, engine-size attribution, and size gates.

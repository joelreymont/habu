---
title: Use coordinator depth for dictionary frames
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T23:52:12.689987+02:00\""
closed-at: "2026-07-23T00:38:54.371710+02:00"
close-reason: Landed as 1ef723720909 on verified master; fresh destruction READY; production depth-growth and native DATA rollback mutations red; focused transaction, exact diff lints, full Maki/lint-libs, host/filemap/dot, and native port gate green.
blocks:
  - habu-bound-declaration-participant-9967c968
  - habu-make-event-snapshot-0b239a3a
---

Problem: GENERATED-DECL-DICTIONARY FRAME-N duplicates GENERATED-DECL coordinator depth, and its rejecting FINALIZE can fail after publication. The duplicate cannot be driven out of sync through any legitimate owner operation, so a test setter would create false authority. Exact contract: remove FRAME-N, REQUIRE-DEPTH, and the public DEPTH query. The coordinator callback depth is the sole frame identity and indexes row depth minus one. SNAPSHOT validates positive bounded depth, ensures capacity before mutation, and records the exact dictionary, code, and data high-waters in that row. ROLLBACK restores that exact row. PREPARE, COMMIT, and FINALIZE are total and mutation-free because no second depth exists to validate or release. SNAPSHOT-RESET requires only GENERATED-DECL depth zero. Preserve nested provisional publication, outer rollback, monotonic WID non-reuse, capacity failures before writes, and snapshot reset. Package owner: GENERATED-DECL-DICTIONARY only. No compatibility depth alias, raw setter, test installer, PRODUCT/SUMTYPE, or second coordinator state. Acceptance: nested clean success, inner success followed by outer rollback, body and prepare failures, repeated reuse, and capacity edges restore exact dictionary/code/data high-waters; old GENERATED-DECL-DICTIONARY:DEPTH rejects; mutations selecting the wrong depth row fail. Files: src/core/generated-declaration-dictionary.f and test/generated-declaration-transaction-suite.f only. Verify: production generated-declaration transaction suite, shared candidate validation, exact typed-local/package/trust gates. Depends: atomic capacity and event snapshot fixes. Ownership: dictionary frame indexing and release only. Claim: agent=release_dictionary workspace=.jj-ws/habu-release-dictionary.

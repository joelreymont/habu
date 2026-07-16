---
title: Mirror defer/is into stage0 bootstrap forth.fs
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T15:20:58.808385+02:00\""
blocks:
  - habu-migrate-engine-hooks-e4f31fc6
---

Capability dot unblocking stage 2 of the stored-xt soundness program (habu-migrate-engine-hooks-e4f31fc6, RCA in habu-checker-exec-of-5923c543). The stage0 bootstrap mirror bootstrap/cg/forth.fs has NO defer/is (grep proof: dictionary word list forth.fs:75), so defer-shaped hooks in src/core/* cannot be baked without breaking no-binary recovery. Work: translate the native C-DEFER/J-IS implementation block from src/habu/habu2.f (~lines 2263-2380: defer create+DOES>-style dispatch cell, is/action-of resolution, defer! defer@) plus the keyword/prefix table rows into bootstrap/cg/forth.fs, keeping the SEVEN-mirror prefix-order sync (habu2.f 3 PFX tables, forth.fs 3 tables, bootstrap.sh, build-fixpoint.f stage lists, boot-pin.f, diagnose-hb-core.f, bootstrap-codegen-test.f pinned counts) intact - update pinned counts honestly. Acceptance: bootstrap-wide-memory.fs green with a prefix defer present in a stage0-loaded file; bootstrap-codegen-test ok; fixpoint x2 byte-identical; boot-pin ok; full run.f GREEN. Files: bootstrap/cg/forth.fs, test/bootstrap-codegen-test.f (pins), possibly tools/boot-pin.f. Ownership: bootstrap/stage0 codegen. Blocks: habu-migrate-engine-hooks-e4f31fc6.

Claim: agent=deferis workspace=.jj-ws/fable-deferis

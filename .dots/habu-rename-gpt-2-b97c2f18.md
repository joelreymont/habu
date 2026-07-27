---
title: Rename GPT-2 checkpoint loader
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T19:24:10.296426+02:00"
blocks:
  - habu-rename-gpt-2-60fee511
---

Why: gpt2-bind.f loads and validates a checkpoint, but its file, package GPT2TX, tests, comments, and rejection codes use invented transaction or binding names. The operation is checkpoint loading.

Owner and exact interface: rename maki/infer/gpt2-bind.f to maki/infer/gpt2-load.f, gpt2-bind-test.f to gpt2-load-test.f, and gpt2-bind-fixture.f to gpt2-checkpoint-fixture.f. Rename package GPT2TX to GPT2LOAD everywhere, including its generated linear and ENUM names. Rename E-GX-COUNT to E-COUNT, E-GX-KEY to E-NAME, E-GX-DTYPE to E-DTYPE, E-GX-RANK to E-RANK, E-GX-SHAPE to E-SHAPE, E-GX-OFFSET to E-OFFSET, E-GX-SLOT to E-SLOT, E-GX-EXTENT to E-SIZE, E-GX-RENDER to E-NAME-SIZE, E-GX-ALIAS to E-ALIAS, E-GX-FOREIGN to E-CONFIG, E-GX-IMAGE to E-NO-IMAGE, E-GX-COPY to E-COPY, and E-GX-FAMILY to E-FAMILY. Rename the private test helper BIND-TAKE-MOVED to TAKE-MOVED. Replace bind, binding, binder, and bound with load, loading, loader, and loaded only where the text describes this checkpoint path.

Behavior and representation do not change: PREPARE, CHECK, CHECK-ALLOC, COMMIT-MAPPED, COMMIT-ALLOCATED, RELINQUISH, cleanup, family validation, plan rows, ownership, and every result arm retain their exact contracts. This leaf does not add the future top-level LOAD word or move model ownership; those require the separately frozen model contract.

Dependency: habu-rename-gpt-2-60fee511 must land first, so this loader consumes GPT2TENSOR and never introduces a compatibility bridge.

Owned write set: the three file renames; gpt2-alloc-test.f, gpt2-check-test.f, gpt2-payload-test.f, weight-store-test.f, maki/test.f, maki/test-core.f, test/enum-decl-suite.f, tools/refine-lint-core.f, FILEMAP.md, STATUS.md, TRUSTED.md, and current task or lesson text that names this exact loader API. The orchestrator reconciles current task descriptions without changing another worker claim.

Forbidden: aliases, forwarding words, duplicate files, old require paths, compatibility packages, behavioral changes, model redesign, a new public pointer, a top-level LOAD implementation, broad replacement of unrelated compiler or evidence uses of bind, or leaving old error names reachable.

Production defect and acceptance: before the change, every real GPT-2 preparation, mapped-load, allocated-load, cleanup, and family-rejection suite requires gpt2-bind.f and resolves GPT2TX; gpt2-load.f and GPT2LOAD are absent. After the change, the renamed focused suites plus gpt2-check-test.f, gpt2-alloc-test.f, gpt2-payload-test.f, weight-store-test.f, and full Maki pass through gpt2-load.f; exact symbol probes accept GPT2LOAD and each clear rejection name while rejecting GPT2TX, every E-GX name, and every old file path; FILEMAP and source-list checks find no old checkpoint-loader path. Exact package, typed-local, error-code, host, and file-map gates pass. This is a pure hard cutover and one compiling commit.

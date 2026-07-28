---
title: Make SHA-256 context reentrant
status: open
priority: 3
issue-type: task
created-at: "2026-07-28T12:29:28.418038+02:00"
---

Full context: found while building src/compiler/digest.f. The engine's SHA-256 context is a single process-wide state and each caller keeps one preimage buffer, so a digest computation cannot be nested or run concurrently. This is pre-existing and shared by every SHA-256 caller in the tree - lib/content-key.f, maki/db/transaction.f, maki/target/target.f, and now src/compiler/digest.f - and is documented at the top of digest.f rather than worked around. It is not a defect today because compilation is single-threaded. Required result: a reentrant hash context, so a caller holds its own state and nesting or concurrency is safe. Acceptance: two interleaved digest computations produce the same results as if run in sequence, proven by a focused test; existing callers migrate to the reentrant form; no caller relies on process-wide hash state. Blocks any multi-threaded compiler stage that digests.

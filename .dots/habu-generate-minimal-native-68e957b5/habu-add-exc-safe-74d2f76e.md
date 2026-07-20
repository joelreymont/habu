---
title: Add exception-safe CUDA scope
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T20:52:20.193802+02:00\""
---

lib/ptx/cuda-driver.f exposes nominal integer handles and RC0, but no ownership/scope abstraction. Callers therefore acquire a primary context, modules, events, and device allocations into ambient variables and call cleanup only at the end of the happy path. A throw between acquisitions skips cleanup; a cleanup throw skips later releases; stale nonzero variables can then be double-freed by a later cleanup attempt. Add a package-owned exception-safe CUDA resource scope that records each successful acquisition exactly once, transfers ownership explicitly, and unwinds all owned resources in reverse order on both success and throw. Preserve the primary operation error while also retaining/reporting cleanup errors; when only cleanup fails, propagate it. Zero/consume handles after attempted release and make repeated cleanup a proved no-op, not a second driver call. Use typed resource states now and migrate to linear owner types when the declared-private-linear-type capability lands; do not expose raw acquisition without a named owner boundary. Add an injected driver table so host-only tests cover failure after every acquisition, partial allocation, every cleanup failure, simultaneous primary+cleanup failures, reverse order, exactly-once calls, ownership transfer, and repeated cleanup. Files: lib/ptx/cuda-driver.f or a new package-owned cuda-scope.f, focused tests, TRUSTED/FILEMAP if needed. Depends: none. Ownership: reusable CUDA ownership/unwind mechanism only; caller migrations are separate dots.

Claim: agent=cudascope workspace=.jj-ws/fable-cudascope machine=spark (owns the package-owned exception-safe CUDA resource scope: lib/ptx/cuda-driver.f or new cuda-scope.f + focused tests + TRUSTED/FILEMAP; caller migrations stay in their own dots)

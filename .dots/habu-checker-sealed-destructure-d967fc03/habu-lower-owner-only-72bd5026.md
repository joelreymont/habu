---
title: Lower owner-only structure destructure
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:50:21.906064+02:00"
blocks:
  - habu-check-owner-only-7b724d00
---

Problem: a checker-only destructure form would certify source that the native compiler, interpreter, and verification tools cannot execute consistently. Required result: implement the exact two-token destructure family protocol in native compilation, interpretation, source reconstruction, verify-source, diagnostics, bootstrap, and AOT paths. After consuming both tokens and proving the same owner-package/product policy as the checker, emit no runtime operation because a product value already is its ordered field cells. Do not define a runtime word, expose an xt, or accept a qualified foreign family. Truncated forms fail closed. Owner: compiler/lowering and tool parity only; checker semantics are frozen by the dependency. Dependency: habu-check-owner-only-7b724d00. Acceptance: one source compiles, interprets, snapshots, restores, and verifies identically; mutation tests for missing operands, wrong owner, wrong kind, and an inserted runtime operation fail; native fixpoint and exact source gates pass.

---
title: Make package-first rule blocking
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-16T00:53:28.573006+02:00\""
---

Claim: agent=root workspace=.jj-ws/habu-package-rule

Full context: AGENTS.md Forth style summarizes casing and typing but omits docs/forth.md:109-123 package-first rule, so agents repeatedly generate raw namespace stems such as LRD-* despite the canonical standard. Fix: add a concise BLOCKING AGENTS rule requiring every new library, tool, test-support, and subsystem module to use a real package by default; permit globals only for explicitly documented core/prelude language surfaces; require short package-local tails and cross-package qualification; forbid raw prefix stems as namespace substitutes; make a package-less changed module definition a commit failure while the checked package-diff gate tracked by habu-enforce-pkg-first-c28d1dec is built. Acceptance: AGENTS points to the canonical package section, the rule is unambiguous at first-session scan, LESSONS records why indirect documentation was insufficient, and host/filemap/dot/maki/ptx gates pass.

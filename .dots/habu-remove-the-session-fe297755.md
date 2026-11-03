---
title: Remove the session stand-down hook
status: open
priority: 3
issue-type: task
created-at: "2026-11-03T10:15:31.893555+02:00"
blocks:
  - habu-remove-the-compiler-d44fac90
---

Problem: IR-CTX:SESSION-STAND-DOWN! installs a one-xt hook that IR-CTX calls when a compile session closes, so owners clear their per-session state through a cell instead of by data (hooks are justified only by what must be swapped, never by load order). The lease removal (habu-remove-the-compiler-d44fac90, /tmp/jerry-big/nolease-design.md rev 4) keeps the hook. Two ways to remove it: each owner records the session serial it filled its state under and treats a different serial as empty, as src/compiler/native/compiler.f:575 already reads liveness; or the compiler owns the close and clears each owner by a direct call. Acceptance: SESSION-STAND-DOWN! and its cell go; no behaviour change on session close or refusal. Files: to be named by the design after the lease lane lands. Verify: the lease lane's proof rows. Depends: habu-remove-the-compiler-d44fac90. Ownership: IR-CTX's session close and its owners. Claim: unassigned.

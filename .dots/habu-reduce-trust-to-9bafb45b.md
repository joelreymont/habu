---
title: Reduce TRUST to audited leaves
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:00:25.004451+02:00"
---

Problem: TRUST/TRUSTED: remains necessary but broad: src/core/check-hook.f exports checker internals as trusted signatures; PTX words, FFI casts, image/snapshot/build emitters, and test recursive-check helpers still use TRUST/TRUSTED:. Fix: classify every TRUSTED.md row into expressible-now, missing checker capability, or true native boundary; remove expressible rows by checked factors; replace broad hook exports with smaller typed public words where possible; create child dots for missing capabilities instead of widening trust. Acceptance: trust-lint remains green; TRUSTED.md gains class/owner for each remaining row; count of avoidable TRUST/TRUSTED sites drops; every remaining true boundary has focused tests and no unchecked body larger than the native seam.

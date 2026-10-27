---
title: Keep the x86-64 policy keyword list in step with ARM rows
status: open
priority: 3
issue-type: task
created-at: "2026-10-27T20:56:36.716001+02:00"
---

src/habu/definers.f DEF-OP-KEYWORDS is a hand-kept copy of the ARM dispatch rows (habu2.f EM-COMPILE-OPS, replayed by LROWWALK) that the x86-64 policy writer POLICY-KEYWORD and the sealed DEF-WALL use. It lacks the ARM rows c@ (LKWCFETCH), cells (LKWCELLS), cell+ (LKWCELLP) and +field (LKWPFIELD), so under the seal x86-64 admits a package publishing those names and accepts a sealed definition of them, where ARM refuses both (test/policy/allow-keyword-package.f states why: tier 0 reads the name as the row). Found reviewing the constant-divisor lane, which added / and mod to both. Fix at the root: one source for the row-name set that both targets read, or failing that the four names plus a test/policy case run on x86-64. Acceptance: a package publishing cells is refused on both targets. Needs an x86-64 host to verify.

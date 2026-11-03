---
title: "Keep a does> patch's effect when a colon definition follows the created word"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T15:40:50.554956+02:00"
---

Problem: on the installed hb, `1 set-tier variable V : B ( -- ) does> ( -- n ) drop 9 ; B V .` is refused `at '.' expected: n actual: ptr a`, while `1 set-tier : B2 ( -- ) does> ( -- n ) drop 9 ; variable W B2 W .` prints 9, rc 0. The engine patches the last created word (V), but the checker keeps V's old effect when a colon definition (B) was published after V, so checked code is typed against behaviour that no longer runs: `B V @` would be admitted and fetch from address 9 (inferred, not run). The checker states that a `does>` replaces an effect that already exists (habu2.f LASTC-TRUST comment). Suspect, from reading only: the patch's publish names its word through C-PUSH-DREC-NAME (src/habu/habu2.f:2646), which reads the name from the definition body buffer, which after `: B ... ;` holds B's text, not V's record. Found by the switch lane B6 review (test/outer-interpret.f CONSTANT-REFUSED-DOES, :2028-2045). Acceptance: reduce the failure, find whether the engine's does> publish, the checker or the definer rule is wrong, and fix that layer, so that after the patch the checker types V by the clause's effect or the engine refuses the patch, on arm64 and x86-64, at tier 0 and tier 1; a test on the real load path holds both orders above. Files: per the root cause (src/habu/habu2.f, src/habu/kernel-x64.f, src/habu/definers.f, src/core/checker.f). Verify: the reproducer above on a rebuilt hb at tiers 0 and 1, the outer-interpret and certify-does-definer tests, the full suite; the x86 row on pulsar. Depends: none. Ownership: unassigned. Claim: unassigned.

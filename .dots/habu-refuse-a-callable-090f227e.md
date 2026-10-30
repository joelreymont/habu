---
title: Refuse a callable whose output row is implicit
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T17:24:59.406195+02:00"
---

Problem (tim, from Maki): since 4845688495d4 'Keep a callable's implicit row from escaping', ': KEPT ( R [ R n -- n ] -- n ) 7 swap execute ;' defines, but every call refuses with 'callable row escape … implicit input row' (src/core/checker.f:3476 HOLD-ESCAPE?, :3526 QUOT-PAIR). Declaring the callable's output row by name, ( R [ R n -- S n ] -- S n ) as CMD:WITH-COMMAND does, defines and calls. A definition that no call can use is a refusal reported at the wrong place. Probes: /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/stepsrc-probe/p1.f (refused calls), p2.f (named row, accepted), p3.f; engines /tmp/stepsrc-probe/habu-{pre,rule}. Acceptance: the checker refuses at the definition a callable parameter whose input row is named and whose output row is implicit, with a named error whose text says to name the output row; p1's KEPT is refused there, p2 still defines and runs; test/checker-soundness-suite.f CS-W10..W12 (same shape, :336-339) become refusal cases unless a call of theirs is accepted today (reduce that first; if one is, the rule distinguishes it and the dot records why); docs/forth.md 'Rules learned by refusal' and docs/forth-card.md gain the rule. Files: src/core/checker.f, test/checker-soundness-suite.f, docs/forth.md, docs/forth-card.md. Verify: the soundness suite and the three probes on the lane engine; full suite (shared checker behaviour). Depends: none. Claim: unassigned.

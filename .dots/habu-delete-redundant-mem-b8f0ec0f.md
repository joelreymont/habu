---
title: Delete redundant MEM-BYTE-PTR-REJECT guard
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:50:42.434556+02:00"
---

Why: the special-cased rejection of cell @ over ptr u8 in checker.f predates the landed pointer-element unification (e4cd7436), which now catches both directions structurally in CON-OK?. The guard is redundant machinery. Behavior: delete the guard; prove the unifier still rejects cell @ over ptr u8 with the same or better diagnostic. Owner: src/core/checker.f. Dependencies: none (fix landed). Acceptance: guard deleted; test/ptr-elem-test.f still green including the byte-fetch-through-cell-pointer and cell-fetch-through-byte-pointer cases; fixpoint x2. First consumer: checker.f itself — dead code removal on the certify path. Claim: unassigned.

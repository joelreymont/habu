---
title: "B1: unknown-signature-token error"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:56.106622+02:00"
blocks:
  - habu-a-checked-prelude-e7132a9a
---

Checker: when a ( ... ) stack-signature contains a token that is not a known/declared type, error AT THE SIGNATURE ('unknown type X in signature') instead of silently miscounting cells and exploding downstream ('at then'). src/core/checker.f signature parse. Biggest round-trip killer. Preserve fixpoint.

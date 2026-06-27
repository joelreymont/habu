---
title: "B1: unknown-signature-token error"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-27T13:15:56.106622+02:00\""
closed-at: "2026-06-27T17:11:37.352166+02:00"
close-reason: implemented first-error unknown signature type diagnostics; direct and warm tools now report got at signature; focused slices and full native gate green
blocks:
  - habu-a-checked-prelude-e7132a9a
---

Checker: when a ( ... ) stack-signature contains a token that is not a known/declared type, error AT THE SIGNATURE ('unknown type X in signature') instead of silently miscounting cells and exploding downstream ('at then'). src/core/checker.f signature parse. Biggest round-trip killer. Preserve fixpoint.

---
title: "Core: dictionary-vs-literal precedence is context-inconsistent"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T03:52:11.888567+02:00"
---

FOR THE CORE LANE. Three probes this campaign gave apparently conflicting answers on whether the number parser or dictionary lookup wins: (a) eval-lane checked-candidate probe: checker certified '( n -- ) .0' as the fmt word when loaded; (b) wave-2 reviewer: '.5' in a colon body is E-UNDEFINED (not a literal) and claimed .0 resolves as a word at runtime; (c) fmt-lane + orchestrator probes: colon body '60 .0' with fmt loaded does NOT run the printer (gate test GD-LITERAL-FIRST pins literal-first), and the missing manifest row for .0 structurally corroborates the word being unreachable - yet '.5' errors E-UNDEFINED in the same position. Static invariant candidate: token resolution order must be identical across interpret, colon-compile, and checker-candidate contexts, and total (either a token is always a literal or always a lookup). Reduce with minimal fixtures per context; reconcile GD-LITERAL-FIRST with the .5 E-UNDEFINED behavior (is the float-literal grammar digit-leading only in some paths?); then align the checker model. The E-NUMERIC-DEFINITION lint already prevents NEW shadowable names, so this is soundness bookkeeping, not an active hazard.

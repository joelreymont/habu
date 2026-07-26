---
title: Check owner-only structure destructure
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:50:10.078971+02:00"
blocks:
  - habu-seal-pkgs-owning-139e660f
---

Problem: owner-only structures need an in-package inverse without publishing a callable destructor. Required checker form: destructure family consumes one owner-policy product value and produces its declaration-order field row. family is an operand token resolved only while the original, not-yet-closed owning package block is active; qualified foreign spelling, later package reopen, using imports, globals, PUBLIC-policy products, ENUM families, unknown families, and missing operands reject with named diagnostics on the real token. The checker derives fields and generic substitution only from committed TFAM/TYPE-FIELD metadata and preserves whole-bundle linear accounting. This is a reserved form, not a dictionary word, xt, public raw cast, caller-name heuristic, or test friend. Owner: checker parsing/effect and negative fixtures only. Dependency: habu-seal-pkgs-owning-139e660f. Acceptance: in-owner concrete and generic cases certify; every bypass above rejects; a proof field can be recovered only inside its original declaration block; CHECK!, checker replay, type-linear, and structure suites pass.

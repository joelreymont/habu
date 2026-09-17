---
title: Check owner product construction
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:53:42.657836+02:00"
closed-at: "2026-09-16T14:34:51.195427+03:00"
close-reason: "superseded by habu-campaign-c3-the-a2477c89: The construct form still handles only sum and enum variants, so owner products have no checked constructor; its GPT2 driver moved"
blocks:
  - habu-parse-owner-construction-d876c9ef
---

Problem: the existing compiler form construct handles only sum and enum variants, so an owner product has no checked constructor after public MAKE is suppressed. Result: extend the same reserved form to construct FAMILY for a product marked DRV-CONSTRUCT-OWNER. The parser resolves the family only in the currently active original declaring package, consumes its declared fields in order, and produces the nominal product with exact linear roles. Public products, unmarked/private families, qualified names, wrong or reopened packages, missing/extra operands, wrong fields, and ordinary call syntax reject before lowering. Existing construct FAMILY VARIANT behavior is unchanged. Add no word, proof token, cast, raw identifier, caller-name heuristic, runtime branch, or compatibility alias. Owner: existing checker construct token protocol and type-family effect synthesis only. Production red: removing GPT2-CONFIG:MAKE leaves no owner construction path. Acceptance: real checked owner products compile only in the original package; hostile foreign/reopen/evaluate/JIT/AOT fixtures reject with named diagnostics; linear fields are consumed exactly; checker, package, and exact diff gates pass. Claim: unassigned.

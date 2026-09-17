---
title: Certify generated payloads from their retained prefix
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T02:31:20.735797+03:00"
---

Claim: unassigned (cedar's lane ended 2026-09-15; the reduction below stands). The generated-payload verifier input-scope defect: Complete stage/stdin source executes PREFIX-REWIND:TO-CORE before declaring option and CAD-NUM, but BF-CERTIFY-ACT verifies against the warmed tool host and rejects option as duplicate7102. Preserve real in-program duplicate rejection and all parent host state after accept/throw; use existing prefix and isolation contracts. Reproductions: /tmp/cedar-maker-family-replay.f (loaded7102, fresh0 twice, genuine duplicate7102, retained references0); /tmp/cedar-maker-family-prefix.f (tier1 existing rewind plus neutral replay accepts0). Existing RBF/TF/SCHEMA frames are cursor-only and cannot restore overwritten rows after rewind; investigate existing process/owner APIs before implementation. Separate from incomplete source assembly9c1524cc and replay import depthcc037817.

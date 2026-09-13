---
title: Certify generated payloads from their retained prefix
status: open
priority: 2
issue-type: task
created-at: "\"2026-09-14T02:31:20.735797+03:00\""
---

Cedar owns the generated-payload verifier input-scope defect. Complete stage/stdin source executes PREFIX-REWIND:TO-CORE before declaring option and CAD-NUM, but BF-CERTIFY-ACT verifies against the warmed tool host and rejects option as duplicate7102. Preserve real in-program duplicate rejection and all parent host state after accept/throw; use existing prefix and isolation contracts. Reproductions: /tmp/cedar-maker-family-replay.f (loaded7102, fresh0 twice, genuine duplicate7102, retained references0); /tmp/cedar-maker-family-prefix.f (tier1 existing rewind plus neutral replay accepts0). Existing RBF/TF/SCHEMA frames are cursor-only and cannot restore overwritten rows after rewind; investigate existing process/owner APIs before implementation. Separate from incomplete source assembly9c1524cc and replay import depthcc037817.

The current native-build and native-bootstrap paths do not use these certificates. Cedar retains the reduction, but no process certifier was added: the cold-prefix COW proof passes the duplicate-family boundary then refuses the missing numeric-result constructor effect at NR-OK. Resolve generated-certifier ownership with public BF/recovery migration a2551190; do not revive retired fixture makers to satisfy this scan. WID/chain acceptance instead uses the current native writer with partial/empty captures under leaf2f64be7c.

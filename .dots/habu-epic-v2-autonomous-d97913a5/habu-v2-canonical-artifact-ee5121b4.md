---
title: V2 canonical artifact envelope
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:26.829932+02:00"
---

Implement MODEL-CAD-V2-PLAN.md:1812-1830 first artifact slice. Define checked Artifact<Kind> metadata schemas for schema id/version, kind, payload digest, producer, source revisions, dependencies, target/config/numeric facts, capabilities, and event id using required V2 nominal/layout types; add canonical encode/decode and malformed/noncanonical/kind-mismatch negatives. No raw n identity or trust boundary. Acceptance: equal values encode/hash identically, one semantic field changes the digest, decode round-trips, unknown required fields and digest mismatch return typed diagnostics. Files: new maki/db/artifact.f plus focused test and FILEMAP.

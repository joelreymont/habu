---
title: Lower niche-null and add the maybe-null pointer family
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:27:45.355753+02:00"
---

Lane 1 of /Users/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md: sumtype.f accepts POLICY niche-null for two variants, the first empty and the second one ptr field; T0's MATCH makes the tag from the cell; a built-in one-cell family for a pointer that may be null; a global identity word ( ptr a -- n ) for distances. Nothing is refused yet. Acceptance: one load-path test (construct, MATCH, a zero-filled typed cell answering the empty variant, a signature, a C result declared with the family); the chain check; installed before any source uses it. Depends: Joel's names for the family, its variants and the distance word. Claim: unassigned.

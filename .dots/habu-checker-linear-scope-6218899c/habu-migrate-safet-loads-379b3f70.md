---
title: Migrate SAFET loads to linear scope
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:36.436278+02:00"
blocks:
  - habu-implement-generic-linear-6bf72a41
---

Problem: `SAFET:LOAD` and `LOAD-SPAN` manually use stack-preserving catch,
`CLOSE`, and rethrow after `OPEN`; this duplicates the owner-scope protocol.
Result: use `LINEAR-SCOPE:WITH` for the session from `OPEN` through `MAP-FILE`
or `ADOPT` and `PARSE`. On success the body returns the session for `DETACH`;
on any catchable failure `CLOSE` consumes it exactly once and the original
error escapes. Remove only obsolete manual-catch helpers; preserve census,
mapping, parsing, and pointer-lifetime behavior. Owner:
`maki/infer/safetensors.f` and its focused tests. Dependency:
`habu-implement-generic-linear-6bf72a41`.

Acceptance: real missing-file, malformed-header, malformed-payload,
adopted-span failure, and success paths run through `LOAD` or `LOAD-SPAN`;
live session and mapping counters return to baseline after every case; the
real artifact leg remains unchanged when available. Run the safetensors
focused suites plus package, typed-local, and signature gates.

---
title: Migrate SAFET loads to linear scope
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:36.436278+02:00"
blocks:
  - habu-implement-generic-linear-6bf72a41
---

Problem: SAFET:LOAD and LOAD-SPAN manually use stack-preserving catch, CLOSE, and rethrow after OPEN; this duplicates the owner-scope protocol and is the production example the capability exists to replace. Required result: use LINEAR-SCOPE:WITH for the session from OPEN through MAP-FILE or ADOPT and PARSE. On success the body returns the session for DETACH; on any catchable failure CLOSE consumes it exactly once and the original error escapes. Remove only the obsolete manual-catch caveat and helpers that become unused; do not change census, mapping, parsing, or pointer-lifetime contracts. Owner: maki/infer/safetensors.f and its existing tests/docs. Dependency: habu-implement-generic-linear-6bf72a41. Acceptance: real missing-file, malformed-header, malformed-payload, adopted-span failure, and success paths run through LOAD/LOAD-SPAN; live session/mapping counters return to baseline after every case; the real artifact leg remains unchanged when available; safetensors, package, typed-local, signature, and trust gates pass.

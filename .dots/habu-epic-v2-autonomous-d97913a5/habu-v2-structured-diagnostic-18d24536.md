---
title: V2 structured diagnostic IR
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T12:25:27.328128+02:00\""
---

Edge note 2026-07-17: blocker habu-v2-canonical-artifact-ee5121b4 removed
- the envelope core + id-family codecs this ADT references are landed
(469f1e15 and earlier); the envelope dot remains open only for txn-owned
and user-gated fields irrelevant to the diagnostic IR.

Implement MODEL-CAD-V2-PLAN.md:1871-1894 common Diagnostic ADT plus typed class variants for invariant, unsupported, invalid input, resource, external, numeric, performance, stale evidence, and authorization failures. Include owner, subject, revision, phase/location, expected/observed facts, dependency cone, counterexample, repairs, invalidated evidence, reproduction, environment, parent, and progress. Acceptance: human and JSON renderers consume one value, canonical round-trip passes, missing owner/reproduction rejects, and representative checker/pass/runtime/deploy failures lower losslessly.

Claim: agent=diag workspace=.jj-ws/fable-diag (owns the new diagnostic IR files)

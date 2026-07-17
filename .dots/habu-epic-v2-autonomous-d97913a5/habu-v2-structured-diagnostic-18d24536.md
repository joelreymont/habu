---
title: V2 structured diagnostic IR
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:27.328128+02:00\\\"\""
closed-at: "2026-07-17T18:24:21.556592+02:00"
close-reason: "Diagnostic IR + dual renderers landed (6b19cda8): nine typed classes, canonical round-trip, typed missing-owner/reproduction rejects, lossless lowering proven on four real failure classes. Field-promotion follow-up minted."
---

Edge note 2026-07-17: blocker habu-v2-canonical-artifact-ee5121b4 removed
- the envelope core + id-family codecs this ADT references are landed
(469f1e15 and earlier); the envelope dot remains open only for txn-owned
and user-gated fields irrelevant to the diagnostic IR.

Implement MODEL-CAD-V2-PLAN.md:1871-1894 common Diagnostic ADT plus typed class variants for invariant, unsupported, invalid input, resource, external, numeric, performance, stale evidence, and authorization failures. Include owner, subject, revision, phase/location, expected/observed facts, dependency cone, counterexample, repairs, invalidated evidence, reproduction, environment, parent, and progress. Acceptance: human and JSON renderers consume one value, canonical round-trip passes, missing owner/reproduction rejects, and representative checker/pass/runtime/deploy failures lower losslessly.

Claim: agent=diag workspace=.jj-ws/fable-diag (owns the new diagnostic IR files)

RESOLVED 2026-07-17 (diag lane, commit 6b19cda8): ACCEPTANCE MET. Package
DIAG (maki/db/diagnostic.f + diagnostic-render.f + test): pooled-handle
diagnostic PRODUCT, nine-class ENUM substrate (+ severity/phase/repair
enums, DERIVE eq, exhaustive MATCH), all common fields, staged builder
with TYPED rejects for missing owner/reproduction, canonical
ascending-tag codec with foreign-id fields serialized across owner
boundaries. Human + JSON renderers (checked lib/json-write.f - no host
tooling) consume ONE value. Lossless lowering proven field-for-field on
four real failure classes (checker reject, ptxas failure, device-launch
fault, gate timeout). Conservative readings documented at definition
sites: subject-digest -> artifact-id, environment -> config-id,
revision + invalidated-evidence as STRINGS, parent as code, facts as
strings. First-slice bounded ring documented honestly. Post-rebase
correction by the orchestrator: rev-id DOES have an owner registry now
(maki/rev.f landed mid-lane) - promoting `revision` is the follow-up dot
habu-diag-nominal-ids, not a missing capability; evidence-id genuinely
lacks a registry (noted on the evidence dots).

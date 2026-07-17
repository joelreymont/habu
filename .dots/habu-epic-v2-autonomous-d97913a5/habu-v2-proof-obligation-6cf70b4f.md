---
title: V2 proof obligation schema
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T12:25:27.203645+02:00\""
---

Edge note 2026-07-17: blocker habu-v2-canonical-artifact-ee5121b4 SATISFIED
and removed - the envelope program closed (three slices + wire migration);
the subject-digest and identity surfaces this schema consumes are landed.

Implement MODEL-CAD-V2-PLAN.md:1851-1869 typed obligations naming subject digest, claimed relation, proof domain, policy, verifier class, and required environment. Exact, approximate, empirical, device, safety, and performance domains are distinct families. Acceptance: wrong-domain evidence cannot discharge, subject or environment mismatch rejects, producer cannot be sole verifier when policy requires independence, and changed dependency invalidates only affected obligations.

Claim: agent=oblig workspace=.jj-ws/fable-oblig (owns new maki obligation schema files + focused tests)

---
title: Wire numeric-policy ENFORCE into the promotion gate-set
status: open
priority: 2
issue-type: task
created-at: "2026-07-14T23:52:52.332571+02:00"
---

Follow-on from habu-v2-numeric-policy-6f639843 (landed 710eed8f): NPOL:ENFORCE (E-NPOL-APPROX) exists as a checked gate with executed fixtures, and EVID:golden carries its achieved domain (GOLD-DOM), but cad.f PROMOTE does not yet auto-enforce the achieved domain against a required policy - wiring it naively would make every accumulating/transcendental region refuse the default exact policy and break existing promotions. Fix: add a numeric-policy requirement to the promote-policy gate-set (maki/evidence/policy.f POLICY:CHECK or the DEFAULT-POLICY req rows) keyed on the region's REQUESTED policy (the skey pol field), enforcing GOLD-DOM SATISFIES? requested at promotion with the named refusal; default policy for accumulating/transcendental region classes must be set honestly (relative) rather than exact-and-always-refusing. Acceptance: a TF32/relative golden promotes under a relative-requested region and refuses under exact; existing demo-ffn/cad promotions stay green with honest defaults; executed non-vacuous fixtures. Files: maki/evidence/policy.f, maki/cad.f promote seam, maki/numpolicy.f defaults, tests. Verify: policy-e2e + cad + demo-ffn suites, maki/test.f. Ownership: maki evidence/policy.

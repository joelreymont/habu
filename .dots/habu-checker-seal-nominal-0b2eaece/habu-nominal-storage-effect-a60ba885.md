---
title: "Nominal storage: effect parametricity"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-12T16:08:35.684126+02:00\\\"\""
closed-at: "2026-07-15T01:38:43.554512+02:00"
close-reason: "Merged ef2fff8d on master: NP-CHECK post-body validation in CHECK (after body+return unification, gated on otherwise-certified) row-walks the DECLARED rows (not NMAP - typed-local pollution) and rejects with E-NONPARAMETRIC-EFFECT / repair class fix_parametric_effect: (1) a declared quantifier specialized to a sealed identity-bearing family (plain-scalar widening stays legal - 236+ corpus sites depend on it), (2) quantifier aliasing (injectivity via seen-roots). Diagnostics through DCODE/REPAIR-CLASS/SUGGEST/prose/JSON with quantifier+family named; ptr-family-to-ptr-a stays on the stable NOMPTR path. No new bindings in NP-CHECK so trail discipline unchanged; rejected sigs never persist (CERT-ADD gated on verdict); engine-suite pins post-reject state cleanliness; fixtures use zero added trust surface (first draft's 3 TRUST calls replaced by checked scaffolding after the inventory ratchet caught them). Red-first proven; zero false positives across full run.f RUN_EXIT=0 + maki + slices on the exact merged tree with refreshed binary. Scope: no descent into quotation/parametric-family subterms (higher-order false positives / deferred parametric-cell governance - documented in docs/effects.md). Known pre-existing flake noted: perf-regress fork-worker race under machine saturation (LESSONS 1256), not this lane's code. Unblocks typed-definers c5f44d66 + immutable-nominal 9290a81f."
---

Problem: a declared polymorphic effect can specialize a variable to a concrete family or erase ptr family to ptr a, then the checker records the original generic signature. Acceptance: after body checking, every declared quantifier still resolves to a distinct variable; specialization or quantifier aliasing rejects; valid generic LOAD and ID wrappers certify; pointer-pointee diagnostic path and E-NONPARAMETRIC-EFFECT repair class are stable; multi-error and rollback never persist rejected signatures. Files: src/core/checker.f, diagnostic renderer/schema, engine and all-errors fixtures, docs/effects.md. Verify: direct family-to-a, ptr-family-to-ptr-a and injectivity negatives; generic positives; checker/rollback/fixpoint/bootstrap/full gates. Depends: habu-nominal-storage-raw-a3430ef2.

Claim: agent=effseal workspace=.jj-ws/fable-effseal

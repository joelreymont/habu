---
title: Correct the type-system guide for wide and parametric locals
status: active
priority: 2
issue-type: task
created-at: "2026-09-20T03:01:46.499522+03:00"
---

docs/type-system.md still refuses wide locals in section 5, groups them with linear local refusals in section 9, and says span<a> cannot bind or duplicate in section 11. Reconcile these claims with the integrated checker and existing wide-local/span fixtures; retain the still-enforced linear-owner refusal. Documentation only. Claim: alder, .jj-ws/alder-type-local-doc on 1afd910c. Acceptance: current language examples and owning focused rows pass; no checker/compiler change.

The same section also reversed the mint's widening, which changed when open
span layouts became placeable. Corrected to the current STW-MINT / STW-MINT2
assertions; SPAN:BYTES is the explicit reverse view. The remaining linear-local
refusal is pinned by CBAD-OWN-LOCAL-ONCE.

Validation: whole wide-typed-local-probe, wide-typed-local-probe-aot, span and
engine registry rows pass on private host 31be3fb0 against 1afd910c. Logs are in
/tmp/alder-type-local-doc. No registry or tools/*-test.f reader names this doc.
Astra review clear. No runtime, checker or compiler change.

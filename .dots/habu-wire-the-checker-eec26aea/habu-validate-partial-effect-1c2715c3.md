---
title: Validate partial effect row widths against type semantics
status: closed
priority: 2
issue-type: task
created-at: "\\\"2026-09-13T22:36:30.824089+03:00\\\""
closed-at: "2026-09-16T14:34:48.394167+03:00"
close-reason: "done: Producer and importer now validate encoded effect-row widths against the type, with the scalar-zero and hidden-wide negatives in the real suite. [test/aot-payload-graph-child.f:138-174 CORRUPT drives scalar-zero and hidden-wide rows through EN.C and ER.MINI.]"
---

Independent Astra review of 81a865e9 found EN-PUSH admits any positive EN.C encoded width without checking its referenced type. Safe real 17-section roundtrip reproducer: in test/aot-payload-graph-child.f CORRUPT set 1 1 GRAPH DIN EN.C ! and 0 1 GRAPH ER.MINI !; import succeeds and PAYLOAD-FIXED has one input term but zero input cells. Native callable metadata consumes these widths; no execution miscompile claimed. Derive/check widths using the actual type and captured registry layout semantics before producer/export and importer publication. Preserve polymorphic, nominal and hidden physical widths. Add scalar-zero contradiction and known wide-layout mismatch refusals, legitimate-width controls and unchanged checker/registry publication state checks. Keep exception, dynamic SCH-CON and native return-row contracts in their separate leaves. Claim: payload_resume; must pass independent Astra review before landing.

Implemented in the child of 81a865e9. Producer and importer share graph validation, with logical widths memoized after child validation and the registry's schema-parameter substitution rule applied through the uninstalled future view. Each EN-PUSH must equal its type's physical width plus one; hidden fields remain one cell and must name an existing physical slot. Cell-width arguments reuse the already validated declared width. Scratch is reused during export and released at capture preparation or refusal.

The real file suite adds scalar-zero, hidden-wide and logical-wide corruptions, each with a matching forged minimum-input field; all refuse before publication. The private validator's caught refusal compares published USIGS, symbol and constructor contents/counts, registry contents/counts, effect heads and family/variant indexes. Public installation then refuses rc 76 with the named width diagnostic. A corrupt source effect is refused by the producer too. Positive controls preserve two physical product fields, a three-cell instantiated layout argument, a logical two-cell polymorphic family and a zero-width empty-product argument yielding a one-cell tagged value.

Focused acceptance: `bin/hb --load test/aot-payload-graph.f`, `test/aot-registry-identity.f`, `test/aot-payload-unsupported.f`, and `test/aot-payload-admission.f`. Supplemental generated probes in build/payload compile all 50 registry bodies through tier 1; graph-width-native.f binds the actual optimizing width implementation during source export and real file restoration. These retain typed JIT adapters for prehook dependencies; they are not a wholly native checker or fresh-process native artifact acceptance. Await independent review and integration; the existing exception, dynamic-constructor and return-row leaves remain open.

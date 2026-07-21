---
title: SC-QUOT full effect rows for quotation payloads
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T07:23:54.380873+02:00\""
---

Follow-up from habu-universal-enum-parametric-ad011c21: quotation variant payloads currently carry ONE type per effect side because the landed SC-QUOT schema node stores single din/dout/rin/rout type nodes; multi-input, multi-output, and empty sides reject at declaration. Lifting this needs SC-QUOT to store full effect ROWS per side: a type-schema.f node-encoding change (SC-QUOT layout), the PF-NODE-KIND? scanner in src/core/type-family.f, TFC-SCH-TERM instantiation (MK-QUOT already takes row bases, so the instantiation side generalizes naturally), TDGEN-SCH rendering of multi-type sides back into generated-constructor text, and the TDECL-VPAY-QUOT parser loop in src/core/sumtype.f. Update the existing type-family-suite SC-QUOT tests plus the decl/ctor suite fixtures. Ensure snapshot identity of the new node encoding is deterministic and covered by the rollback/persist canonical-zero invariant. Prove with an xt payload of effect ( n n -- n ) stored and executed through MATCH.

Claim: agent=scquot workspace=.jj-ws/habu-sc-quot-full-db4d0518 (Mac; owns the SC-QUOT node encoding in src/core/type-schema.f, its consumers PF-NODE-KIND?/TFC-SCH-TERM/TDGEN-SCH in src/core/type-family.f, the TDECL-VPAY-QUOT parser loop in src/core/sumtype.f, and the affected suites. Running lanes structparse/genmake/usingshadow are consumers-only of these files - write sets disjoint.)

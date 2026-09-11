---
title: "Type DSL: specify the hard cutover"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:18.630318+02:00"
closed-at: "2026-07-13T18:14:21.828055+02:00"
close-reason: Specified and destruction-reviewed unified STRUCTURE/ENUM hard cutover; compact ENUM omits arity and headers; exact census and explicit bootstrap-layout ownership landed at 6e16bb08.
---

Inventory every declaration and consumer of BEGIN-STRUCTURE, END-STRUCTURE, VALUE-RECORD, PRODUCT, SUMTYPE, ENUM, VARIANT, FIELD, generated constructor packages, reflection, snapshots, and AOT metadata. Pin the sole grammar: STRUCTURE name arity [POLICY p] [DERIVE ...] FIELD name type ... ;STRUCTURE and ENUM name arity [POLICY p] [DERIVE ...] VARIANT name [FIELD name type ...] ;VARIANT ... ;ENUM, with the compact ENUM name v... ;ENUM shorthand only for payloadless variants. Specify hard errors for all removed definers and mixed legacy syntax. Update MODEL-CAD-V2-PLAN.md and docs/type-families.md before implementation. Acceptance: every old surface has an explicit migration owner and no compatibility path.

Claim: agent=type_dsl_spec workspace=.jj-ws/type-dsl-spec

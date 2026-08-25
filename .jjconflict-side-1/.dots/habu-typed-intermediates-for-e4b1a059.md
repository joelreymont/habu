---
title: Typed intermediates for chained generated ctors
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T07:24:04.846938+02:00"
---

Follow-up from habu-universal-enum-parametric-ad011c21 (pre-existing item-11 generated-constructor-call limitation, re-confirmed by the nested opt<opt<n>> fixture): chained construction through generated constructors requires a typed intermediate word because the inner constructor's multi-cell result is only recovered from a declared output effect - a bare nested call site cannot express the intermediate's type. Either extend the generated-ctor call typing so a constructor's declared output feeds the enclosing constructor argument slot directly (checker-side: the ctor's certified effect is known), or document the one-line typed-helper pattern as the sanctioned form in docs/type-families.md with a checked example. Decide which is long-term correct rather than leaving both implicit; the E3 fixture in test/type-ctor-suite.f is the reproducer.

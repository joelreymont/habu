---
title: "checker: DEFTYPE/DEFLINEAR/VALUE-RECORD certify in-body (same class as DSL openers)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T23:51:21.289358+02:00"
---

Sibling gap from the in-body-opener fix (dot habu-checker-in-body-af7cf855, 2026-07-12): ': X ( -- ) DEFTYPE ;' certifies rc 0 (probed) - same hazard class as the now-rejected TYPEFAMILY/SUMTYPE/ENUM/PRODUCT openers: runtime token parsing + registry/dictionary mutation from a certified body (DEFTYPE even evaluates generated TRUSTED: source via DTC-EVAL, src/core/roles.f:44,119,172). DIFFERENT admission path: certified usigs, not PRIM: axioms - so UNSAFE-TOK? membership may not be the whole fix; investigate whether the usig should be removed/narrowed or the tokens added to the reject set (or both). Regressions per opener (in-body reject, top-level positive), enumerate in-tree in-body uses first (expected zero - the roles plumbing goes through internals). Owner: checker lane. Blocked by: the in-body opener commit landing (same suite file).

---
title: "checker: in-body type-DSL openers accepted at empty declared stack"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-12T15:58:50.463663+02:00\""
---

Found during the internal-word gate lane (dot habu-hb-crash-bare-c5be6634), proven PRE-EXISTING on both the pre-change and reconciled engines (probe battery behavior-identical with and without the new PRIM: axioms): a checked definition whose body invokes a type-DSL block opener - ': X ( -- ) PRODUCT ;' and SUMTYPE/ENUM/TYPEFAMILY variants - certifies rc 0 at empty declared stack, while deep-stack variants reject. Openers parse their own tokens and mutate the type registry; whether in-body use should EVER certify is a checker-semantics question (registry mutation from inside a certified word is a side effect the effect row ( -- ) does not express). Static invariant candidate: a checked body may not invoke a registry-mutating declaration opener; the checker should reject the token class in compile context (same mechanism family as UNSAFE-TOK?, which now covers LAYOUT-BUFFER exactly this way). Mechanism unidentified - reduce first: minimal fixture per opener, find which checker path admits the token, then either model the effect honestly or add the openers to the unsafe-token reject set with negative regressions. Owner: type-system lane (checker semantics). Evidence: crash-lane rebase report 2026-07-12; probes in .jj-ws/fable-crash internal-word-gate work.

---
title: "Checker: normalize nominal type case"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T01:38:06.417512+02:00\""
---

Static invariant: Forth dictionary/type names are case-insensitive, so DEFTYPE FOO must behave identically to deftype foo in signatures, typed locals, and generated >FOO/FOO>N casts. Current tools/check.f rejects uppercase nominal typed locals with E-UNKNOWN-SIGNATURE-TYPE, violating docs/forth.md uppercase project-word rule. Reproducer: /tmp/deftype-local-upper.f. Fix checker/verify-source nominal registry normalization at definition and lookup boundaries; add positive uppercase signature/local/cast regressions and mixed-case equivalence plus negative distinct-role regression. Verify exact tools/check.f fixture, owning checker tests, host/filemap lints. Dependency: owner persistence branch needs uppercase OWNER-ROW-IDX and PROT-ROW-IDX.

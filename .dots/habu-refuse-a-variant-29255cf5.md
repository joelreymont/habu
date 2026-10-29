---
title: Refuse a variant and family sharing a name in either load order
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T17:34:03.853362+02:00"
---

Problem: src/core/sumtype.f TDECL-VAR-TAKEN? (:296-301) filters variants by TDECL-VAR-SCOPE?, so a family declared after a package's variant of the same name is accepted, while the variant declared after the family is refused (TDECL-REQUIRE-VARIANT-NAME :323-330, type-family.f VARIANT-REQUIRE :2556-2560). The rule at sumtype.f:323-326 says a variant lives in no scope and collides with a family in every scope; validity depends on load order. Found in tim's stdio lane: IR-SCHEMA's ENUM side variant 'result' vs result.f's global family result<a,b> (renamed to 'output' there). Acceptance: both directions refuse with E-TDECL-NAME regardless of order; a rejected-program row for each order; census and rename of every collision the stricter check exposes in src, tools, test (and report client hits). Files: src/core/sumtype.f, src/core/type-family.f, checker tests. Verify: rejected rows, full suite, two-generation convergence (checker change). Depends: tim's stdio lane (the schema.f rename). Ownership: unassigned. Claim: unassigned.

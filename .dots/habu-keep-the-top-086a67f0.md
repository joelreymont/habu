---
title: Keep the top-row tracker from refusing a line
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T13:11:50.459339+03:00"
---

Problem: the tier-1 top-row tracker ends the process on an interpreted word, though docs/typed-top-level.md (Tier 1, warnings only) says tier 1 observes and never blocks. Chain: src/core/top-row.f TR-WORD -> TR-CERT-DOUT-EMPTY? -> EFFECT-QUERY -> FIND-SIG -> CHECKER-FIND-ACTIVE-SYM -> CHECKER-PKG-CONTEXT -> CHECKER-PKG-CONTEXT-REJECT (rc 70, 'no authenticated package context for this definition'). The query runs only when the tracked top has a known family, so whether a line refuses depends on tracker state. Measured on base 75b4 by lane PX (habu-throw-the-pkg-be01af7e): inside a package after '0 set-current', '1 2 drop drop' exits 70 while '5 .' passes; with '0 set-top-check' first, '1 2 2drop' gives rc 0. Related: habu-model-bare-wordlists-9e7c3521.
Acceptance: at tier 1 the tracker never changes rc or stops a line: a word whose effect cannot be queried in the current scope grays its outputs. Prefer a non-throwing checker probe for a package context that the tracker consults before EFFECT-QUERY, over changing EFFECT-QUERY, which the native compiler also calls (src/compiler/native/dict.f, checker-owner.f). Tier 2 (HABU_TOP_TIER=2) keeps refusing by its own diagnostic. Tests: the two measured lines at tier 1 (rc 0, no package-context diagnostic) and the tier-2 refusal, in test/top-row-warn.f.
Files: src/core/top-row.f, src/core/checker.f (the probe and its export), the seal/xref entries the export needs, test/top-row-warn.f.
Verify: the suite; chain and gate (baked).
Depends: none.
Ownership: krait.
Claim: unassigned.

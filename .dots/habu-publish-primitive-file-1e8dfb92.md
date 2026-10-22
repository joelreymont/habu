---
title: Publish primitive FILE dependency contracts
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T07:53:30.464134+03:00"
---

Combined6b+818 trailing-colon replay fixture is cold-correct but warm-misses because NUNIT-DEP:CAPTURE-BIND sees ordinal22 XREF 1+, a valid declaration key with CHECKER record CONTRACT$ known=false, and emits ORDERED before reference replay. This common built-in dependency can defeat otherwise safe FILE reuse across libraries. Identify the responsible checker primitive-contract publication boundary and supply portable semantic contract data using existing primitive/checker authority; preserve conservative ORDERED for genuinely unknown behavior. No special-casing only 1+, blanket known=true, name guesses or whole-history hashing. Inspect existing signature/primitive manifest contracts and real replay tests; prove exact first miss and actual warm hits/runtime output for representative built-in dependencies plus unknown-action fallback. Carl owns design, independent implementation/review and matching native checks. Dot is recorded before implementation; fixture may separately avoid unrelated 1+ while testing colon spelling.

Matching source-only recovery host c1c992d0 rejects the declaration-key E2E while loading ALTER-KEY-ARTIFACT, at its locals binding after SOURCE-ROOT:CANON-OS (exit 70). Determine whether this is a source-host declaration limitation or a real fixture defect before changing code; final acceptance requires the matching native product and actual key-mismatch fallback.

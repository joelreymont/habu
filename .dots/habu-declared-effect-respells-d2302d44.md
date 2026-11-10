---
title: declared_effect respells a quotation row and fresh names
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.448747+02:00"
---

Problem: the packet's declared_effect is not the signature as written for a quotation row `[ R -- S ]` (rendered `[ -- ]`) and for fresh names (rendered `fresh-region-a-a`). habu-render-the-declared-f4f40bb8's acceptance, declared_effect equals declared_effect_source modulo formatting, is unmet for those two. Found by its review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: for a signature with a quotation row and one with a fresh name, declared_effect equals declared_effect_source modulo formatting. Files: src/core/render.f (DECL-EFFECT and the record renderer), src/core/checker.f (SIG-EFF-CACHE!). Verify: two cases through tools/check.f --json-errors on a private engine, as rows of test/compiler/input-underflow-refusals.f that fail on the base. Depends: habu-render-the-declared-f4f40bb8 landed. Ownership: the declared-effect renderer. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-declared-effect-respells-d2302d44

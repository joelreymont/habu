---
title: "A top-level step's refusal record has an empty word and no position"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:08:07.052070+02:00"
---

Problem: the DIAG-JSON record for a refused top-level step (variable V / 1 V @ !, E-RAW-CELL-PTR) has "word":"" and no column or byte fields, so diag-contract fails (case top1.f in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases7/effect/cases). Found by the review of habu-a-packet-for-72692145. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the record passes the record check: a shape for a top-level step, or the fields filled. Files: src/core/render.f, tools/gate-json-assert-core.f. Verify: the case through diag-contract. Depends: habu-a-packet-for-72692145 landed. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-top-level-adb73a06

Hunt 2026-11-10 (report /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/toprec-hunt.md, cases /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/toprec): a top step has no definition, so NMA/NMU are empty (render.f DIAG-JSON) and TBASE/TBLEN are the token alone; DLOC-FIELDS writes only `line` because the run loads tools/check-core.f's marked copy, whose markers shift columns and bytes.

Shape settled 2026-11-10 (bob, by the long-term rule; told to Joel as a fact): a top-level step's record keeps the definition shape and says there is no definition, as `declared_effect` already does with null: `"word":null`, and `definition_source`, `token_index`, `declared_effect`, `declared_effect_source` and `inferred_effect` null with it. No new field, code or shape name; a reader that sees `word` null knows the token ran at top level. tools/gate-json-assert-core.f GJA-DIAG-COMMON requires those fields null together or none of them, as GJA-DECLARED-PAIR does for its pair. The place stays `line` alone for such a record: DECISIONS.md (the error-place words) has top-level errors print a column once tools/check.f's run file stops shifting columns, which is check-tool work that waits for incremental builds; no marker-offset map is added. Depends: the landing of habu-a-does-clause-d74786de and habu-refusal-rows-take-3a1d211f (same lines of DIAG-JSON).

tools/repair-packet-core.f RP-DEF-PACKET requires `word` a string (briefs8/reports/toprec-stop.md). Settled (bob, by the long-term rule): repair-packet refuses a record whose `word` is null with "repair-packet: a top-level step has no packet", exit 74, as it refuses a deferral and a warning: no definition exists to repair, and today it already refuses such a record for its missing column. GJA-REPAIR-DEF is unchanged, since no top-level record reaches it.

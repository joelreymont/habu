---
title: "Reuse indexed dictionary records and binding results"
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.274143+03:00"
blocks:
  - habu-give-a-word-297b990d
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own habu1.f WLFIND compiler record wrapper/checked declaration, native/dict.f WL-CANDIDATE/SPELL-REC and binding tests. WLFIND:LENTRY already returns record x12/start x11; consume it instead of indexed search-wl then full XREF rescan. Preserve public search-wl, private/TRUSTED-CELL visibility and retired-wordlist latest-record fallback. Verify precedence/ambiguity, collisions, hide/forget/redeclare and failed-evaluate slot reuse. Keep existing single EFFECT-QUERY; remove further duplicate consumers only if counted. Normal lookup avoids full dictionary scan; measure call counts and all-AOT contribution.375us/intrinsic is historical.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

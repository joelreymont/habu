---
title: "TFAM 10: native+Gforth MATCH/constructor lowering + bad-tag proof"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.948255+02:00"
---

PLAN.md item 10. Keyword data/labels/EMIT-KWDATA + lowering for MATCH/OF/ENDOF/ENDMATCH, token consumption, tag pushes, compare/branch chains, invalid-tag die (no normal continuation) in habu2.f AND bootstrap/cg/forth.fs; object/AOT test-entry support for preseeded bad-tag runs (entry identity = package/WID/record id + seeded cells + mode in every cache key/schema/index); bad-tag dies at runtime on native AND Gforth-recovered candidates; one-payload/wide/zero-payload + arbitrary third family; byte-identical fixpoint. Gate 17k. Depends: TFAM 9, 12.

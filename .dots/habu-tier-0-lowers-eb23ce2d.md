---
title: Tier 0 lowers a generic-row call at a wide type
title: Tier 1 refuses a rejected generic call in a TRUSTED: body
status: open
priority: 3
issue-type: task
created-at: "2026-10-24T03:12:59.643153+03:00"
---

Tier 1 refuses (-8519) a type-rejected call to a word whose row holds a naked type variable inside a TRUSTED: body; tier 0 runs the physical cell code. Width-independent: `: GN ( a bool -- bool a ) swap ; TRUSTED: GX ( -- n n ) 1 2 GN ;` prints `1 2` at tier 0, -8519 at tier 1 (master, 2026-10-24). Site: src/compiler/native/elaborate.f DO-WORD-CALL (~3358/3369): the rejected unify records no CW-CALL row (checker.f CWIN-CELLS/CWIN-GLUE ~14072) and SELECTED-CALL-ABI declines non-fixed rows. A fixed-row twin (`: F2 ( n n -- n n ) swap ;`) runs identically at both tiers. Acceptance: record the unjudged splice's physical call row (in/out cells, glue) as a CW-CALL row for a spliced rejected call, so tier 1 runs GN and IC-RAW as tier 0 does; a test/tier.f row for GN at both tiers.

---
title: Find whether any compile writes a row naming its own definition
status: open
priority: 3
issue-type: task
created-at: "2026-11-07T05:23:35.112825+02:00"
---

F1 fold-in (2026-11-07): RESOLVE's LINK-SELF branch (src/habu/linker.f RESOLVE, tag 0 whose record is the entry being linked) and FRESH?'s bound top EM-RECS @ 1 + in CALL-CK and ADDR-CK admit a call or address row naming the definition's own record (SLOT-CK resolves nothing: a slot's target must be tag 5, none), but no fixture writes one: T0 compiles recurse as an in-place bl to the emission's start (src/habu/t0-arm64.f SELF+, placement :410-413) with no row, and breaking both bounds still passes file-objects. Find whether any compile path a build runs (quotations, does> clauses, tier 1) writes such a row; if one does, add it to a fixture so A2 covers it; if none does, drop LINK-SELF and tighten the bound in the same commit. Not on the incremental-build path. Claim: unassigned.

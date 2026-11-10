---
title: Find whether any compile writes a row naming its own definition
status: closed
priority: 3
issue-type: task
created-at: "2026-11-07T05:23:35.112825+02:00"
closed-at: "2026-11-10T10:58:41.852983+02:00"
close-reason: "landed; no compile writes a call or address row naming its own record (planted dies never reached in an engine build cold and warm, test/file-objects.f and a program of self-referring forms; a review traced tier 0 and tier 1: recurse files no row, a body naming its own word is E-UNDEFINED); RESOLVE's LINK-SELF branch is removed and CALL-CK and ADDR-CK refuse such a row as stale; engine built by the installed hb = the engine it builds. Wasm's CALL-SELF must not file its own record as a call target: such a file would never link."
---

F1 fold-in (2026-11-07): RESOLVE's LINK-SELF branch (src/habu/linker.f RESOLVE, tag 0 whose record is the entry being linked) and FRESH?'s bound top EM-RECS @ 1 + in CALL-CK and ADDR-CK admit a call or address row naming the definition's own record (SLOT-CK resolves nothing: a slot's target must be tag 5, none), but no fixture writes one: T0 compiles recurse as an in-place bl to the emission's start (src/habu/t0-arm64.f SELF+, placement :410-413) with no row, and breaking both bounds still passes file-objects. Find whether any compile path a build runs (quotations, does> clauses, tier 1) writes such a row; if one does, add it to a fixture so A2 covers it; if none does, drop LINK-SELF and tighten the bound in the same commit. Not on the incremental-build path. Claim: unassigned.

Claim: agent=jerry workspace=.jj-ws.noindex/jerry-self, 2026-11-10: a trap in the branch over engine and program builds, then the branch removed if nothing reaches it.

---
title: Verify trusted-inventory-test on master
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T16:45:49.799504+02:00"
---

DETERMINATION ALREADY MADE. This dot records a completed measurement and is ready to close as a lane artifact; no fix is owed. It is recorded as a dot so the finding and its standing consequence are not lost.

The vecmem lane reported tools/trusted-inventory-test.f red at lane base a3785217, with exit status 1, a TFAIL at assert 20 and four failures. Measured on master ed3465d3: running the suite through its documented owning path, an empty stdin piped into bin/hb --load tools/trusted-inventory-test.f, exits 0 and prints "test: ok" with empty stderr. The suite is green on master.

Cause of the discrepancy: a3785217 is an ancestor of master, and tools/trusted-inventory.f, tools/trusted-inventory-test.f and TRUSTED.md are byte-identical between a3785217 and master, so nothing about the suite or its data was repaired in between. The suite is a whole-repo scanner. Requiring the tool executes its CLI main, which scans the live tree and prints the report, and the suite then asserts TINV:RATCHET-BAD# equal to zero and TINV:RATCHET-STALE# equal to zero against the derived ratchet ceiling in the TRUSTED.md classification block. The lane's own in-progress edits changed the live inventory of trusted sites without the matching update to that classification block, which is exactly the shape that produces a small cluster of ratchet failures. The red was the lane's tree state, not a defect on master.

Standing consequence worth keeping: this suite is tree-sensitive by design and is fail-closed in both directions, so any lane that adds or retires an unchecked or trusted site must update the TRUSTED.md classification block in the same change. A red here inside a working lane should first be read as that lane's own uncommitted inventory drift, before anyone treats it as a master regression.

Acceptance: satisfied by the determination recorded above. Close as artifact. Closure is an orchestrator action; this worker does not close dots.

---
title: Make gate-engine child failures loud (256-throw hazard)
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T07:35:37.060895+02:00"
---

Found during the Xid-31 RCA landing (2026-07-21): when the census ratchet (GE-CENSUS-RATCHET in test/gate-engine-lib.f) failed on a stale STATUS.md row, the gate-engine build child exited rc 0 with ZERO output - the documented BTHROW hazard (a throw code that is a multiple of 256 exits silently masked to 8 bits). tools/build-fixpoint.f hardened ITS chain with the BF-CLI catch-all for exactly this reason (see its comment block ~:1560), but the gate-engine entry (test/gate-engine.f 'GE-MAIN catch ... GE-THROW-REPORT') still let a code slip through silently - the failing gate showed 'rc: 0, stdout 0 bytes, (unmapped)' and cost a multi-step diagnosis. Fix: make every gate-engine child death loud - audit GE-THROW-REPORT and the die paths reachable from GE-BUILD-FIXPOINT (census ratchet, size ratchets, shape checks) so any escaped throw prints its code and exits nonzero-and-not-256-aligned (mirror BF-FAIL-DIE). Red-first: reproduce the silent exit with a synthetic 256-aligned throw on the unfixed base, prove the hardened path prints and exits nonzero.

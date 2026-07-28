---
title: Publish typed byte advance in CAD-NUM
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T16:46:35.609062+02:00"
---

Why: readers that prove bounds in CAD-NUM's checked algebra still each need a private trusted projection of CAD-NUM:byte-off to a raw number solely to advance a pointer, adding a new audited trust boundary per consumer (SAFET's reader was about to add one). CAD-NUM:byte-off is a nominal role carrying no relation to any particular pointer or extent, so no checker obligation can be discharged from the signature alone; the correct small capability is a single public advance owned by the algebra itself. Exact result: public CAD-NUM:BYTE+ ( ptr u8 CAD-NUM:byte-off -- ptr u8 ) in lib/cad-num-arithmetic.f, implemented through CAD-NUM's existing private BYTE-OFF>N projection. It preserves the offset role at the public boundary; it does not prove bounds - callers must prove bounds first in the checked algebra. Owner: package CAD-NUM. Dependencies: none. Forbidden: claiming any bounds proof; new trusted rows; touching WSTORE, STR, or BUF projections (different words for different purposes, not this sink); raw compatibility APIs. Acceptance: arithmetic source and focused test only, plus manifest registration; exact negatives prove a raw n and the wrong role CAD-NUM:byte-len are rejected by the checker; consumers can then advance without a private trusted projection. Smallest owning-path check: before the change, a checked consumer advancing by a byte-off cannot compile without minting its own trusted projection.

Claim: agent=claude workspace=.jj-ws/habu-add-bounded-little-189c4aa9

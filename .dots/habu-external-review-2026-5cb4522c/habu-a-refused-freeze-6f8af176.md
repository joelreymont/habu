---
title: A refused freeze can be retried onto half-published edges
status: open
priority: 3
issue-type: task
created-at: "2026-08-20T21:09:34.431546+02:00"
---

Found by lane ir-1 while making IR row appends transactional (habu-a-mid-row-e02603c6). IR-BUILD:FREEZE (src/compiler/ir/build.f) runs VERIFY-CK, which ends in IR-VERIFY:EDGES-PUBLISH, and only then TABLES-FREEZE. A caught freeze leaves the builder LIVE (test/compiler/ir-verify.f REFUSE-CASE pins exactly that), so a caller may freeze again. IR-VERIFY:ROOM-CK compares each edge arena's CEILING against the counts a single run writes - HC-CAP vs BLOCKS, HC-CAP vs EDGE-CELLS - not against the room still left, so a second run would pass its capacity check and append a second full set of edges on top of whatever the first left. The reservation this lane added makes a scratch-refused run write nothing, which closes the case that is reachable today; what is unresolved is whether FREEZE should be retryable at all. Decide: make a refused freeze consume the builder (ABORT is already the documented answer for most refusals), or make ROOM-CK count remaining room so a retry refuses named. NOT observable today: the edge arenas are reachable only through IR-BUILD:FEDGE-POOL / FEDGE-ROWS, which need a FROZEN module, so a freeze that threw leaves no reader - which is also why this lane could write no red-on-master fixture for verify.f. Files: src/compiler/ir/build.f, src/compiler/ir/verify.f, test/compiler/ir-verify.f. Depends: none.

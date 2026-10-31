---
title: Plan slot literal rows in SEGMENT-ROOM?
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.872359+01:00"
closed-at: "2026-10-31T20:52:55.802571+02:00"
close-reason: "Obsolete: SEGMENT-ROOM? and EARLIER-SLOTS! 0 hits; literals plan through POOL-PLAN."
---

tools/native-unit-object.f SEGMENT-ROOM? plans pool room only for a segment's payload literals, not its own slot literal rows; EARLIER-SLOTS! interns literals after trailing loads without a pool-room check. Risk: a hit runs out of literal-pool room mid-install, so a hit throws. Found in the int-merge lane (L2a).

---
title: Plan slot literal rows in SEGMENT-ROOM?
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.872359+01:00"
---

tools/native-unit-object.f SEGMENT-ROOM? plans pool room only for a segment's payload literals, not its own slot literal rows; EARLIER-SLOTS! interns literals after trailing loads without a pool-room check. Risk: a hit runs out of literal-pool room mid-install, so a hit throws. Found in the int-merge lane (L2a).

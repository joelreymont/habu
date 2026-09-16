---
title: Validate both captured registry payloads during merge
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-14T12:14:20.570379+03:00\""
closed-at: "2026-09-16T14:34:47.986178+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Registry payload merge must read and bound both stored tables rather than use byte lengths as a compatibility decision; residue is symmetric prefix-only admission."
---

Owner cedar. b9745c57 reserializes the live checker over the captured host and uses byte lengths as a compatibility decision. A later empty window makes a real captured delta appear empty. Immediate repair reads and bounds the stored host table; equal prefix-only payloads must succeed. Complete symmetric admission of a prefix-only incoming payload with a host delta, retaining the delta and comparing canonical prefix identity; reject two conflicting actual deltas. Test captured bytes independently of active checker ownership.

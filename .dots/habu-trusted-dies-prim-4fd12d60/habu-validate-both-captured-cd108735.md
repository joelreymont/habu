---
title: Validate both captured registry payloads during merge
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T12:14:20.570379+03:00"
---

Owner cedar. b9745c57 reserializes the live checker over the captured host and uses byte lengths as a compatibility decision. A later empty window makes a real captured delta appear empty. Immediate repair reads and bounds the stored host table; equal prefix-only payloads must succeed. Complete symmetric admission of a prefix-only incoming payload with a host delta, retaining the delta and comparing canonical prefix identity; reject two conflicting actual deltas. Test captured bytes independently of active checker ownership.

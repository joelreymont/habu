---
title: Bound AOT donor-map lookup
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T01:35:56.736645+03:00"
---

Ordinary Dots stripped build on the private incremental candidate times out after 559 CPU seconds. The sample and live debugger identify IMAGE-NAMES:DONOR-SOURCE$, PACKAGE$ and FIELD$: each member rescans every donor row, including a full namespace search even when the span differs. Index code spans and namespace rows once; preserve exact span/name/package matching and alias preference. Verify the unchanged Dots build, runnable stripped output, existing stripped-map/cache E2E and required gates. ACAP-NEXT-CELL was an unconfirmed hypothesis, not the sampled owner.

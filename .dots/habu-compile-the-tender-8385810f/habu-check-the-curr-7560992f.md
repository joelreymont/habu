---
title: Check the current provenance and partial-payload layout boundaries
status: active
priority: 2
issue-type: task
created-at: "2026-09-14T00:36:41.710478+03:00"
---

Owner Cedar. F full gate: protection-span wrongly accepts a write crossing the protected provenance table end; aot-section-reach expects the old 28-label source before partial payload restored three conditional graph labels. Preserve seal rejection and valid adjacent stores, add explicit provenance start/end cases, and require all 31 actual section labels. Scope test/protection-span.f and tools/aot-section-reach-lint-test.f; no runtime policy change. Verify actual native focused loads, independent review, then combined full gate.

Both actual F focused loads pass: protection-span in 0.765 seconds and
aot-section-reach in 0.618 seconds. Logs: /tmp/cedar-F-protection-span-current.log
and /tmp/cedar-F-aot-section-reach-current.log. Independent review and the next
combined gate remain pending.

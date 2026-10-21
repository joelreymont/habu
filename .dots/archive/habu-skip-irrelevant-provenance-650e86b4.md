---
title: Skip irrelevant provenance range math
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T12:38:40.784775+03:00"
closed-at: "2026-10-21T12:45:27.050734+03:00"
close-reason: "Rejected and fully reverted: matching candidate tier/protection E2Es pass, but alternating500M-store medians show DATA0.92221s to0.92150s and mapped2.12978s to2.12235s, within drift. Mapped-path instructions fall22 percent while cycles stay flat; helper576 to572 bytes changes no product size. Normal macOS DATA lies above the provenance map, falsifying the expected common-path shortcut. No production or trial-specific test change retained."
---

Incremental build profiles put protected stores at16–22 percent of samples. ARM64 GUARD-SPAN currently computes the allocated provenance-table end before testing whether a write lies wholly below its base. After existing zero-length and unsigned-wrap checks, compare the checked end against the loaded map base first; only potentially overlapping writes need capacity/end arithmetic. Preserve the exact protected set and all rejection behavior, dynamic map allocation/capacity coverage and helper ABI. Select existing protection/provenance tests before implementation; measure a real store loop and warm build, keep only an actual gain. Do not repeat the reverted paired-store inlining experiment or alter protection policy.

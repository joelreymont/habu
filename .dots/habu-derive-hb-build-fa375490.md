---
title: Derive hb-build tool key list from event closure
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T09:25:29.189684+02:00"
---

Referenced as 'tracked by habu-tfam-5-add-7730ca3e (hb-build-lib.f key list)' in closed sub-dot habu-tfam-5-event-d7618516, but that dot was never minted - this one replaces it. tools/hb-build-lib.f HBB-KEY-LOAD-FILES (:430-455) is a hand-maintained list of the hb-build tool's own load files folded into the artifact cache key; user-source closure keys already come from the event-closure producer (HBB-CLOSURE-CK+ via EC:BUILD), but the tool's own closure is still hand-listed, so a new transitive require in any listed file's closure can silently miss the key and produce stale-cache reuse. Task: derive the tool key list from EC:BUILD over the hb-build entry files (or, if load-order constraints prevent that, add a parity regression asserting HBB-KEY-LOAD-FILES equals the producer closure so drift fails loudly). Owning tests: tools/hb-build-test.f.

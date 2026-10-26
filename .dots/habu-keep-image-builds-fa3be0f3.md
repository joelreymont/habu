---
title: "Run a fresh keyed image once before a deadline covers it"
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T22:07:23.215165+02:00"
---

Problem: a consumer's deadline covers the first exec of a keyed image that was just published. The build itself has its own budget (tools/check-verify-core.f VFY-BUILD-MS 600000), but the first exec of a freshly written ad-hoc binary waits for macOS AMFI (amfid), which on a loaded host answered 36.6 s late (tim, 2026-10-26, ~/.cache/tmp/tim/arena/hang/loop-master/stuck.txt). Measured: check.f --source-list with a private cold HABU_BUILD_CACHE ends rc 69 "the verifier did not complete: deadline of 120000 ms passed" (467 s run); the same command on the same image next takes 1 s (/Users/joel/.cache/tmp/carl-keyt.Njcj/rc). tools/check-verify-test-lib.f GUARD-MS (60000, :235), documented as a deadlock guard, is reached the same way: master tkprrovt 9987b871 cold, two-in-turn 'the first' verdict 4 after 99.7 s (/Users/joel/.cache/tmp/carl-cvcmp.h3SD/master-cold.log); under load the row fails a different image case per run (image-stale 2622/2623, image-saver-leak 2634/2635, two-in-turn; carl-gate1.ng71, carl-red1.Op0U). "Key the verifier image on its canonical entry" removed the per-fixture-root rebuilds that multiplied this cost (one image per linked root before). Acceptance: no consumer deadline covers a keyed image's first exec (for example tools/keyed-image.f runs the image once before PUBLISH); check.f with a cold private cache and tools/check-verify-test.f cold and warm pass under load; the image cases still prove staleness and saver-leak detection. Verify: those runs with a private HABU_BUILD_CACHE.

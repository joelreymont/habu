---
title: "Keep image builds outside check-verify's deadlock guard"
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T22:07:23.215165+02:00"
---

Problem: tools/check-verify-test-lib.f GUARD-MS (60000, :235) is documented as a deadlock guard never reached by a slow check, but cases whose check builds the verifier image run under it. Measured 2026-10-26 on master tkprrovt 9987b871 with a private HABU_BUILD_CACHE (cold): two-in-turn 'the first' got verdict 4 (incomplete) after 99.7 s, row rc 1 (/Users/joel/.cache/tmp/carl-cvcmp.h3SD/master-cold.log). On a loaded host (load 8-48) the same row fails a different case per run, all image-building ones: image-stale 2622/2623, image-saver-leak 2634/2635, two-in-turn (carl-gate1.ng71, carl-red1.Op0U). Acceptance: no check-verify case puts an image build inside a deadlock guard (build the image in the row's setup, or guard only the check after the image exists); the row passes with a cold private cache under load; the image cases still prove staleness and saver-leak detection. Verify: tools/check-verify-test.f cold and warm with a private HABU_BUILD_CACHE.

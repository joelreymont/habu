---
title: Charge foreign profile samples to the Habu word that called out
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T07:32:36.341839+02:00"
---

Problem: EMIT-PROF (src/habu/prof.f:621) counts a sample whose x20 or x26 is not Habu's as foreign with no lookup or walk (:653-655), so time inside C a Habu word calls is credited to no word: 544 of 564 samples of a loop of FS:ATOMIC-WRITE-FILE were foreign. Acceptance: a foreign sample on the main stack is charged to the nearest Habu word on that stack, with its callers walked as for any sample; only samples off the main stack stay foreign; the report says how many it charged this way. Depends: habu-walk-a-profile-d56f7c32. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-prof-c.

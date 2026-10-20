---
title: Locate regenerated application DATA drift
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T22:08:21.377275+03:00"
closed-at: "2026-10-20T22:36:54.342000+03:00"
close-reason: Root cause was redundant live builder XT addresses in sparse DATA. Encoder masks exact declared XT byte intervals in both passes without mutating source cells; unaligned neighbors and DATA-kind cells remain intact. Fresh-cache, object-relink and warm Dots binaries/maps are byte-identical; callback and stripped-image E2Es pass. Independent Astra review clear.
---

Same-source Dots stripped output regenerated with fresh caches differs at three four-byte ranges in __DATA_CONST plus code signatures. Same existing-object relink is exact; PROC-MAPS optimization did not create this, because unchanged original3bd freshcache reproduces the same ranges. Baseline/rootleaf ranges171683..171686,172400..172403,172415..172418 (one-based), mapsidenticalexceptimageSHA, runtimehelp/versionequal. Locate actual decodedDATA owners and source cause using retained paired artifacts; fix responsible lifetime/canonicalization onlyafter provenreduction. Do not normalize arbitrarybytes or bless equality from runtime smoke. Evidence /Users/joel/.cache/tmp/carl-app-profile.m0ynql/BOUNDS.md.

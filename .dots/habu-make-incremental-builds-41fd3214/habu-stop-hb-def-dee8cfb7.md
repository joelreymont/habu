---
title: Stop hb-def entry churn in the shared build cache
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T23:46:11.012121+02:00"
---

kevin 2026-10-26: ~/.cache/habu-build holds 3,874,738 entries (130 GB); 3,840,510 are hb-def-<key>.hbd (tools/native-definition-cache.f ENTRY-PATH). 3,768,014 were last used within 24 h, so the 24 h prune (src/lib/build-cache.f) removed only ~20k: about 200k new or re-dated keys a day. Every hb-build publish prunes the family (tools/hb-build-lib.f HBB-PRUNE-ENTRIES), scanning the whole directory. Acceptance: root cause named with a reproducer; an unchanged rebuild adds no hb-def entry; the shared cache is cleaned once the cause is fixed. Lane: carl, root-cause worker (no workspace, scratch under ~/.cache/tmp/carl-hbdef.*).

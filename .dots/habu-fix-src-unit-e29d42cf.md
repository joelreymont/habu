---
title: Fix source-unit-range-e2e F68 on master
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T15:38:33.905810+02:00"
---

TMPDIR=/tmp/jerry-sur hb --load test/source-unit-range-e2e.f on master oxtpmytk with the public hb: two 'hb: interpret stack underdepth: drop' lines, then F68 'assert: expected true got false', test: 1 failure(s). Same failure at the T0 line tip pwxnuopw on both S1 engines, so it predates the T0 line. Root cause not yet investigated.

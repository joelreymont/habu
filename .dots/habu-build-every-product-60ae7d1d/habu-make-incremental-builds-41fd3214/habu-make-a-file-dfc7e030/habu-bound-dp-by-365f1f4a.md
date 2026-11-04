---
title: Bound DP by two header cells
status: active
priority: 1
issue-type: task
created-at: "2026-11-04T13:16:01.131591+02:00"
---

Cross-build step 2 (design /tmp/jerry-big/design-xbuild-r2.md 3.2). DP-CHECK reads DP-FLOOR-CELL and DP-CEILING-CELL (names Joel's, 2026-11-04) instead of baked constants; every arm64 boot writes today's values; no behaviour change. x86-64's twin moves at step 8. Brief /tmp/jerry-big/dpcell-brief.md. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-dpcell (worker running).

---
title: Give xt! a quotation-kinded type-variable row
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:50:03.253538+03:00"
---

Found by lane 537 (dot 8a8912cc; owner 1610f30c superseded with no child): src/core/checker.f (~:2876) keeps the XT-DECL token window that exempts xt! from FENCE-EXEC, because no type-variable kind among the TVK kinds (~:336-372) admits only quotations. Acceptance: a TVK kind admitting only quotations/xts; xt!'s row demands it; the XT-DECL window is deleted; through the real load path a non-xt passed to xt! is refused and a declared xt admitted (both seen failing first on the current engine); the comment names no missing dot; baked: rebuild, g1 == g2, two-generation build.

---
title: Make die end the process when the exit hook throws
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T03:27:44.799107+03:00"
---

die returns when the exit hook throws under a live catch (review of the R-EXIT class). Engine fix on both kernels, commit wopkoktr before pwmronzn in .jj-ws.noindex/carl-die.

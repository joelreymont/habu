---
title: "t0cap: give T0's byte buffers back before a capture"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:27.424687+02:00"
---

Problem: a saved image crashed (rc 139) on its first tier-0 compile when T0 compiled before the save: src/habu/t0-arm64.f RAW/OUT/EPI kept LIVE 1 over storage the registry releases before the DATA copy. Acceptance: persistent IMAGE-LIFECYCLE hook disposes them and clears LIVE; /tmp/rev-r2a/p/save0.f + run.f prints 5 and 42 rc 0; hb-build-repl-image rc 0. Files: src/habu/t0-arm64.f. Brief /tmp/jerry-big/t0-cap-brief.md. Commit mnzvrsym on R2a zxwoonrk. Depends: R2a. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0cap

---
title: Make zzusage, blackboard and kiba load on current Habu
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T19:07:22.711208+03:00"
---

Their sources require lib/... but Habu moved libraries to src/lib/ in 6308d497213. One worker per repo in that repo's .jj-ws.noindex/jerry-load workspace; lead reviews and pushes each default branch. kiba converted to colocated jj 2026-10-23 (origin still https: the remote switch to SSH needs Joel's permission). zzusage local master carries 17 unpushed 2026-10-13 commits that ride along.

Progress (2026-10-23): zzusage master 52d7726f and blackboard main cdd8a7bf pushed (zzusage 142/142 differential; blackboard 39/39 regressions, 135/135 differential; blackboard's compiler hash pin dropped on Joel's ruling, both build from ~/.local/lib/habu/current). kiba change rnvpltkt reviewed and ready in ~/Work/kiba/.jj-ws.noindex/jerry-load (src/lib requires; its STR< deleted for the identical library word): waiting on origin moving to SSH. kiba's test.sh fails 7 assertions on macOS only (fake curl uses /usr/bin/cat and GNU stat; kiba targets Omarchy Linux). Out-of-scope kiba findings: its -9900..-9999 error block lies outside the -999999..-100000 range src/lib/errors.f reserves for applications; AGENTS.md cites RUN-INHERIT, which no longer exists (sw-run.f has EXEC-STAGED/RUN-STAGED).

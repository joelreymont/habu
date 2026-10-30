---
title: "ctrl: refuse malformed control flow at the native tiers as the JIT does"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T19:22:21.333659+02:00"
---

Gap A: unchecked/TRUSTED malformed control flow ends uncaught (-9601/-8502/-8575 rc 67); SETTLE-UNIT ends a control fault as a refusal (ncomp line with token, rc 70), as for an undefined word; JIT texts not reproduced (first worker found they need a new bootstrap file and hook-report suppression). Brief /tmp/jerry-big/t0-ctrl2-brief.md (first: t0-ctrl-brief.md). Tier-1 -8575 split to habu-report-a-tier-aeefcaf4. Base mktzwnmu. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ctrl

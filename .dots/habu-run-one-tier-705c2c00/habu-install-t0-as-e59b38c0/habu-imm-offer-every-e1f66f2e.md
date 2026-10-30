---
title: "imm: offer every body immediate to the checker's preflight"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:56:34.545080+02:00"
---

Gap (top-row-hook F101/F103, load-reject-diag F160/F163): the native capture skips the armed hook's preflight for non-neutral immediates; T0/tier 1 die 76 lowering-certificate mismatch or uncaught -9603/-8286 where the JIT gives 70. Also unify T0's E-T0-IMMEDIATE with tier 1's code, drop the x86-64 branch, fix DEF-TIER-0 comment. Brief /tmp/jerry-big/t0-imm-brief.md. Base mktzwnmu. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-imm

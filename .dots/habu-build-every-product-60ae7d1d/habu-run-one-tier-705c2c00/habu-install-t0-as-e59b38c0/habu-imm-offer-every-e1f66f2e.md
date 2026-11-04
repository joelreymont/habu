---
title: "imm: offer every body immediate to the checker's preflight"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:56:34.545080+02:00"
closed-at: "2026-11-02T11:33:14.866144+02:00"
close-reason: "Landed 7d55e756b2 'Preflight every immediate a capture holds': E-T0-IMMEDIATE is gone, TEST-IMM-TRUSTED has no x86-64 branch, the DEF-TIER-0 comment is corrected."
---

Gap (top-row-hook F101/F103, load-reject-diag F160/F163): the native capture skips the armed hook's preflight for non-neutral immediates; T0/tier 1 die 76 lowering-certificate mismatch or uncaught -9603/-8286 where the JIT gives 70. Also unify T0's E-T0-IMMEDIATE with tier 1's code, drop the x86-64 branch, fix DEF-TIER-0 comment. Brief /tmp/jerry-big/t0-imm-brief.md. Base mktzwnmu. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-imm

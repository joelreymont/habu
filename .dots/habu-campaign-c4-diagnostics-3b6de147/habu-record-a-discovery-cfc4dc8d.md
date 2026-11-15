---
title: Record a discovery rejection where discovery meets it
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:24:18.259773+01:00"
closed-at: "2026-11-16T04:07:34.801220+01:00"
---

Problem: discovery's and the quiet composition's E-DISC rejections throw bare codes, and check.f builds an old packet from the code (tools/check-verify-core.f CHK-FAULT$). Acceptance: one placed record per rejection from the site that meets it, rc 70, under check.f and --verify-only; the LSP still blocks the file and prints nothing. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

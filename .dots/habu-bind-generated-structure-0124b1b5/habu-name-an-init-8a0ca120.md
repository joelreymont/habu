---
title: Name an init accessor through TDECL-ADDR-NAME
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T19:23:25.949469+01:00"
closed-at: "2026-11-14T20:44:51.406432+01:00"
---

Problem: src/core/sumtype.f TDINIT-NAME repeats TDECL-ADDR-NAME's body (TDGEN-CLEAR, TDGEN-DRV-REF, the buffer) with the member name TFAM-INIT-MEMBER$ answers. Acceptance: TDINIT-NAME is fam fid store TFAM-INIT-MEMBER$ TDECL-ADDR-NAME, or its callers call that directly and it goes; type-decl passes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-chain.

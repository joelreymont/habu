---
title: "Stop a sealed package's duplicate at the seal"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.901641+01:00"
closed-at: "2026-11-16T04:07:33.977256+01:00"
---

Problem: diag-position assert 4772: a duplicate variable into a sealed package exits 70, docs/forth.md:194 says 84. Acceptance: the case prints one E-SEALED-PACKAGE record and exits 84 in the loader and check.f. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

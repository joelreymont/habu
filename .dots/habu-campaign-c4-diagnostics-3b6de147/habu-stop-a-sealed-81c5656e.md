---
title: "Stop a sealed package's duplicate at the seal"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.901641+01:00"
---

Problem: diag-position assert 4772: a duplicate variable into a sealed package exits 70, docs/forth.md:194 says 84. Acceptance: the case prints one E-SEALED-PACKAGE record and exits 84 in the loader and check.f. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-sealdup.

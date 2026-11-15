---
title: "Merge master's latest into C4"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:54:40.620802+01:00"
closed-at: "2026-11-16T04:07:35.426955+01:00"
---

Problem: master moved past C4's merge ytspyxyk (base abe6b6ca788c) to d64ef8a1 (Retire PRODUCT, the pre-hook constant workarounds deleted, engine-error-effects.f deleted); the second merge yyoxwplr (pkoupzly + master@origin) has conflicts in 15 code and test files. Acceptance: yyoxwplr holds no conflict, keeps both sides' behaviour, builds and converges, and the suite passes on it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c4m2 (brief /tmp/jerry-c4/w63-merge2.md).

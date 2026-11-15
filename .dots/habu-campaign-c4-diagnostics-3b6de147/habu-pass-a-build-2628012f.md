---
title: "Pass a build's rejection status on"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:11.026001+01:00"
closed-at: "2026-11-16T04:07:35.233590+01:00"
---

Problem: hb-build turns a rejection's 70 into 74 (test/wasm/hb-build.f pins 74). Acceptance: a build stopped by a rejection exits 70 with its one record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
